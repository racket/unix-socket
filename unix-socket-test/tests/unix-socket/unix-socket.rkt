#lang racket
(require racket/port
         rackunit
         racket/unix-socket
         (only-in racket/private/unix-socket-ffi platform))

(define (call-in-custodian proc)
  (parameterize ((current-subprocess-custodian-mode 'kill))
    (parameterize ((current-custodian (make-custodian)))
      (call-with-continuation-barrier
       (lambda ()
         (dynamic-wind void
                       proc
                       (lambda ()
                         (custodian-shutdown-all (current-custodian)))))))))

;; Commands for creating socket listeners
;;  - netcat is commonly available, but cannot use Linux abstract namespace
;;  - socat can use Linux abstract namespace, but is less common
;; So use netcat for path test and socat for abstract-name test.

(define netcat
  (for/first ([netcat '("/bin/nc" "/usr/bin/nc")]
              #:when (and (file-exists? netcat)
                          (memq 'execute (file-or-directory-permissions netcat))))
    netcat))

(define socat
  (for/first ([socat '("/usr/bin/socat")]
              #:when (and (file-exists? socat)
                          (memq 'execute (file-or-directory-permissions socat))))
    socat))

(define-check (check-comm msg out in)
  (write-bytes msg out)
  (flush-output out)
  (check-equal? (sync/timeout 1 (read-bytes-evt (bytes-length msg) in))
                msg))

(define (close-ports . ports)
  (for ([port ports])
    (cond [(input-port? port) (close-input-port port)]
          [(output-port? port) (close-output-port port)])))

(define (make-temp-file-name)
  (define tmp ((values make-temporary-file)))
  (delete-file tmp)
  tmp)

(unless unix-socket-available?
  (error "cannot test unix sockets; not supported"))

;; ============================================================
;; connect tests

;; Test path-based socket
(test-case "unix socket : connect w/ netcat"
  (unless netcat
    (printf "skipping connect w/ netcat; netcat not found\n"))
  (when netcat
    ;; Uses netcat to create a simple unix domain socket server
    (define tmp (make-temp-file-name))
    (call-in-custodian
     (lambda ()
       (define-values (ncprocess ncout ncin ncerr)
         (subprocess #f #f #f netcat "-Ul" (path->string tmp)))
       (sleep 0.5)
       (define-values (from-sock to-sock)
         (unix-socket-connect tmp))
       (check-comm #"hello" to-sock ncout)
       (check-comm #"charmed" ncin from-sock)
       (check-comm #"well\ngoodbye, then" to-sock ncout)
       (close-ports to-sock from-sock)
       (close-ports ncin ncout ncerr)
       (or (sync/timeout 1 ncprocess)
           (subprocess-kill ncprocess))
       ))
    (when (file-exists? tmp) (delete-file tmp))))

;; Test Linux abstract name socket
(test-case "unix socket w/ socat, abstract namespace"
  (unless socat
    (printf "skipping connect w/ socat, abstract namespace; socat not found"))
  (when (and socat (eq? platform 'linux))
    ;; Uses socat to create a simple unix domain socket server
    (call-in-custodian
     (lambda ()
       (define name #"TestRacketABC")
       (define-values (ncprocess ncout ncin ncerr)
         (subprocess #f #f #f socat (format "ABSTRACT-LISTEN:~a" name) "STDIO"))
       (sleep 0.5)
       (define-values (from-sock to-sock)
         (unix-socket-connect (bytes-append #"\0" name)))
       (check-comm #"hello" to-sock ncout)
       (check-comm #"charmed" ncin from-sock)
       (check-comm #"well\ngoodbye, then" to-sock ncout)
       (close-ports to-sock from-sock)
       (close-ports ncin ncout ncerr)
       (or (sync/timeout 1 ncprocess)
           (subprocess-kill ncprocess))
       (void)
       ))))

;; ============================================================
;; combined connect and listen/accept tests

(define (combined-test sockaddr)
  (test-case (format "unix socket: listen/connect/accept at ~e" sockaddr)
    (call-in-custodian
     (lambda ()
       (define l (unix-socket-listen sockaddr))
       (check-eq? (sync/timeout 0.1 l) #f)
       (define-values (cin cout) (unix-socket-connect sockaddr))
       (check-eq? (sync/timeout 0.1 l) l)
       (define-values (ain aout) (unix-socket-accept l))
       (check-eq? (sync/timeout 0.1 l) #f)
       (check-comm #"hello" cout ain)
       (check-comm #"wow you sound a lot closer now" aout cin)
       (check-comm #"that's because\nwe're in\nthe same process!" cout ain)
       (check-comm #"ttfn" aout cin)
       (close-ports cout aout)
       ;; FIXME: input ports block, rather than eof, after output ports closed
       ;; (check-eq? (read-byte cin) eof)
       ;; (check-eq? (read-byte ain) eof)
       (close-ports cin ain)
       (when (and (path? sockaddr) (file-exists? sockaddr))
         (delete-file sockaddr))))))

;; Test path-based socket
(combined-test (let ([tmp ((values make-temporary-file))]) (delete-file tmp) tmp))

;; Test Linux abstract name socket
(when (eq? platform 'linux)
  (combined-test #"\0TestRacketDEF"))
