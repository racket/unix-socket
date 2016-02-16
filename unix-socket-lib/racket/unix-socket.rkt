#lang racket/base

;; Support for connecting to UNIX domain sockets.

#|
References:
linux (64):
  Linux Standard Base Core Specification 4.1
macosx (64):
  /usr/include/i386/_types.h: __darwin_socklen_t
  /usr/include/sys/socket.h: AF_UNIX
  /usr/include/sys/un.h: struct sockaddr_un
|#

;; TODO:
;; - listener (bind/listen/accept)

(require racket/contract
         (rename-in ffi/unsafe (-> -->))
         ffi/unsafe/atomic
         ffi/unsafe/custodian
         ffi/unsafe/define
         ffi/file)

(provide
 (contract-out
  [unix-socket-available?
   boolean?]
  [unix-socket-connect
   (-> unix-socket-path? (values input-port? output-port?))]
  [unix-socket-path?
   (-> any/c boolean?)]))


;; Data structures and error handling code differs between the platforms.
(define platform
  (cond
    [(eq? (system-type 'os) 'macosx)
     'macosx]
    [(regexp-match? #rx"^Linux" (system-type 'machine))
     'linux]
    [else
     #f]))

(define unix-socket-available?
  (and platform #t))

(define AF-UNIX 1)
(define SOCK-STREAM 1)
(define SOCK_NONBLOCK #o00004000)

;; linux: asm-generic/{errno-base,errno}.h; macosx: sys/errno.h
(define EINTR           4)
(define EAGAIN          (case platform [(linux) 11]  [(macosx) 35]))
(define EWOULDBLOCK     EAGAIN)
(define EINPROGRESS     (case platform [(linux) 115] [(macosx) 36]))

;; linux: asm-generic/fcntl.h; macosx: sys/fcntl.h
(define F_SETFL         4)
(define O_NONBLOCK      (case platform [(linux) #o4000] [(macosx) 4]))

;; linux: asm-generic/socket.h; macosx: sys/socket.h
(define SOL_SOCKET      (case platform [(linux) 1] [(macosx) #xFFFF]))
(define SO_ERROR        (case platform [(linux) 4] [(macosx) #x1007]))

(define UNIX-PATH-MAX
  (case platform
    [(linux) 108]
    [else    104]))

(define _socklen_t
  (case platform
    [(linux) _uint]
    [else    _uint32]))

(define-cstruct _linux_sockaddr_un
  ([sun_family _ushort]
   [sun_path   (make-array-type _byte UNIX-PATH-MAX)]))

(define-cstruct _macosx_sockaddr_un
  ([sun_len    _ubyte]
   [sun_family _ubyte]
   [sun_path   (make-array-type _byte UNIX-PATH-MAX)]))

(define _sockaddr_un-pointer
  (case platform
    [(linux)  _linux_sockaddr_un-pointer]
    [(macosx) _macosx_sockaddr_un-pointer]
    [else     _pointer]))


(define-ffi-definer define-libc (ffi-lib #f)
  #:default-make-fail make-not-available)

(define-libc socket
  (_fun #:save-errno 'posix
        _int _int _int --> _int))

(define-libc connect
  (_fun #:save-errno 'posix
        _int _sockaddr_un-pointer _int --> _int))

(define-libc close
  (_fun #:save-errno 'posix
        _int --> _int))

(define-libc fcntl
  (_fun #:save-errno 'posix
        _int _int _int --> _int))

(define-libc getsockopt
  (_fun #:save-errno 'posix
        _int _int _int (value : (_ptr io _int) = 0) (len : (_ptr io _uint32) = (ctype-sizeof _int))
        --> (result : _int)
        --> (cond [(zero? result)
                   value]
                  [else
                   (define errno (saved-errno))
                   (error 'getsockopt "error\n  errno: ~a~a" errno (errno-error-line errno))])))

(define-libc scheme_make_fd_output_port
  (_fun _int _racket _bool _bool _bool --> _racket))

(define-libc scheme_fd_to_semaphore
  (_fun _intptr _int _bool --> _racket))

(define MZFD_CREATE_READ 1)
(define MZFD_CREATE_WRITE 2)

(define strerror-name
  (case platform
    [(linux) "__xpg_strerror_r"]
    [else    "strerror_r"]))

(define strerror_r
  (get-ffi-obj strerror-name #f
               (_fun (errno) ::
                     (errno : _int)
                     (buf : _bytes = (make-bytes 1000))
                     (buf-len : _size = (bytes-length buf))
                     --> _void
                     --> (cast buf _bytes _string/locale))
               (lambda ()
                 (lambda (errno) #f))))


(define (unix-socket-path? v)
  (and (unix-socket-path->bytes v) #t))

(define (unix-socket-path->bytes path)
  (if (path-string? path)
      ;; On all platforms, normal path of up to UNIX-PATH-MAX bytes after
      ;; conversion to absolute is considered valid and shall be accepted.
      (let ([bstr (path->bytes (cleanse-path (path->complete-path path)))])
        (and (<= (bytes-length bstr) UNIX-PATH-MAX) bstr))

      ;; On Linux, paths may be in so-called abstract namespace where they
      ;; start with #\nul and do not have a corresponding socket file.
      ;; We accept such paths only as byte strings because we don't know
      ;; the correct encoding.
      (and (eq? platform 'linux)
           (bytes? path)
           (> (bytes-length path) 0)
           (<= (bytes-length path) UNIX-PATH-MAX)
           (= (bytes-ref path 0) 0)
           path)))


(define (make-sockaddr path-bytes)
  (case platform
    [(linux)
     (make-linux_sockaddr_un AF-UNIX path-bytes)]
    [(macosx)
     (make-macosx_sockaddr_un (bytes-length path-bytes) AF-UNIX path-bytes)]))

(define (errno-error-line errno)
  (define err (strerror_r errno))
  (if err (format "\n  error: ~a" err) ""))

(define (check-platform who)
  (unless platform 
    (error who "unix domain sockets are not supported on this platform")))

;; do-make-sockaddr : Symbol Path/String -> (values Sockaddr-Pointer Nat)
(define (do-make-sockaddr who path)
  (when (path-string? path)
    (security-guard-check-file who path '(read write)))
  (define path-bytes (unix-socket-path->bytes path))
  (define sockaddr   (make-sockaddr path-bytes))
  (define addrlen    (+ (ctype-sizeof _ushort) (bytes-length path-bytes)))
  (values sockaddr addrlen))

;; do-make-socket : Symbol -> (values FD Cust-Reg)
(define (do-make-socket who)
  (define socket-fd  (socket AF-UNIX SOCK-STREAM 0))
  (unless (positive? socket-fd)
    (let ([errno (saved-errno)])
      (error who "failed to create socket\n  errno: ~a~a"
             errno (errno-error-line errno))))
  (unless (zero? (fcntl socket-fd F_SETFL O_NONBLOCK))
    (close socket-fd)
    (let ([errno (saved-errno)])
      (error who "failed to put socket in non-blocking mode\n  errno: ~a~a"
             errno (errno-error-line errno))))
  (values socket-fd (register-custodian-shutdown socket-fd close)))

;; close/unregister : Cust-Reg -> Void
(define (close/unregister fd reg)
  (close fd)
  (unregister-custodian-shutdown fd reg))

;; make-socket-ports : Symbol FD Cust-Reg -> (values Input-Port Output-Port)
(define (make-socket-ports who socket-fd reg)
  (with-handlers ([(lambda (e) #t)
                   (lambda (exn)
                     (close/unregister socket-fd reg)
                     (raise exn))])
    (begin0 (scheme_make_fd_output_port socket-fd 'unix-socket #f #f #t)
      ;; closing the ports closes socket-fd, so custodian no longer needs to manage directly
      (unregister-custodian-shutdown socket-fd reg))))

;; unix-socket-connect : Path/String -> (values Input-Port Output-Port)
(define (unix-socket-connect path)
  (check-platform 'unix-socket-connect)
  (define-values (sockaddr addrlen) (do-make-sockaddr 'unix-socket-connect path))
  (define connect-k
    ;; Non-blocking connect may succeed immediately or require waiting to see.
    ;; - If succeeds immediately, make ports in same atomic block
    ;; - If wait, must exit atomic mode to sync
    ;; So we return a procedure to be applied in non-atomic mode that does 
    ;; whatever needs doing.
    (call-as-atomic
     (lambda ()
       (define-values (socket-fd reg) (do-make-socket 'unix-socket-connect))
       (define r (connect socket-fd sockaddr addrlen))
       (define errno (saved-errno))
       (cond [(= r 0) ;; connected
              (define-values (in out) (make-socket-ports 'unix-socket-connect socket-fd reg))
              (lambda () (values in out))]
             [(= errno EINPROGRESS) ;; wait and see
              (define sema (scheme_fd_to_semaphore socket-fd MZFD_CREATE_WRITE))
              (lambda () ;; called in non-atomic mode!
                (sync sema)
                ;; FIXME: check custodian hasn't been shut down?
                (call-as-atomic
                 (lambda ()
                   (define errno (getsockopt socket-fd SOL_SOCKET SO_ERROR))
                   (cond [(= errno 0)
                          (make-socket-ports 'unix-socket-connect socket-fd reg)]
                         [else
                          (close/unregister socket-fd reg)
                          (error 'unix-socket-connect
                                 "failed to connect socket (non-blocking)\n  path: ~e\n  errno: ~a~a"
                                 path errno (errno-error-line errno))]))))]
             [else
              (close/unregister socket-fd reg)
              (let ([errno (saved-errno)])
                (error 'unix-socket-connect "failed to connect socket\n  path: ~e\n  errno:~a~a"
                       path errno (errno-error-line errno)))]))))
  (connect-k))
