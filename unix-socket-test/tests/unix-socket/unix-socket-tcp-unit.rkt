#lang racket/base

(require net/tcp-sig
         racket/match
         racket/place
         racket/unit
         racket/unix-socket
         racket/unix-socket-tcp-unit
         rackunit
         "common.rkt")

(test-case* "make-unix-socket-tcp@"
  (call-in-custodian
   (lambda ()
     (define filename (make-temp-file-name))
     (define-values/invoke-unit (make-unix-socket-tcp@ filename)
       (import)
       (export tcp^))
     (define l (tcp-listen 80))
     (check-false (tcp-accept-ready? l))
     (define messages (make-channel))
     (thread
      (lambda ()
        (define-values (in _out)
          (unix-socket-connect filename))
        (channel-put messages 'connected)
        (when (eof-object? (read-byte in))
          (channel-put messages 'closed))))
     (check-equal? (sync/timeout 5 messages) 'connected)
     (check-true (tcp-accept-ready? l))
     (check-true (tcp-accept-ready? l))
     (define-values (in out)
       (tcp-accept l))
     (check-false (tcp-accept-ready? l))
     (close-output-port out)
     (tcp-close l)
     (check-equal? (sync/timeout 5 messages) 'closed))))

(module listener racket/base
  (require net/tcp-sig
           racket/place
           racket/unit
           racket/unix-socket-tcp-unit)
  (provide listen)
  (define (listen ch)
    (define filename (place-channel-get ch))
    (define-values/invoke-unit (make-unix-socket-tcp@ filename)
      (import)
      (export tcp^))
    (define l (tcp-listen 80))
    (parameterize-break #f
      (define received-break?
        (with-handlers ([exn:break? (lambda (_) #t)])
          (place-channel-put ch 'ready)
          (begin0 #f
            (tcp-accept/enable-break l))))
      (place-channel-get ch)
      (define accepted?
        (not (tcp-accept-ready? l)))
      (tcp-close l)
      (place-channel-put ch (list accepted? received-break?)))))

(module connector racket/base
  (require racket/place
           racket/unix-socket)
  (provide connect)
  (define (connect ch)
    (define filename (place-channel-get ch))
    (place-channel-put ch 'ready)
    (unix-socket-connect filename)
    (place-channel-put ch 'done)))

(test-case* "make-unix-socket-tcp@ tcp-accept/enable-break"
  (define filename (make-temp-file-name))
  (define listener-p
    (dynamic-place '(submod tests/unix-socket/unix-socket-tcp-unit listener) 'listen))
  (place-channel-put listener-p filename)
  (place-channel-get listener-p) ;; ready to accept
  (define connector-p
    (dynamic-place '(submod tests/unix-socket/unix-socket-tcp-unit connector) 'connect))
  (place-channel-put connector-p filename)
  (place-channel-get connector-p) ;; ready to connect
  (place-break listener-p)
  (place-channel-get connector-p)
  (place-channel-put listener-p 'continue)
  (match (place-channel-get listener-p)
    [(list #f #f) (fail "the connection was not accepted, nor was a break raised")]
    [(list #t #t) (fail "either the connection should be accepted or the break should be raised, but not both")]
    [_ (void)]))
