#lang racket/base

(require net/tcp-sig
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
