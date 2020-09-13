#lang racket/base

(require racket/file
         rackunit)

(provide
 call-in-custodian
 make-temp-file-name
 test-case*)

(define (call-in-custodian proc)
  (parameterize ((current-subprocess-custodian-mode 'kill))
    (parameterize ((current-custodian (make-custodian)))
      (call-with-continuation-barrier
       (lambda ()
         (dynamic-wind void
                       proc
                       (lambda ()
                         (custodian-shutdown-all (current-custodian)))))))))

(define (make-temp-file-name)
  (define tmp ((values make-temporary-file)))
  (delete-file tmp)
  tmp)

(define-syntax-rule (test-case* name . body)
  (let ([name-var name])
    (printf "testing: ~a\n" name-var)
    (test-case name-var . body)))
