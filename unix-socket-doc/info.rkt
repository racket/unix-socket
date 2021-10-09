#lang info

(define collection 'multi)
(define version "1.1")
(define deps '("base"
               "net-lib"
               ["unix-socket-lib" #:version "1.3"]
               "web-server-lib"))
(define build-deps '("scribble-lib"
                     "net-doc"
                     "racket-doc"
                     "web-server-doc"))
(define pkg-authors '(ryanc))

(define license
  '(Apache-2.0 OR MIT))
