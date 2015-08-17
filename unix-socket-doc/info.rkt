#lang info

(define collection 'multi)
(define version "1.0")
(define deps '(("base" #:version "6.2.900.10")
               "unix-socket-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define pkg-authors '(ryanc))
