#lang info

(define collection 'multi)
(define version "1.1")
(define deps '(("base" #:version "6.4.0.9")
               ["unix-socket-lib" #:version "1.1"]))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define pkg-authors '(ryanc))
