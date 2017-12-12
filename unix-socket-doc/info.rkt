#lang info

(define collection 'multi)
(define version "1.1")
(define deps '("base"
               ["unix-socket-lib" #:version "1.2"]))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define pkg-authors '(ryanc))
