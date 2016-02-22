#lang info

(define collection 'multi)
(define version "1.1")
(define deps '(("base" #:version "6.4.0.9")))
(define build-deps '("rackunit-lib"
                     ["unix-socket-lib" #:version "1.1"]))
(define pkg-authors '(ryanc))
