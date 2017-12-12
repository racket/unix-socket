#lang info

(define collection 'multi)
(define version "1.1")
(define deps '("base"))
(define build-deps '("rackunit-lib"
                     ["unix-socket-lib" #:version "1.2"]))
(define pkg-authors '(ryanc))
