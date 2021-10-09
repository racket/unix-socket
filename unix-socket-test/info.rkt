#lang info

(define collection 'multi)
(define version "1.1")
(define deps '("base"))
(define build-deps '("net-lib"
                     "rackunit-lib"
                     ["unix-socket-lib" #:version "1.3"]))
(define pkg-authors '(ryanc))

(define license
  '(Apache-2.0 OR MIT))
