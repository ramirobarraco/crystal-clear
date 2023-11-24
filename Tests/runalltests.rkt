#lang racket
(require redex
         ; type-checking
         "./sattest.rkt"
         "./typecheckingtest.rkt"
         ; dyn. semantics
         "./fullprogstest.rkt"
         "./sigmaprogstest.rkt"
         rackunit/text-ui
         )

(define (test-type-checking)
  (print "testing sat:")
  (sat-test-suite)
  (print "testing complements of types:")
  (comp-test-suite)
  (print "testing sup. and inf. of types:")
  (inf-sup-test-suite)
  (print "testing sup. and inf. of typing contexts:")
  (inf-sup-contexts-test-suite)
  (print "testing subtyping:")
  (subtyping-test-suite)
  (print "testing typing relation:")
  (type-checking-test-suite)
  )

(define (test-dyn-sem)
  (print "testing sigma-progs-rel:")
  (sigma-progs-test-suite)
  (print "testing full-progs-rel:")
  (full-progs-rel-test-suite)
  )

(define (test-all)
  (check-redundancy #t)
  (caching-enabled? #t)
  (test-dyn-sem)
  (test-type-checking))

(test-all)
