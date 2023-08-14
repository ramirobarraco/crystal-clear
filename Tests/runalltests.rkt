#lang racket
(require redex
         ; Type-checking
         "./sattest.rkt"
         "./typecheckingtest.rkt"
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
  (print "testing typing relation:")
  (type-checking-test-suite)
  )

(define (test-all)
  (check-redundancy #t)
  (caching-enabled? #t)
  (test-type-checking))

(test-all)
