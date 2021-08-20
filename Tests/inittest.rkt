#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/init.rkt")

(define (init-test-suite)
  ; expression without structure
  (test-equal (term (addVal () ()))
              (term (() ())))
  
  (test-equal (term (addVal (((ref 1) 3)) ()))
              (term ((((ref 1) 3)) ())))
  
  (test-equal (term (addVal () (3)))
              (term ((((ref 0) 3)) ((ref 0)))))

  (test-equal (term (addVal (((ref 1) 3)) (3)))
              (term ((((ref 1) 3)((ref 2) 3)) ((ref 2)))))

  (test-equal (term (addVal (((ref 1) 3)) (3 2)))
              (term ((((ref 1) 3)((ref 2) 3) ((ref 3) 2)) ((ref 2)(ref 3)))))

  )
(init-test-suite)