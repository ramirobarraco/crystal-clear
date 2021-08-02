#lang racket
(require redex
         math/base
         "../grammar.rkt"
         "../Meta-functions/delta.rkt")

(define (delta-test-suite)
  ; Arithmetic operations
  (test-equal (term (δ + 1 1))
              2)
  
  (test-equal (term (δ - 1 1))
              0)

  (test-equal (term (δ - 0))
              -0)
  
  (test-equal (term (δ * 2 2))
              4)
  
  (test-equal (term (δ / 2 2))
              1)
  
  (test-equal (term (δ == 1 1))
              (term true))
  
  (test-equal (term (δ < 1 2))
              (term true))
  
  (test-equal (term (δ >= 2 2))
              (term true))
  
  (test-equal (term (δ <= 2 2))
              (term true))
  
  (test-equal (term (δ > 2 1))
              (term true))
  
  (test-equal (term (δ and true true))
              (term true))
  
  (test-equal (term (δ or true false))
              (term true))
  )
(delta-test-suite)
