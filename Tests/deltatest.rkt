#lang racket
(require redex
         math/base
         "../grammar.rkt"
         "../Meta-functions/delta.rkt")

(define (delta-test-suite)
  ; Arithmetic operations
  (test-equal (term (δbasic + 1 1))
              2)
  
  (test-equal (term (δbasic - 1 1))
              0)
  
  (test-equal (term (δbasic * 2 2))
              4)
  
  (test-equal (term (δbasic / 2 2))
              1)
  
  (test-equal (term (δbasic == 1 1))
              term true)
  
  (test-equal (term (δbasic < 1 2))
              term true)
  
  (test-equal (term (δbasic >= 2 2))
              term true)
  
  (test-equal (term (δbasic <= 2 2))
              term true)
  
  (test-equal (term (δbasic > 2 1))
              term true)
  
  (test-equal (term (δbasic and true true))
              term true)
  
  (test-equal (term (δbasic or true false))
              term true)
  )