#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/progs.rkt")
(define (progs-rel-test-suite)
  
  (test-->> progs-rel
            (term (1 + 1))
            (term 2))
)