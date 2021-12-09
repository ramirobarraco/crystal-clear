#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/progs.rkt")
(define (progs-rel-test-suite)
  
  (test-->> progs-rel
            (term (1 + 1))
            (term 2))

  (test-->> progs-rel
            (term (if true then 1 else 2))
            (term 1))
  
  (test-->> progs-rel
            (term (if false then 1 else 2))
            (term 2))

  (test-->> progs-rel
            (term (\; (1 + 1)))
            (term 2))
  
  (test-->> progs-rel
            (term (\; (1 + 1) (2 + 2) (3 + 3)))
            (term ((1 + 1) (2 + 2) (3 + 3))))

  (test-->> progs-rel
            (term (while false (bar = 1)))
            (term \;))

  (test--> progs-rel
           (term (isa? Bool true))
           (term true))

  (test--> progs-rel
           (term (isa? Int32 true))
           (term false))
)

(progs-rel-test-suite)
