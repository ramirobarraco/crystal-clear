#lang racket
(require redex
         "../grammar.rkt"
         )

(define-metafunction crystal-lang
  [(init () v)
   0]
  
  [(init ((r_1 v_1) ... (r_2 v_2)))
   ,(+ 1 (term r_2))]
  
  )