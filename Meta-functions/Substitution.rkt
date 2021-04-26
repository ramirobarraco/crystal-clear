#lang racket
(require redex/tut-subst
         "../grammar.rkt"
         )

(define-metafunction ext-lang
  subst : name v e -> e
  [(subst name v e)
   ,(subst/proc name? (list (term name))(list (term v))(term e))]
  (define name? (redex-match ext-lang x))
  
  