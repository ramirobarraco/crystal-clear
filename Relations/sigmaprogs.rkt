#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         )
(define σ-rel
  (reduction-relation
   crystal-lang
   #:domain (σ:P)
   #:arrow -->σ/P
   [-->σ/P (((r_1 v_1) ... (r v) (r_2 v_2) ...) : (in-hole Ev r))
         (((r_1 v_1) ... (r v) (r_2 v_2) ...) : (in-hole Ev v))
         Local-Deref]

   [-->σ/P (((r_1 v_1) ... (r v) (r_2 v_2) ...) : (r = v_3))
         (((r_1 v_1) ... (r v_3) (r_2 v_2) ...) : \;)
         Local-Assgn]

   


   
   
   ))
(provide σ-rel)
   