#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         "../Meta-functions/init.rkt"
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

   [-->σ/P (((r_1 v_1) ...) : (name = v_2 P))
         (((r_1 v_1) ...(r v_2)) : subst((P) (name r)))
         (where r (init((r_1 v_1) ...)))
         ]
  
   
   ))
(provide σ-rel)
   