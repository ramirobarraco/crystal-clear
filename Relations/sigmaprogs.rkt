#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/init.rkt"
         "../Meta-functions/Substitution.rkt"
         "../Meta-functions/aux_fun.rkt"
         )

; TODOM: documentación e indentación del código
(define σϵ-rel
  (reduction-relation
   crystal-lang
   #:domain (σ : ϵ : P)
   #:arrow -->σϵ/P
   
   [-->σϵ/P (((r_2 v_2) ... (r_1 v_1) (r_3 v_3) ...) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : Name_1)
            (((r_2 v_2) ... (r_1 v_1) (r_3 v_3) ...) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : v_1)
            Dename]

   [-->σϵ/P (((r_2 v_2) ... (r_1 v_1) (r_3 v_3) ... ) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : (Name_1 = v))
            (((r_2 v_2) ... (r_1 v) (r_3 v_3) ... ) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : v)
            reAssgn]

   [-->σϵ/P (σ_1 : ((Name_1 r_1) ...) : (Name_2 = v_2))
            (σ_2 : ((Name_2 r_2) (Name_1 r_1) ...) : v_2)
            
            (side-condition (not (term (name-in? ((Name_1 r_1) ...) Name_2 ))))
            
            (where (σ_2 (r_2)) (addVal σ_1 (v_2)))
            
            Naming]
   ))

(provide σϵ-rel)
