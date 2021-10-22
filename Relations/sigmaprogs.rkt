#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/init.rkt"
         "../Meta-functions/Substitution.rkt"
         )
(define σ-rel
  (reduction-relation
   crystal-lang
   #:domain (σ : P)
   #:arrow -->σ/P
   [-->σ/P (((r_1 v_1) ... (r v) (r_2 v_2) ...) : (in-hole Ev r))
         (((r_1 v_1) ... (r v) (r_2 v_2) ...) : (in-hole Ev v))
         Local-Deref]

   [-->σ/P (((r_1 v_1) ... (r v) (r_2 v_2) ...) : (r = v_3))
         (((r_1 v_1) ... (r v_3) (r_2 v_2) ...) : \;)
         Local-Assgn]
   
   [-->σ/P (σ_1 : (let Name = v_2 in P))
         (σ_2 : (subst P ((Name r_2))))
         (where (σ_2 (r_2)) (addVal σ_1 (v_2)))
         ]
   
   [-->σ/P (σ_1 : (let t Name = v_2 in P))
         (σ_2 : (subst P ((Name r_2))))
         (where (σ_2 (r_2)) (addVal σ_1 (v_2)))
         ]


   ;[-->σ/P (σ_1 : (let Name_1 Name_2 ..._1 = v_2 v_3 ..._1 in P))
    ;     (σ_2 : term(subst(P ((Name_1 r_2)(Name_2 r_3) ...))))
     ;    (where (σ_2 (r_2 r_3 ...)) (term(addVal σ_1 (v_2 v_3 ...))))
      ;   ]
  
   
   ))
(provide σ-rel)
   