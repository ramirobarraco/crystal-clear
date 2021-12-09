#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/init.rkt"
         "../Meta-functions/Substitution.rkt"
         )
(define σϵ-rel
  (reduction-relation
   crystal-lang
   #:domain (σ : ϵ : P)
   #:arrow -->σϵ/P

   [-->σϵ/P (((r_1 v_1) ... (r_2 v_2) (r_3 v_3) ... ) : ((r_1 Name_1) ... (r_2 Name_2) (r_3 Name_3) ...) : (Name_2 = v))
            (((r_1 v_1) ... (r_2 v) (r_3 v_3) ... ) : ((r_1 Name_1) ... (r_2 Name_2) (r_3 Name_3) ...) : \;)
            reAssgn]

   [-->σϵ/P (((r_1 v_1) ... (r v) (r_2 v_2) ...) : ϵ : (r = v_3))
         (((r_1 v_1) ... (r v_3) (r_2 v_2) ...) : ϵ : \;)
         reference-REAssgn]

   [-->σϵ/P ((σ_1 : ((r_1 Name_1) ...) : (Name_2 = v_2))
             (σ_2 : ((r_2 Name_2) (r_1 Name_1) ...) : \;))
            (where (σ_2 (r_2)) (addVal σ_1 (v_2)))
            Naming]

   [-->σϵ/P (σ : ((r_1 Name_1) ... (r Name) (r_3 Name_3) ...) :  Name)
            (σ : ((r_1 Name_1) ... (r Name) (r_3 Name_3) ...) :  r)
            Dename]

   [-->σϵ/P (((r_1 v_1) ... (r v) (r_2 v_2) ...) : ϵ : r)
            (((r_1 v_1) ... (r v) (r_2 v_2) ...) : ϵ : v)
            Deref]
   
   [-->σϵ/P (((r_1 v_1) ... (r_2 v_2) (r_3 v_3) ... ) : ((r_1 Name_1) ... (r_2 Name_2) (r_3 Name_3) ...) : (t Name_2 = v))
            (((r_1 v_1) ... (r_2 v) (r_3 v_3) ... ) : ((r_1 Name_1) ... (r_2 Name_2) (r_3 Name_3) ...) : \;)
            typed-reAssgn]

   ;[-->σ/P (σ_1 : (let Name_1 Name_2 ..._1 = v_2 v_3 ..._1 in P))
    ;     (σ_2 : term(subst(P ((Name_1 r_2)(Name_2 r_3) ...))))
     ;    (where (σ_2 (r_2 r_3 ...)) (term(addVal σ_1 (v_2 v_3 ...))))
      ;   ]
  
   
   ))
(provide σϵ-rel)
   