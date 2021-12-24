#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/init.rkt"
         "../Meta-functions/Substitution.rkt"
         "../Meta-functions/aux_fun.rkt"
         )

(define σϵ-rel
  (reduction-relation
   crystal-lang
   #:domain (σ : ϵ : P)
   #:arrow -->σϵ/P
   [-->σϵ/P (((r_2 v_2) ... (r_1 v_1) (r_3 v_3) ...) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : (in-hole Ev Name_1))
         (((r_2 v_2) ... (r_1 v_1) (r_3 v_3) ...) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : (in-hole Ev v_1))
         Dename]

   [-->σϵ/P (((r_2 v_2) ... (r_1 v_1) (r_3 v_3) ... ) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : (Name_1 = v))
            (((r_2 v_2) ... (r_1 v) (r_3 v_3) ... ) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : nil)
            reAssgn]

   [-->σϵ/P (σ_1 : ((Name_1 r_1) ...) : (Name_2 = v_2))
             (σ_2 : ((Name_2 r_2) (Name_1 r_1) ...) : nil)
            (side-condition (not (term (name-in? ((Name_1 r_1) ...) Name_2 ))))
            (where (σ_2 (r_2)) (addVal σ_1 (v_2)))
            Naming]

   
   [-->σϵ/P (((r_2 v_2) ... (r_1 v_1) (r_3 v_3) ... ) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : (t Name_1 = v))
            (((r_2 v_2) ... (r_1 v) (r_3 v_3) ... ) : ((Name_2 r_4) ... (Name_1 r_1) (Name_3 r_5) ...) : nil)
            typed-reAssgn]

   ;[-->σ/P (σ_1 : (let Name_1 Name_2 ..._1 = v_2 v_3 ..._1 in P))
   ;     (σ_2 : term(subst(P ((Name_1 r_2)(Name_2 r_3) ...))))
   ;    (where (σ_2 (r_2 r_3 ...)) (term(addVal σ_1 (v_2 v_3 ...))))
   ;   ]
  
   
   ))

(provide σϵ-rel)
