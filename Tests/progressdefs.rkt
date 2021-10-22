#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         )

(define-extended-language crystal-lang+Γ crystal-lang
  [Γ · (Name : t Γ)])

(define-judgment-form
  crystal-lang+Γ
  #:mode (WF I I I O O)
  #:contract (WF Γ σ P t Γ)
;TODO add Γ to all the rules

  [--------------------------------
   (WF Γ σ Nil Nil Γ)]

  [--------------------------------
   (WF Γ σ Bool Bool Γ)]

  [--------------------------------
   (WF Γ σ Int32 Int32 Γ)]

  [--------------------------------
   (WF Γ σ String String Γ)]

   [(WF Γ σ P_1 Bool Γ)
   (WF Γ σ P_2 t_1 Γ)
   (WF Γ σ P_3 t_2 Γ)
   -----------------------------
   (WF Γ σ (if P_1 then P_2 else P_3) (t_1 t_2) Γ)]

  [(WF Γ σ var t Γ)
   (WF Γ σ P t Γ)
   -----------------------------
   (WF Γ σ (var = P) t Γ)]

  [(WF Γ σ P_1 t Γ)
   (WF Γ σ P_2 t Γ)
   (WF Γ σ P_3 t Γ)
   ...
   -----------------------------
   (WF Γ σ (P_1 P_2 P_3 ...) t Γ)]

  [(WF Γ σ P_1 Bool Γ)
   (WF Γ σ P_2 t Γ)
   -----------------------------
   (WF Γ σ (while P_1 P_2) t Γ )]

  [(WF Γ σ P_1 t Γ)
   (WF Γ σ P_2 t Γ)
   -----------------------------
   (WF Γ σ (P_1 binop P_2) t Γ)]

  [(WF Γ σ P_1 t Γ)
   -----------------------------
   (WF Γ σ (unop P_1) t Γ)]

  [(where (rp_1 ... (r v) rp_2 ...) σ)
   -----------------------------
   (WF Γ σ r t Γ)]
  
  [(WF Γ σ P_1 t Γ)
   (where Γ_1 (x Γ))
   (WF Γ_1 σ P_2 t Γ)
   -----------------------------
   (WF Γ σ (let x = P_1 in P_2) t Γ)]

  [(side-condition (in Γ Name))
   (where t (typeof Name Γ))
   -----------------------------
   (WF Γ σ Name t Γ)]
  
  [(where Int32 v)
   -----------------------------
   (WF Γ σ (- v) t Γ)]
  
  
  )

(define-metafunction crystal-lang+Γ
  [(in (Name_1 : t_1 Γ) Name_1)
   #t
   ]
  [(in · Name_1)
   #f]
  [(in (Name : t Γ) Name_1)
   (in Γ Name_1)])

(define-metafunction crystal-lang+Γ
  [(typeof (Name_1 : t_1 Γ) Name_1)
   t_1
   ]
  [(typeof · Name_1)
   nil]
  [(typeof (Name : t Γ) Name_1)
   (typeof Γ Name_1)])

(provide WF)

(define (WF? P)
  (not (null? (judgment-holds (WF · () ,P ())
                              #t))))

(define v? (redex-match? crystal-lang v))

(define (reduces? P)
  (not (null? (apply-reduction-relation
               full-rel
               (term (() : P))))))

(define (progress-holds? P)
  (if (WF? P)
      (or (v? P)
          (reduces? P))
      #t))

(redex-check crystal-lang P (progress-holds? (term P)))





