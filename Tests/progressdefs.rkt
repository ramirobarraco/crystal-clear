#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         )

(define-extended-language crystal-lang+Γ crystal-lang
  [Γ · (Name : t Γ)]
  [Θ (t ...)])

(define-judgment-form
  crystal-lang+Γ
  #:mode (WF I I I I)
  #:contract (WF Γ σ P Θ)
  
  [--------------------------------
   (WF Γ σ Nil Θ)]

  [--------------------------------
   (WF Γ σ Bool Θ)]

  [--------------------------------
   (WF Γ σ Int32 Θ)]

  [--------------------------------
   (WF Γ σ String Θ)]
  
  [--------------------------------
   (WF Γ σ Union Θ)]

   [(WF Γ σ P_1 (Bool))
   (WF Γ σ P_2 (t_1))
   (WF Γ σ P_3 (t_2))
   -----------------------------
   (WF Γ σ (if P_1 then P_2 else P_3) (t_1 t_2))]

  [(WF Γ σ var Θ)
   (WF Γ σ P Θ)
   -----------------------------
   (WF Γ σ (var = P) Θ)]

  [(WF Γ σ P_1 Θ)
   (WF Γ σ P_2 Θ)
   (WF Γ σ P_3 Θ)
   ...
   -----------------------------
   (WF Γ σ (P_1 P_2 P_3 ...) Θ)]

  [(WF Γ σ P_1 (Bool))
   (WF Γ σ P_2 Θ)
   -----------------------------
   (WF Γ σ (while P_1 P_2) Θ)]

  [(WF Γ σ P_1 Θ)
   (WF Γ σ P_2 Θ)
   -----------------------------
   (WF Γ σ (P_1 binop P_2) Θ)]

  [(WF Γ σ P_1 Θ)
   -----------------------------
   (WF Γ σ (unop P_1) Θ)]

  [(where (rp_1 ... (r v) rp_2 ...) σ)
   -----------------------------
   (WF Γ σ r Θ)]
  
  [(WF Γ σ P_1 Θ)
   (where Γ_1 (x Γ))
   (WF Γ_1 σ P_2 Θ)
   -----------------------------
   (WF Γ σ (let x = P_1 in P_2) Θ)]

  [(side-condition (in Γ Name))
   -----------------------------
   (WF Γ σ Name Θ)]
  
  [(where Int32 v)
   -----------------------------
   (WF Γ σ (- v) Θ)]
  
  
  )

(define-metafunction crystal-lang+Γ
  [(in (Name_1 : t_1 Γ) Name_1)
   #t
   ]
  [(in · Name_1)
   #f]
  [(in (Name : t Γ) Name_1)
   (in Γ Name_1)])

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





