#lang racket
(require redex
         "../grammar.rkt"
         )

(define-extended-language crystal-lang+Γ crystal-lang
  [Γ · (Name Γ)])

(define-judgment-form
  crystal-lang+Γ
  #:mode (WF I I I)
  #:contract (WF Γ σ P)
  
  [--------------------------------
   (WF Γ σ nil)]

  [--------------------------------
   (WF Γ σ bool)]

  [--------------------------------
   (WF Γ σ int32)]

  [--------------------------------
   (WF Γ σ str)]
  
  [--------------------------------
   (WF Γ σ union)]

   [(WF Γ σ P_1)
   (WF Γ σ P_2)
   (WF Γ σ P_3)
   -----------------------------
   (WF Γ σ (if P_1 then P_2 else P_3))]

  [(WF Γ σ var)
   (WF Γ σ P)
   -----------------------------
   (WF Γ σ (var = P))]

  [(WF Γ σ P_1)
   (WF Γ σ P_2)
   (WF Γ σ P_3)
   ...
   -----------------------------
   (WF Γ σ (P_1 P_2 P_3 ...) )]

  [(WF Γ σ P_1)
   (WF Γ σ P_2)
   -----------------------------
   (WF Γ σ (while P_1 P_2))]

  [(WF Γ σ P_1)
   (WF Γ σ P_2)
   -----------------------------
   (WF Γ σ (P_1 binop P_2))]

  [(WF Γ σ P_1)
   -----------------------------
   (WF Γ σ (unop P_1))]

  [
   -----------------------------
   (WF Γ σ r)]
  
  [(WF Γ σ P_1)
   (where Γ_1 (x Γ))
   (WF Γ_1 σ P_2)
   -----------------------------
   (WF Γ σ (let x = P_1 in P_2))]

  [(side-condition (in Γ Name))
   -----------------------------
   (WF Γ σ Name)]
  
  
  )

(provide WF)

(define-metafunction crystal-lang+Γ
  [(in (Name_1 Γ) Name_1)
   #t
   ]
  [(in · Name_1)
   #f]
  [(in (Name Γ) Name_1)
   (in Γ Name_1)])
