#lang racket
(require redex
         "../grammar.rkt"
         )

(define-extended-language crystal-lang+Γ crystal-lang
  [Γ · (v : t Γ)])

(define-judgment-form
  crystal-lang
  #:mode (T I I I I)
  #:contract (T Γ σ P t)
  
  [--------------------------------
   (T Γ σ nil Nil)]

  [--------------------------------
   (T Γ σ bool Bool)]

  [--------------------------------
   (T Γ σ int32 Int32)]

  [--------------------------------
   (T Γ σ str Str)]
  
  [--------------------------------
   (T Γ σ union Union)]

   [(T Γ σ P_1 Bool)
   (T Γ σ P_2 t_1)
   (T Γ σ P_3 t_2)
   -----------------------------
   (T Γ σ (if P_1 then P_2 else P_3) (t_1 t_2))]
  
  )