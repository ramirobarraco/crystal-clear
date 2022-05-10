#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         "../Relations/subtyping.rkt"
         "../Meta-functions/aux_fun.rkt"
         "../Relations/sigmaprogs.rkt"
         "../Relations/progs.rkt"
         )

(define-judgment-form
  crystal-lang+Γ
  #:mode (SAT I I O)
  #:contract (SAT Γ P Γ)
  
  [--------------------------------"T-NIL"
   (SAT Γ nil Nil Γ)]

  [--------------------------------"T-BOOL"
   (SAT Γ bool Bool Γ)]

  [--------------------------------"T-INT32"
   (SAT Γ int32 Int32 Γ)]

  [--------------------------------"T-SSATING"
   (SAT Γ str String Γ)]

  [(TN Γ P_1 Γ_1 Γ_2)
   (SAT Γ_1 P_2 t_1 Γ_3)
   (SAT Γ_2 P_3 t_2 Γ_4)
   -----------------------------"T-IF"
   (SAT Γ (if P_1 then P_2 else P_3) (supreme-t t_1 t_2) (supreme-Γ Γ_3 Γ_4))]
  
  [(SAT Γ P t_1 Γ)
   (side-condition ,(redex-match? crystal-lang+Γ (#f _) (term (in-Γ Γ Name))))
   (side-condition ,(not (redex-match? crystal-lang+Γ Unit (term t_1))))
   (where Γ_1 (Name : t_1 Γ))
   -----------------------------"T-DEFINE"
   (SAT Γ (Name = P) t_1 Γ_1)]
  
  [(SAT Γ P t_1 Γ)
   (side-condition ,(redex-match? crystal-lang+Γ (#t _) (term (in-Γ Γ Name))))
   (side-condition ,(not (redex-match? crystal-lang+Γ Unit (term t_1))))
   (where Γ_1 (Name : t_1 (remove-Γ Γ Name)))
   -----------------------------"T-REDEFINE"
   (SAT Γ (Name = P) t_1 Γ_1)]

  [(concat Γ_1 P_2 P_1 t_1 Γ_2)
   --------------------------------------------------------------"T-2P"
   (SAT Γ_1 (P_1 P_2) t_1 Γ_2)]

  [(concat Γ_1 (P_2 P_3 P_4 ...) P_1 t_1 Γ_2)
   --------------------------------------------------------------"T-CONCAT"
   (SAT Γ_1 (P_1 P_2 P_3 P_4 ...) t_1 Γ_2)]

  [(TN Γ P_1 Γ_1 Γ_3)
   (SAT Γ_1 P_2 t Γ_2)
   -----------------------------"T-WHILE"
   (SAT Γ (while P_1 P_2) t (supreme-Γ Γ_3 Γ_2))]

  [(SAT Γ P_1 Int32 Γ)
   (SAT Γ P_2 Int32 Γ)
   -----------------------------"T-ARITHOP"
   (SAT Γ (P_1 arithop P_2) Int32 Γ)]
  
  [(SAT Γ P_1 Int32 Γ)
   (SAT Γ P_2 Int32 Γ)
   -----------------------------"T-RELOP"
   (SAT Γ (P_1 relop P_2) Bool Γ)]
  
  [(SAT Γ P_1 Bool Γ)
   (SAT Γ P_2 Bool Γ)
   -----------------------------"T-SHORTBINOP"
   (SAT Γ (P_1 shortbinop P_2) Bool Γ)]

  [(SAT Γ P_1 Bool Γ)
   -----------------------------"T-NOT"
   (SAT Γ (not P_1) Bool Γ)]

  [(SAT Γ P_1 Int32 Γ)
   -----------------------------"T-NEGATIVE"
   (SAT Γ (- P_1) Int32 Γ)]

  [(side-condition ,(redex-match? crystal-lang+Γ (#t _) (term (in-Γ Γ Name))))
   (where t (typeof-Γ Γ Name))
   -----------------------------"T-NAME"
   (SAT Γ Name t Γ)]
  
  [(SAT Γ P t Γ)
   ----------------------------"T-ISA?"
   (SAT Γ (isa? t P) Bool Γ)]
  )