#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         "../Relations/subtyping.rkt"
         "../Meta-functions/aux_fun.rkt"
         "../Relations/sigmaprogs.rkt"
         "../Relations/progs.rkt"
         "../Tests/progressdefs.rkt"
         )

(define-judgment-form
  crystal-lang+Γ
  #:mode (SAT I O)
  #:contract (SAT P Γ)
  ; aca Tengo que explicar que no hay ninguna asignacion de tipos satisface a una guarda que es nil o false
  
  [--------------------------------"SAT-NIL"
   (SAT nil  ·)]

  [--------------------------------"SAT-FALSE"
   (SAT false ·)]

  ; aca tengo que explicar que toda asignacion de tipos satisface una guarda que es int32 o str o true
  [--------------------------------"SAT-TRUE"
   (SAT true ·)]
 
  [--------------------------------"SAT-INT32"
   (SAT int32  ·)]

  [--------------------------------"SAT-STRING"
   (SAT str  ·)]

  [(SAT P_3 Γ_1)
   (SAT P_2 Γ_2)
   -----------------------------"SAT-IF"
   (SAT (if P_1 then P_2 else P_3)  Γ_1)]
  
  [(SAT P Γ_1)
   () ;aca deberia una logica que explique como agrego a Name pero es imposible ya que no tengo info del Gamma anterior
   -----------------------------"SAT-DEFINE"
   (SAT (Name = P)  Γ_1)]
  
  [(SAT P Γ_1)
   () ;mismo que arriba como se que es una redefinicion si no se cosa del gamma anterior
   -----------------------------"SAT-REDEFINE"
   (SAT (Name = P) Γ_1)]
  
  [(SAT P_1 Γ_1)
   (SAT P_2 Γ_2)
   --------------------------------------------------------------"SAT-2P"
   (SAT (P_1 P_2) Γ_1:Γ_2)]

  [(TR · (P_1 P_2 P_3 P_4 ...) t Γ_1)
   --------------------------------------------------------------"SAT-CONCAT"
   (SAT (P_1 P_2 P_3 P_4 ...) Γ_1)]

  [(TR · (while P_1 P_2) t Γ_1)
   -----------------------------"SAT-WHILE"
   (SAT (while P_1 P_2) Γ_1)]

  [(TR · (P_1 arithop P_2) t Γ_1)
   -----------------------------"SAT-ARITHOP"
   (SAT (P_1 arithop P_2)  Γ_1)]
  
  [(TR · (P_1 relop P_2) t Γ_1)
   -----------------------------"SAT-RELOP"
   (SAT (P_1 relop P_2)  Γ_1)]

  [SAT P_1 Γ_1
   SAT P_2 Γ_2
   -----------------------------"SAT-OR"
   (SAT (P_1 or P_2)  Supremo(Γ_1 Γ_2))]

  [SAT P_1 Γ_1
   SAT P_2 Γ_2
   -----------------------------"SAT-AND"
   (SAT (P_1 and P_2) Γ_1)]

  [(TR · P t Γ_1)
   -----------------------------"SAT-NOT"
   (SAT (not P_1)  Γ_1)]

  [(TR · (- P_1) t Γ_1)
   -----------------------------"SAT-NEGATIVE"
   (SAT (- P_1)  Γ_1)]

  [(TR · Name t Γ_1)
   -----------------------------"SAT-NAME"
   (SAT Name  Γ_1)]
  
  [(where Γ_1 (Name : t ·))
   ----------------------------"SAT-ISA?"
   (SAT (isa? t Name)  Γ_1)]
  )