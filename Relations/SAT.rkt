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
   (SAT nil B)]

  [--------------------------------"SAT-FALSE"
   (SAT false B)]

  ; aca tengo que explicar que toda asignacion de tipos satisface una guarda que es int32 o str o true
  [--------------------------------"SAT-TRUE"
   (SAT true T)]
 
  [--------------------------------"SAT-INT32"
   (SAT int32 T)]

  [--------------------------------"SAT-STRING"
   (SAT str T)]

  [(SAT P_3 Γ_1)
   (SAT P_2 Γ_2)
   ; TODO: el gamma devuelto debiera combinar información de gamma_1 y _2
   ; quizás hacer un supremo de ambos
   ; TODO: comentario al márgen: efectivamente, en el código del parser,
   ; el ast de un if acepta como guarda un ASTNode: es decir, un término
   ; cualquiera
   -----------------------------"SAT-IF"
   (SAT (if P_1 then P_2 else P_3)  Γ_1)]
  
  [(SAT P Γ_1)
   () ;aca deberia una logica que explique como agrego a Name pero es imposible ya que no tengo info del Gamma anterior

   ; TODO: efectivamente, la signatura de SAT debiera incluir un gamma de
   ; entrada y de salida, además del gamma que ya tenés (que lo que contiene
   ; son las posibles soluciones de sat), pero estos nuevos
   ; gammas serían exclusivamente para incluir información de tipos introducida
   ; y modificada por construcciones como esta (una asignación)

   ; TODO: experimentando con el compilador de crystal parece notarse que una
   ; guarda de la forma "Name = P" es considerada verdadera si P evalua a algo
   ; truthy, así es que este caso se reduce a hacer 2 cosas:
   ; - actualizar en el nuevo gamma de salida la correspondiente información de
   ;   tipos sobre la variable Name (por si hiciera falta de ser utilizada en
   ;   otro lugar, fuera de esta construcción)
   ; - hacer sat de P y tomar el gamma que contiene las posibles soluciones de
   ;   sat. Ese es el gamma que contiene también las posibles soluciones de sat
   ;   pero para toda la construcción Name = P
   -----------------------------"SAT-DEFINE"
   (SAT (Name = P)  Γ_1)]
  
  [(SAT P Γ_1)
   () ;mismo que arriba como se que es una redefinicion si no se cosa del gamma anterior
   -----------------------------"SAT-REDEFINE"
   (SAT (Name = P) Γ_1)]
  
  [(SAT P_1 Γ_1)
   (SAT P_2 Γ_2)
   ; TODO: en base a experimentar con el compilador, pareciera que
   ; una concatenación de la forma P_1 P_2 ... que aparezca como guarda
   ; tiene el valor booleano que tome solamente P_1: se evalua todo el
   ; resto pero no se tiene en cuenta. Entonces, algo como:
   ; true; b = false; 1
   ; evalua a true, pero algo como:
   ; false; b = false; 1
   ; evalua a falso

   ; TODO: en base a lo anterior, sería necesario analizar la concatenación
   ; completa de construcciones, por si alguna tiene una asignación que
   ; altere la información de tipos de alguna variable (información de tipos
   ; que sea necesaria en otro lugar), pero la solución de SAT para esta
   ; construcción se reduce a la solución de SAT para la primer instrucción
   ; P_1
   --------------------------------------------------------------"SAT-2P"
   (SAT (P_1 P_2) Γ_1:Γ_2)]

  [(TR · (P_1 P_2 P_3 P_4 ...) t Γ_1)
   ; TODO: idem caso anterior, no haría falta hacer TR, sino SAT
   --------------------------------------------------------------"SAT-CONCAT"
   (SAT (P_1 P_2 P_3 P_4 ...) Γ_1)]

  [(TR · (while P_1 P_2) t Γ_1)
   -----------------------------"SAT-WHILE"
   (SAT (while P_1 P_2) Γ_1)]

  [(TR · (P_1 arithop P_2) t Γ_1)
   ; TODO: no debiera hacer falta hacer TR sino sat de P_1 y P_2, solo
   ; por si existe alguna instrucción en P_1 y P_2 que modifique
   ; tipos, pero notar que el resultado de una expresión aritmética
   ; bien tipada siempre es un nro, así es que esto siempre es verdadero
   ; sat de P_1 arithop P_2 es trivial: vale siempre
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