#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/typing.rkt"
         )

(provide (all-defined-out))
; some auxiliary functions useful to define SAT

; complement of varsol: types + expressions involving Names
(define-metafunction crystal-lang+Γ
  comp-varsol : varsol -> varsol

  [(comp-varsol t) (comp-t t)]
  
  [(comp-varsol (not varsol)) varsol]
  
  ; guards of the form Name_1 == Name_2 do not restrict the 'else'
  ; branch; hence, the complement here should not behave as with the
  ; remaining varsols: in the 'else' branch, the variable recover its
  ; original type
  [(comp-varsol (varsol_1 △ varsol_2)) varsol_1]
  
  [(comp-varsol varsol) (not varsol)]
  )

; complement of typing environments
(define-metafunction crystal-lang+Γ
  comp-SOL : SOL -> SOL
  ; TODO: no estoy seguro de esta ecuación
  [(comp-SOL ·) ·]
  
  [(comp-SOL (Name_1 : varsol SOL))
   (Name_1 : (comp-varsol varsol) (comp-SOL SOL))]
  )

; supremum of varsols
(define-metafunction crystal-lang+Γ
  sup-varsol : varsol varsol -> varsol

  [(sup-varsol t_1 t_2)
   (supreme-t t_1 t_2)]
  
  [(sup-varsol varsol varsol)
   varsol]

  ; {varsol_1 <> varsol_2}
  [(sup-varsol varsol_1 varsol_2)
   (varsol_1 ⊔ varsol_2)]
  )

; "supremum" of 2 given sat solutions: returns the most general
; solution (in terms of subtyping)
(define-metafunction crystal-lang+Γ
  sup-SOL : SOL SOL -> SOL
  ; recall that · defines ·(x) = x for all variable
  [(sup-SOL · ·) ·]
  
  [(sup-SOL (Name : varsol SOL) ·)
   (Name : (varsol ⊔ Name) (sup-SOL SOL ·))]
  
  [(sup-SOL · (Name : varsol SOL))
   (Name : (Name ⊔ varsol) (sup-SOL · SOL))]
  
  [(sup-SOL (Name_1 : varsol_1 SOL_1) (Name_1 : varsol_2 SOL_2))
   (Name_1 : (sup-varsol varsol_1 varsol_2) (sup-SOL SOL_1  SOL_2))]
  
  ;{Name_1 <> Name_2}
  [(sup-SOL (Name_1 : varsol_1 SOL_1) (Name_2 : varsol_2 SOL_2))
   (Name_1 : (sup-varsol varsol_1 varsol_3)
           (Name_2 : (sup-varsol varsol_2 varsol_4)
                   (sup-SOL (remove-SOL SOL_1 Name_2)
                            (remove-SOL SOL_2 Name_1))))
   (where varsol_3 (in-SOL (Name_2 : varsol_2 SOL_2) Name_1))
   (where varsol_4 (in-SOL (Name_1 : varsol_1 SOL_1) Name_2))]
  )

; removes the solution proposed for a given Name, into a given SOL
(define-metafunction crystal-lang+Γ
  remove-SOL : SOL Name -> SOL
  
  [(remove-SOL · Name)
   ·]
  
  [(remove-SOL (Name_1 : _ SOL) Name_1)
   SOL]

  ; {Name_1 <> Name_2}
  [(remove-SOL (Name_1 : any SOL) Name_2)
   (Name_1 : any (remove-SOL SOL Name_2))]
  )

; returns the solution proposed by a given SOL, for a given Name
(define-metafunction crystal-lang+Γ
  in-SOL : SOL Name -> varsol

  ; ∀ x, ·(x) = x 
  [(in-SOL · Name)
   Name]
  
  [(in-SOL (Name_1 : varsol SOL) Name_1)
   varsol]
  
  ; {Name_1 != Name_2}
  [(in-SOL (Name_1 : _ SOL) Name_2)
   (in-SOL SOL Name_2)])

; infimum of varsol
(define-metafunction crystal-lang+Γ
  inf-varsol : varsol varsol -> varsol
  ; base cases
  [(inf-varsol t_1 t_2) (inf-t t_1 t_2)]
  [(inf-varsol varsol varsol) varsol]
  [(inf-varsol varsol_1 varsol_2) (varsol_1 ⊓ varsol_2)]
  )

; "infimum" of 2 given sat-sol: returns the most restricted
; solution (in terms of subtyping) from 2 given possible solutions
; infimum of typing environments: the most restricted type assumptions
(define-metafunction crystal-lang+Γ
  inf-SOL : SOL SOL -> SOL

  ; recall that · defines ·(x) = x for all variable
  [(inf-SOL · ·)
   ·]
  
  [(inf-SOL (Name : varsol SOL) ·)
   (Name : (varsol ⊓ Name) (inf-SOL SOL ·))]
  
  [(inf-SOL · (Name : varsol SOL))
   (Name : (Name ⊓ varsol) (inf-SOL · SOL))]
  
  [(inf-SOL (Name_1 : varsol_1 SOL_1) (Name_1 : varsol_2 SOL_2))
   (Name_1 : (inf-varsol varsol_1 varsol_2) (inf-SOL SOL_1  SOL_2))]

  ; {Name_1 <> Name_2}
  [(inf-SOL (Name_1 : varsol_1 SOL_1) (Name_2 : varsol_2 SOL_2))
   (Name_1 : (inf-varsol varsol_1 varsol_3)
           (Name_2 : (inf-varsol varsol_2 varsol_4)
                   (inf-SOL (remove-SOL SOL_1 Name_2)
                            (remove-SOL SOL_2 Name_1))))
   
   (where varsol_3 (in-SOL (Name_2 : varsol_2 SOL_2) Name_1))
   (where varsol_4 (in-SOL (Name_1 : varsol_1 SOL_1) Name_2))]
  )

; denotational semantics for varsols
(define-metafunction crystal-lang+Γ
  denote-varsol : varsol -> t
  ; PRE : {every P occurring in the original varsol has been replaced by its type}
  [(denote-varsol t) t]

  [(denote-varsol (not varsol))
   (comp-t (denote-varsol varsol))]

  [(denote-varsol (varsol_1 ⊔ varsol_2))
   (supreme-t (denote-varsol varsol_1) (denote-varsol varsol_2))]

  [(denote-varsol (varsol_1 ⊓ varsol_2))
   (inf-t (denote-varsol varsol_1) (denote-varsol varsol_2))]

  [(denote-varsol (t △ varsol))
   (inf-t t (denote-varsol varsol))]
  )

; denotational semantics for a given SOL, that has embeded all the required
; typing information to build the corresponding Γ
; PRE : {every P occurring in varsols in SOL have been replaced by their type}
(define-metafunction crystal-lang+Γ
  denote-SOL : SOL -> Γ
  [(denote-SOL ·) ·]

  [(denote-SOL (Name : varsol SOL))
   (Name : (denote-varsol varsol)
         (denote-SOL SOL))]
  )


;                                
;                                
;                                
;                                
;                                
;                                
;     ;;;;;      ;;     ;;;;;;;; 
;    ;;   ;      ;;        ;;    
;    ;          ;;;        ;;    
;    ;          ; ;;       ;;    
;    ;;         ;  ;       ;;    
;     ;;;      ;;  ;       ;;    
;      ;;;;    ;   ;;      ;;    
;        ;;    ;;;;;;      ;;    
;         ;   ;;    ;      ;;    
;         ;   ;;    ;;     ;;    
;   ;;   ;;   ;     ;;     ;;    
;   ;;;;;;   ;;      ;     ;;    
;                                
;                                
;                                
;                                
;                                

(define-judgment-form
  crystal-lang+Γ
  #:mode (SAT I O)
  #:contract (SAT P SOL)

  ; constants: no type restriction at all
  
  [--------------------------------"SAT-NIL"
   (SAT nil ·)]

  [--------------------------------"SAT-FALSE"
   (SAT false ·)]

  [--------------------------------"SAT-TRUE"
   (SAT true ·)]
 
  [--------------------------------"SAT-INT32"
   (SAT int32 ·)]

  [--------------------------------"SAT-STRING"
   (SAT str ·)]

  [-----------------------------"SAT-ARITHOP"
   (SAT (P_1 arithop P_2) ·)]
  ;
  ;  TODO: opportunity to improve type narrowing: in the following example,
  ;  the compiler cannot infer that xshould be Int32, for the guard to hold:
  ;
  ;  x = 1
  ;  y = 1
  ;
  ;  if true
  ;     x = true
  ;  else
  ;     x = 1
  ;  end
  ;
  ;  if x == y
  ;      puts x + y
  ;  else
  ;      puts "no"
  ;  end
  ; 11 | puts x + y
  ;           ^
  ; Error: undefined method '+' for Bool (compile-time type is (Bool | Int32))
  ; pasa lo mismo con otras relaciones
  [(side-condition ,(not (equal? (term Name_1)
                                 (term Name_2))))
   -----------------------------------------------------------"SAT-RELOP-DIF-NAMES"
   (SAT (Name_1 == Name_2) (Name_1 : (Name_1 △ Name_2)
                           (Name_2 : (Name_2 △ Name_1) ·)))]

  ; no restriction
  [----------------------"SAT-RELOP-SAME-NAME"
   (SAT (Name == Name) ·)]

  [(side-condition ,(not (redex-match? crystal-lang+Γ Name (term P))))
   -------------------------------------------------------------------"SAT-RELOP-L-NAME"
   (SAT (Name == P) (Name : (Name △ P) ·))]

  
  [(side-condition ,(not (redex-match? crystal-lang+Γ Name (term P))))
   -------------------------------------------------------------------"SAT-RELOP-R-NAME"
   (SAT (P == Name) (Name : (Name △ P) ·))]

  
  [(side-condition ,(and (not (redex-match? crystal-lang+Γ Name (term P_1)))
                         (not (redex-match? crystal-lang+Γ Name (term P_2)))))
   ---------------------------------------------------------------------------"SAT-RELOP-NO-NAME"
   (SAT (P_1 == P_2) ·)]

  ; we do not improve type narrowing any further in this case:
  ; type checking itself will ask for P_1 and P_2 to have the same type, and that
  ; type being String or Int32
  [-----------------------"SAT-RELOP-NO-EQ"
   (SAT (P_1 relop P_2) ·)]
  
  ; (isa? t Name) : bool that indicates if Name has a type that
  ;  is a subtype of t, in run-time
  ; while is_a? determines the run-time type of a variable, "the compiler
  ; knows about it and it can affect type information", as indicated by the
  ; reference manual. Hence, it is taken into account in this sat step
  ; it can be trivially satisfied by a solution of the form Name : t
  [--------------------------------"SAT-ISA?"
   (SAT (isa? t Name) (Name : t ·))]
  
  ; NOTE: the compiler 1.5 disables type narrowing in guards like:
  ; x = 1
  ; if false || x.is_a?(Bool) 
  ;	puts "si"
  ;	puts x + "1"
  ; else
  ;	puts "no"
  ; end
  ;
  ; the previous program fails with type error:
  ;   5 | puts x + "1"
  ;            ^
  ;   Error: no overload matches 'Int32#+' with type String
  ; TODO: lo anterior puede ser modelado tranquilamente revisando
  ; que algunos de los SOL_i de los disyuntos sea B o T; en tal caso,
  ; devolvemos T
  [(SAT P_1 SOL_1)
   (SAT P_2 SOL_2)
   -----------------------------"SAT-OR"
   (SAT (P_1 or P_2) (sup-SOL SOL_1 SOL_2))]

  [(SAT P_1 SOL_1)
   (SAT P_2 SOL_2)
   -----------------------------"SAT-AND"
   (SAT (P_1 and P_2) (inf-SOL SOL_1 SOL_2))]
  
  [(SAT P SOL)
   -----------------------------"SAT-NOT"
   (SAT (not P) (comp-SOL SOL))]
  
  [(SAT P SOL)
   ;aca deberia una logica que explique como agrego a Name pero es imposible ya que no tengo info del Gamma anterior
  
   ; TODO: experimentando con el compilador de crystal parece notarse que una
   ; guarda de la forma "Name = P" es considerada verdadera si P evalua a algo
   ; truthy, así es que este caso se reduce a:
   ; - hacer sat de P y tomar el gamma que contiene las posibles soluciones de
   ;   sat. Ese será considerado como el gamma que contiene también las posibles
   ;   soluciones de sat para toda la construcción Name = P
   ;
   ; TODO: cosas raras que hace el compilador: esto termina con error de
   ;       compilación, y parece ser que se confunde directamente el compilador
   ;  x = 1
   ;
   ;  if x = "asdf" && x.is_a?(Bool)
   ;     puts x + 1
   ;  else
   ;     puts "no"
   ;  end
   ;
   ;   4 | puts x + 1
   ;            ^
   ; Error: undefined method '+' for Bool
     
   -----------------------------"SAT-DEFINE"
   (SAT (Name = P) SOL)]
  
  [-----------------------------"SAT-NEGATIVE"
   (SAT (- P_1) ·)]
  
  [-----------------------------"SAT-NAME"
   (SAT Name ·)]
  
  [; TODO: el gamma devuelto debiera combinar información de gamma_1 y _2
   ; quizás hacer un supremo de ambos
   ; TODO: comentario al márgen: efectivamente, en el código del parser,
   ; el ast de un if acepta como guarda un ASTNode: es decir, un término
   ; cualquiera
  
   ; TODO: en el siguiente programa, el compilador parece que simplemente
   ; aborta type narrowing:
   ;  x = true
   ;
   ;  if if true
   ;       x.is_a?(Int32)
   ;     else
   ;       x.is_a?(String)
   ;     end
   ;   
   ;     puts (x + 1)
   ;  else
   ;   
   ;  end
   ; falla con el error:
   ; 10 | puts (x + "1")
   ;              ^
   ;Error: undefined method '+' for Bool
   ; notar que esto es semejante a un or de x.is_a, tratar el or primero
   -----------------------------"SAT-IF"
   (SAT (if P_1 then P_2 else P_3) ·)]
    
  [; TODO: en base a experimentar con el compilador, pareciera que
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
   ; TODO: de todos modos, cuando una concatenación está anidada en otra
   ; expresión, se toma el valor de la última instrucción:
   ; if 1 + (1; false) 
   ;	  puts "si"
   ; else
   ;	  puts "no"
   ; end
   ; no tipa porque el operando de la derecha resulta ser false...
   --------------------------------------------------------------"SAT-2P"
   (SAT (P_1 P_2) ·)]
   
  [; TODO: idem caso anterior, no haría falta hacer TR, sino SAT
   --------------------------------------------------------------"SAT-CONCAT"
   (SAT (P_1 P_2 P_3 P_4 ...) ·)]
   
  [-----------------------------"SAT-WHILE"
   (SAT (while P_1 P_2) ·)]
   
  )