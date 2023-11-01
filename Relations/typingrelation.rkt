#lang racket
(require redex
         "../grammar.rkt"
         "./fullprogs.rkt"
         ; TODO: usar subtyping
         "./subtyping.rkt"
         "./sat.rkt"
         "../Meta-functions/aux_fun.rkt"
         "../Meta-functions/typing.rkt"
         )


; flow-sensitive typing relation: Γ × P × t × Γ
(define-judgment-form
  crystal-lang+Γ
  #:mode (TR I  I O O )
  #:contract (TR Γ P t Γ)

  [--------------------------------"T-NIL"
   (TR Γ nil Nil Γ)]

  [--------------------------------"T-BOOL"
   (TR Γ bool Bool Γ)]

  [--------------------------------"T-INT32"
   (TR Γ int32 Int32 Γ)]

  [--------------------------------"T-STRING"
   (TR Γ str String Γ)]

  [(TR Γ_1 P_1 t_1 Γ_2)
   (TN Γ_2 P_1 Γ_3 Γ_4)
   (TR Γ_3 P_2 t_2 Γ_5)
   (TR Γ_4 P_3 t_3 Γ_6)
   -------------------------------------------------------------------------"T-IF"
   (TR Γ_1 (if P_1 P_2 else P_3) (supreme-t t_2 t_3) (supreme-Γ Γ_5 Γ_6))]

  [(TR Γ P t_1 Γ)
   (side-condition ,(redex-match? crystal-lang+Γ (#f _) (term (in-Γ Γ Name))))
   (side-condition ,(not (redex-match? crystal-lang+Γ unit (term t_1))))
   (where Γ_1 (Name : t_1 Γ))
   -----------------------------"T-DEFINE"
   (TR Γ (Name = P) t_1 Γ_1)]
  
  [(TR Γ P t_1 Γ)
   (side-condition ,(redex-match? crystal-lang+Γ (#t _) (term (in-Γ Γ Name))))
   (side-condition ,(not (redex-match? crystal-lang+Γ unit (term t_1))))
   (where Γ_1 (Name : t_1 (remove-Γ Γ Name)))
   -----------------------------"T-REDEFINE"
   (TR Γ (Name = P) t_1 Γ_1)]

  [(TR Γ_1 P_1 t_1 Γ_2)
   (TR Γ_2 P_2 t_2 Γ_3)
   --------------------------------------------------------------"T-2P"
   (TR Γ_1 (P_1 P_2) t_2 Γ_3)]

  [(TR Γ_1 P_1 t_1 Γ_2)
   (TR Γ_2 (P_2 P_3 P_4 ...) t_2 Γ_3)
   --------------------------------------------------------------"T-CONCAT"
   (TR Γ_1 (P_1 P_2 P_3 P_4 ...) t_2 Γ_3)]

  [(TN Γ P_1 Γ_1 Γ_3)
   (TR Γ_1 P_2 t Γ_2)
   (TN Γ_2 P_1 Γ_1 Γ_3)
   -----------------------------"T-WHILE"
   (TR Γ (while P_1 P_2) t (supreme-Γ Γ_3 Γ_2))]

  [(TR Γ P_1 Int32 Γ)
   (TR Γ P_2 Int32 Γ)
   -----------------------------"T-ARITHOP"
   (TR Γ (P_1 arithop P_2) Int32 Γ)]
  
  [(TR Γ P_1 t Γ)
   (TR Γ P_2 t Γ)
   (side-condition ,(or (equal? (term t) (term String))
                        (equal? (term t) (term Int32))))
   ---------------------------------------------------------------------"T-RELOP"
   (TR Γ (P_1 relop P_2) Bool Γ)]

  [(TR Γ P_1 t_1 Γ)
   (TR Γ P_2 t_2 Γ)
   --------------------------"T-EQ"
   (TR Γ (P_1 == P_2) Bool Γ)]

  [(TR Γ P_1 String Γ)
   (TR Γ P_2 String Γ)
   ----------------------------"T-STRCONCAT"
   (TR Γ (P_1 .. P_2) String Γ)]

  ; TODO: ; el tipo de ambos conyuntos no tiene por qué ser bool, puede ser cualquier cosa
  ; TODO: para el caso de &&, de https://crystal-lang.org/reference/1.6/syntax_and_semantics/and.html:
  ; "Its type is the union of the types of both sides."
  [(TR Γ P_1 t_1 Γ)
   (TR Γ P_2 t_2 Γ)
   (where t_3 (supreme-t t_1 t_2))
   -----------------------------"T-SHORTBINOP"
   (TR Γ (P_1 shortbinop P_2) t_3 Γ)]

  [(TR Γ P _ Γ) ; it does not matter the actual type of P
   -----------------------------"T-NOT"
   (TR Γ (not P) Bool Γ)]

  [(TR Γ P_1 Int32 Γ)
   -----------------------------"T-NEGATIVE"
   (TR Γ (- P_1) Int32 Γ)]

  [(side-condition ,(redex-match? crystal-lang+Γ (#t _) (term (in-Γ Γ Name))))
   (where t (typeof-Γ Γ Name))
   -----------------------------"T-NAME"
   (TR Γ Name t Γ)]
  
  [(TR Γ_1 P t_2 Γ_2)
   ----------------------------"T-ISA?"
   (TR Γ_1 (isa? t_1 P) Bool Γ_2)]
  
  )

; type narrowing
(define-judgment-form
  crystal-lang+Γ
  #:mode (TN I I O O)
  #:contract (TN Γ P Γ Γ)
  
  [(SAT P SOL_1)
   ; SOL could have some varsols of the form P, for an arbitrary P;
   ; we type and replace them with their type in SOL
   (where SOL_2 (type-SOL-terms SOL_1 Γ))
   
   (where Γ_1 (inst-SOL SOL_2 Γ))
   (where Γ_2 (inst-SOL (comp-SOL SOL_2) Γ))

   ; inf. between the proposed Γ_1 and the original Γ
   (where Γ_3 (inf-Γ Γ Γ_1))
   (where Γ_4 (inf-Γ Γ Γ_2))
   (side-condition ,(println (term SOL_1)))
   (side-condition ,(println (term Γ_3)))
   (side-condition ,(println (term Γ)))
   --------------------------------------------------------------------
   (TN Γ P Γ_3 Γ_4)]

  )

(provide (all-defined-out))


; to replace varsols of the form P, present into a given SOL,
; with their respective type
(define-metafunction crystal-lang+Γ
  type-SOL-terms : SOL Γ -> SOL

  [(type-SOL-terms · _)
   ·]
  
  [(type-SOL-terms (Name : varsol SOL) Γ)
   (Name : (type-varsol-terms varsol Γ)
           (type-SOL-terms SOL Γ))]
  )


(define-metafunction crystal-lang+Γ
  type-varsol-terms : varsol Γ -> varsol

  [(type-varsol-terms P Γ_1)
   t

   (where ((t _)) ,(judgment-holds (TR Γ_1 P t Γ_2) (t Γ)))]
  
  [(type-varsol-terms (not varsol) Γ)
   (not (type-varsol-terms varsol Γ))]
  
  [(type-varsol-terms (not varsol) Γ)
   (not (type-varsol-terms varsol Γ))]
  
  [(type-varsol-terms (varsol_1 ⊔ varsol_2) Γ)
   ((type-varsol-terms varsol_1 Γ) ⊔ (type-varsol-terms varsol_2 Γ))]
  
  [(type-varsol-terms (varsol_1 ⊓ varsol_2) Γ)
   ((type-varsol-terms varsol_1 Γ) ⊓ (type-varsol-terms varsol_2 Γ))]
  
  [(type-varsol-terms varsol _)
   varsol]
  )
; TODO: esta función para no usarse
(define-metafunction crystal-lang+Γ
  make-Γ : σϵprog -> Γ
  [(make-Γ (σ : () : P)) · ]
  
  [(make-Γ (((r v) ... (r_1 v_1) (r v) ...) : ((Name_1 r_1) (Name_2 r_2) ...) : P))
   (Name_1 : (typeof-v v_1) (make-Γ (((r v) ... (r_1 v_1) (r v) ...) : ((Name_2 r_2) ...) : P)))]
  
  [(make-Γ (((r v) ...) : ((Name_1 r_1) (Name_2 r_2) ...) : P))
   (Name_1 : Nil (make-Γ (((r v) ...) : ((Name_2 r_2) ...) : P)))]
  )

(define-metafunction crystal-lang
  get-σ : σϵprog -> σ
  [(get-σ (σ : ϵ : P)) σ]
  )

(define-metafunction crystal-lang
  get-ϵ : σϵprog -> ϵ
  [(get-ϵ (σ : ϵ : P)) ϵ]
  )

(define-metafunction crystal-lang
  get-P : σϵprog -> P
  [(get-P (σ : ϵ : P)) P]
  )

(define-metafunction crystal-lang
  [(check-σ ()) #t]
  [(check-σ ((r_1 v_1) (r_2 v_2) ...))
   (check-σ ((r_2 v_2) ...))
   (side-condition (not (term (r-in-σ? ((r_2 v_2) ...) r_1))))
   or
   #f])

(define-metafunction crystal-lang
  [(check-ϵ ()) #t]
  [(check-ϵ ((Name_1 r_1) (Name_2 r_2) ...))
   (check-ϵ ((Name_2 r_2) ...))
   (side-condition (not (term (name-in? ((Name_2 r_2) ...) Name_1))))
   or
   #f])

(define-metafunction crystal-lang
  [(fix_missing_values_σ () ((Name_1 r_1) (Name_2 r_2) ...))
   (fix_missing_values_σ ((r_1 nil)) ((Name_2 r_2) ...))]
  
  [(fix_missing_values_σ ((r_1 v_1) ...) ())
   ((r_1 v_1) ...)
   ]

  [(fix_missing_values_σ ((r_1 v_1) ...) ((Name_1 r_2)))
   ((r_2 nil) (r_1 v_1) ...)
   (side-condition (not (term (r-in-σ? ((r_1 v_1) ...) r_2))))]
  
  [(fix_missing_values_σ ((r_1 v_1) ...) ((Name_1 r_2) (Name_2 r_3) ...))
   (fix_missing_values_σ ((r_2 nil) (r_1 v_1) ...) ((Name_2 r_3) ...))
   (side-condition (not (term (r-in-σ? ((r_1 v_1) ...) r_2))))]
  
  [(fix_missing_values_σ ((r_1 v_1) ...) ((Name_1 r_2)))
   ((r_1 v_1) ...)
   (side-condition (term (r-in-σ? ((r_1 v_1) ...) r_2)))]
  
  [(fix_missing_values_σ ((r_1 v_1) ...) ((Name_1 r_2) (Name_2 r_3) ...))
   (fix_missing_values_σ ((r_1 v_1) ...) ((Name_2 r_3) ...))
   (side-condition (term (r-in-σ? ((r_1 v_1) ...) r_2)))]

  )

(define-metafunction crystal-lang
  fix_missing_values : σϵprog -> σϵprog
  
  [(fix_missing_values (() : () : P))
   (() : () : P)
   ]
  
  [(fix_missing_values (((r_1 v_1) ...) : () : P))
   (((r_1 v_1) ...) : () : P)
   ]
  
  [(fix_missing_values (((r_1 v_1) ...) : ϵ_1 : P))
   (σ : ϵ_1 : P)
   (where σ (fix_missing_values_σ ((r_1 v_1) ...) ϵ_1))
   ]
  
  )

(define (prepare σϵprog)
  (if (and (term (check-σ (get-σ ,σϵprog))) (term (check-ϵ (get-ϵ ,σϵprog))))
      
      (let ([x (term (fix_missing_values  ,σϵprog))])
        (term ,x))
      
      (println (term ,σϵprog))))


(define (TR? σϵprog)
  (not (null? (judgment-holds (TR (make-Γ ,σϵprog) (get-P ,σϵprog) t Γ)
                              (t Γ))))
  )

(define v? (redex-match? crystal-lang v))


(define (reduces? σϵprog)
  (not (null? (apply-reduction-relation
               full-rel
               (term ,σϵprog)))))

(define (progress-holds? σϵprog)
  (if (TR? σϵprog)
      (or (v? (term (get-P ,σϵprog)))
          (reduces? σϵprog))
      (begin (print ("progress failed ")) #t)))


;checks that if its well typed then it only has one or less reductions
(define (reduce1 σϵprog)
  (if (TR? σϵprog)
      (<= (length (apply-reduction-relation full-rel (term ,σϵprog)))
          1)
      (begin (print ("reduce1 failed ")) #t)))

(define (reducestyped? σϵprog)
  (TR? (list-ref (apply-reduction-relation
                  full-rel
                  (term ,σϵprog)) 0)))

(define (preservation-holds? σϵprog)
  (if (TR? σϵprog)
      (or (v? (term (get-P ,σϵprog)))
          (reducestyped? σϵprog))
      (begin (print ("preservation failed ")) #t)))

(define (safety? σϵprog)
  (and
   (progress-holds? (term ,σϵprog))
   (reduce1 (term ,σϵprog))
   (preservation-holds? (term ,σϵprog)))
  )

