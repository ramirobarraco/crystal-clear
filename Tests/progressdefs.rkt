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

  [(TN Γ P_1 Γ_1 Γ_2)
   (TR Γ_1 P_2 t_1 Γ_3)
   (TR Γ_2 P_3 t_2 Γ_4)
   -----------------------------"T-IF"
   (TR Γ (if P_1 then P_2 else P_3) (supreme-t t_1 t_2) (supreme-Γ Γ_3 Γ_4))]
  
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

  [(concat Γ_1 P_2 P_1 t_1 Γ_2)
   --------------------------------------------------------------"T-2P"
   (TR Γ_1 (P_1 P_2) t_1 Γ_2)]

  [(concat Γ_1 (P_2 P_3 P_4 ...) P_1 t_1 Γ_2)
   --------------------------------------------------------------"T-CONCAT"
   (TR Γ_1 (P_1 P_2 P_3 P_4 ...) t_1 Γ_2)]

  [(TN Γ P_1 Γ_1 Γ_3)
   (TR Γ_1 P_2 t Γ_2)
   -----------------------------"T-WHILE"
   (TR Γ (while P_1 P_2) t (supreme-Γ Γ_3 Γ_2))]

  [(TR Γ P_1 Int32 Γ)
   (TR Γ P_2 Int32 Γ)
   -----------------------------"T-ARITHOP"
   (TR Γ (P_1 arithop P_2) Int32 Γ)]
  
  [(TR Γ P_1 Int32 Γ)
   (TR Γ P_2 Int32 Γ)
   -----------------------------"T-RELOP"
   (TR Γ (P_1 relop P_2) Bool Γ)]

  ; TODO: ; el tipo de ambos conyuntos no tiene por qué ser bool, puede ser cualquier cosa
  ; TODO: para el caso de &&, de https://crystal-lang.org/reference/1.6/syntax_and_semantics/and.html:
  ; "Its type is the union of the types of both sides."
  [(TR Γ P_1 Bool Γ)
   (TR Γ P_2 Bool Γ)
   -----------------------------"T-SHORTBINOP"
   (TR Γ (P_1 shortbinop P_2) Bool Γ)]

  [(TR Γ P_1 Bool Γ)
   -----------------------------"T-NOT"
   (TR Γ (not P_1) Bool Γ)]

  [(TR Γ P_1 Int32 Γ)
   -----------------------------"T-NEGATIVE"
   (TR Γ (- P_1) Int32 Γ)]

  [(side-condition ,(redex-match? crystal-lang+Γ (#t _) (term (in-Γ Γ Name))))
   (where t (typeof-Γ Γ Name))
   -----------------------------"T-NAME"
   (TR Γ Name t Γ)]
  
  [(TR Γ P t Γ)
   ----------------------------"T-ISA?"
   (TR Γ (isa? t P) Bool Γ)]


  
  )

(define-judgment-form
  crystal-lang+Γ
  #:mode (concat I I I O O)
  #:contract (concat Γ P P t Γ)

  [(TR Γ_1 P_1 t_1 Γ_2)
   (concat Γ_2 P_3 P_2 t_2 Γ_3)
   --------------------------------------------------------------------
   (concat Γ_1 (P_2 P_3) P_1 t_2 Γ_3)]

  [(TR Γ_1 P_1 t_1 Γ_2)
   (concat Γ_2 (P_3 P_4 P_5 ...) P_2 t_2 Γ_3)
   --------------------------------------------------------------------
   (concat Γ_1  (P_2 P_3 P_4 P_5 ...) P_1 t_2 Γ_3)]

  [(TR Γ_1 P_1 t_1 Γ_2)
   (TR Γ_2 P_2 t_2 Γ_3)
   --------------------------------------------------------------------
   (concat Γ_1 P_2 P_1 t_2 Γ_3)]
  )

(define-judgment-form
  crystal-lang+Γ
  #:mode (TN I I O O)
  #:contract (TN Γ P Γ Γ)
  
  [(where Γ_1 (inst-SOL (SAT P) Γ))
   (where Γ_2 (inst-SOL (comp-SOL(SAT P)) Γ))
   --------------------------------------------------------------------
   (TN Γ P Γ_1 Γ_2)]
  
  

  )

(provide (all-defined-out))



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

