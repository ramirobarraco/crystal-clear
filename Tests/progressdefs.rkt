#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         )

(define-extended-language crystal-lang+Γ crystal-lang
  [Γ · (Name : t Γ)]
  [Σ · (r : t Σ)]
  )

(define-judgment-form
  crystal-lang+Γ
  #:mode (TR I I I O O O)
  #:contract (TR Γ Σ P t Γ Σ)
;TODO implement suprem and typeof for reference

  [--------------------------------
   (TR Γ Σ Nil Nil Γ Σ)]

  [--------------------------------
   (TR Γ Σ Bool Bool Γ Σ)]

  [--------------------------------
   (TR Γ Σ Int32 Int32 Γ Σ)]

  [--------------------------------
   (TR Γ Σ String String Γ Σ)]

   [(TR Γ Σ P_1 Bool Γ_1 Σ_1)
   (TR Γ_1 Σ_1 P_2 t_1 Γ_2 Σ_2)
   (TR Γ_1 Σ_1 P_3 t_2 Γ_3 Σ_3)
   -----------------------------
   (TR Γ Σ (if P_1 then P_2 else P_3) (supreme-t t_1 t_2) (supreme-Γ Γ_2 Γ_3) (supreme-Σ Σ_2 Σ_3))]

  [(TR Γ Σ P t_1 Γ Σ_1)
   (where Σ_1 (r : t_1 Σ))
   -----------------------------
   (TR Γ Σ (r = P) t_1 Γ Σ_1)]

  [(TR Γ Σ P_1 t_1 Γ_1 Σ_1)
   (TR Γ_1 Σ_1 P_2 t_2 Γ_2 Σ_2)
   ...
   (TR Γ Σ P_3 t_3 Γ_3 Σ_3)
   -----------------------------
   (TR Γ Σ (P_1 P_2 ... P_3) t_3 Γ_3 Σ_3)]

  [(TR Γ Σ P_1 Bool Γ_1 Σ_1)
   (TR Γ_1 Σ_1 P_2 t Γ_2 Σ_2)
   -----------------------------
   (TR Γ Σ (while P_1 P_2) t Γ_2 Σ_2)]

  [(TR Γ Σ P_1 t Γ Σ)
   (TR Γ Σ P_2 t Γ Σ)
   -----------------------------
   (TR Γ Σ (P_1 binop P_2) t Γ Σ)]

  [(TR Γ Σ P_1 t Γ Σ)
   -----------------------------
   (TR Γ Σ (unop P_1) t Γ Σ)]

  [(side-condition (redex-match? crystal-lang+Γ (#t _) (term (in-Σ Σ r))))
   (where t (typeof-Σ Σ r))
   -----------------------------
   (TR Γ Σ r t Γ Σ)]
  
  [(TR Γ Σ P_1 t_1 Γ Σ)
   (where Γ_1 (Name : t_1 Γ))
   (TR Γ_1 Σ P_2 t_2 Γ Σ_1)
   -----------------------------
   (TR Γ Σ (let Name = P_1 in P_2) t_2 Γ Σ_1)]

  [(side-condition (redex-match? crystal-lang+Γ (#t _) (term (in-Γ Γ Name))))
   (where t (typeof-Γ Γ Name))
   -----------------------------
   (TR Γ Σ Name t Γ Σ)]

  [(where t_1 (is_a? v))
   ----------------------------
   (TR Γ Σ v t_1 Γ Σ)]
  )


; agregar caso en el que los t no son listas y usar codigo redexds en vez de racket dentro de las funciones
(define-metafunction crystal-lang+Γ
  supreme-t : t t -> t
  [(supreme-t st_1 st_1) st_1]
  [(supreme-t st_1 st_2) (st_1 st_2)]
  [(supreme-t st_1 (st_2 ...)) (st_2 ...) (side-condition (term(in-t (st_2 ...) st_1))) ]
  [(supreme-t st_1 (st_2 ...)) (st_1 st_2 ...) (side-condition (not (term(in-t (st_2 ...) st_1)))) ]
  [(supreme-t (st_1 ...) st_2) (supreme-t st_2 (st_1 ...))]
  [(supreme-t (st_1 ...) (st_2 ...)) t_3 (where t_3 ,(remove-duplicates (append (term (st_1 ...)) (term (st_2 ...)))))]
  )


(define-metafunction crystal-lang+Γ
  supreme-Γ : Γ Γ -> Γ
  [(supreme-Γ · Γ) Γ]
  [(supreme-Γ Γ ·) Γ]
  [(supreme-Γ Γ_1 (Name_1 : t_1 Γ_2)) (Name_1 : (supreme-t t_1 t_2) (supreme-Γ (remove-Γ Γ_1 Name_1) Γ_2))
                                      (side-condition (redex-match? crystal-lang+Γ (#t _) (term (in-Γ Γ_1 Name_1))))
                                      (where (#t t_2) (in-Γ Γ_1 Name_1))
                                      ]
  [(supreme-Γ Γ_1 (Name_1 : t_1 Γ_2)) (Name_1 : t_1 (supreme-Γ Γ_1 Γ_2))
                                      (side-condition (redex-match? crystal-lang+Γ (#f _) (term (in-Γ Γ_1 Name_1))))
                                      ]
  )

(define-metafunction crystal-lang+Γ
  supreme-Σ : Σ Σ -> Σ
  [(supreme-Σ · Σ) Σ]
  [(supreme-Σ Σ ·) Σ]
  [(supreme-Σ Σ_1 (r_1 : t_1 Σ_2)) (r_1 : (supreme-t t_1 t_2) (supreme-Σ (remove-Σ Σ_1 r_1) Σ_2))
                                      (side-condition (redex-match? crystal-lang+Γ (#t _) (term (in-Σ Σ_1 r_1))))
                                      (where (#t t_2) (in-Σ Σ_1 r_1))
                                      ]
  [(supreme-Σ Σ_1 (r_1 : t_1 Σ_2)) (r_1 : t_1 (supreme-Σ Σ_1 Σ_2))
                                      (side-condition (redex-match? crystal-lang+Γ (#f _) (term (in-Σ Σ_1 r_1))))
                                      ]
  )

(define-metafunction crystal-lang+Γ
  [(remove-Γ (Name_1 : t Γ) Name_1) Γ]
  [(remove-Γ (Name_1 : t Γ) Name_2) (Name_1 : t (remove-Γ Γ Name_2))]
  )
(define-metafunction crystal-lang+Γ
  [(remove-Σ (r_1 : t Σ) r_1) Σ]
  [(remove-Σ (r_1 : t Σ) r_2) (r_1 : t (remove-Σ Σ r_2))]
  )

(define-metafunction crystal-lang+Γ
  [(in-Γ (Name_1 : t_1 Γ) Name_1)
   (#t t_1)
   ]
  [(in-Γ · Name_1)
   (#f Nil)]
  [(in-Γ (Name : t Γ) Name_1)
   (in-Γ Γ Name_1)])

(define-metafunction crystal-lang+Γ
  [(in-Σ (r_1 : t_1 Σ) r_1)
   (#t t_1)
   ]
  [(in-Σ · r_1)
   (#f Nil)]
  [(in-Σ (r : t Σ) r_1)
   (in-Σ Σ r_1)])

(define-metafunction crystal-lang+Γ
  [(in-t (st_1 st_2 ...) st_1) #t]
  [(in-t () st_1) #f]
  [(in-t (st st_1 ...) st_2) (in-t (st_1 ...) st_2)]
  )

(define-metafunction crystal-lang+Γ
  [(typeof-Γ Γ Name_1)
   t (where (_ t) (in-Γ Γ Name_1))
   ])

(define-metafunction crystal-lang+Γ
  [(typeof-Σ Σ r_1)
   t (where (_ t) (in-Σ Σ r_1))
   ])


(provide (all-defined-out))
;(provide WF)

;(define (WF? P)
;  (not (null? (judgment-holds (WF · () ,P ())
;                              #t))))

;(define v? (redex-match? crystal-lang v))

;(define (reduces? P)
;  (not (null? (apply-reduction-relation
;               full-rel
;               (term (() : P))))))

;(define (progress-holds? P)
;  (if (WF? P)
;      (or (v? P)
;          (reduces? P))
;      #t))

;(redex-check crystal-lang P (progress-holds? (term P)))





