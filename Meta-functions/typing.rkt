#lang racket

(require redex
         "../grammar.rkt"
         )

; to ease type checking
(define-metafunction crystal-lang
  typeof-v : v -> t
  [(typeof-v int32) Int32]
  [(typeof-v str) String]
  [(typeof-v bool) Bool]
  [(typeof-v nil) Nil]
  )

; supreme between types
(define-metafunction crystal-lang+Γ
  supreme-t : t t -> t
  [(supreme-t ⊥ t) t]
  [(supreme-t t ⊥) t]
  [(supreme-t st_1 st_1) st_1]
  [(supreme-t st_1 st_2) (st_1 st_2)]
  [(supreme-t st_1 (st_2 ...)) (st_2 ...) (side-condition (term (in-t (st_2 ...) st_1))) ]
  [(supreme-t st_1 (st_2 ...)) (st_1 st_2 ...) (side-condition (not (term (in-t (st_2 ...) st_1)))) ]
  [(supreme-t (st_1 ...) st_2) (supreme-t st_2 (st_1 ...))]
  [(supreme-t (st_1 ...) (st_2 ...)) t_3 (where t_3 ,(remove-duplicates (append (term (st_1 ...)) (term (st_2 ...)))))]
  )

; infimum between types
(define-metafunction crystal-lang+Γ
  inf-t : t t -> t
  [(inf-t ⊥ t) ⊥]
  [(inf-t t ⊥) ⊥]
  [(inf-t st_1 st_1) st_1]
  [(inf-t st_1 st_2) ⊥]
  [(inf-t st_1 (st_2 ...)) st_1
                           (side-condition (term (in-t (st_2 ...) st_1))) ]
  [(inf-t st_1 (st_2 ...)) ⊥
                           (side-condition (not (term (in-t (st_2 ...) st_1)))) ]
  [(inf-t (st_1 ...) st_2) (inf-t st_2 (st_1 ...))]
  ; several base cases, to solve everything here and avoid defining auxiliary
  ; functions
  [(inf-t (st_1 st_2) (st_3 st_4 st_5 ...)) t_3
                                            
                                            (where t_1 (inf-t st_1
                                                              (st_3 st_4 st_5 ...)))
                                            (where t_2 (inf-t st_2
                                                              (st_3 st_4 st_5 ...)))
                                            (where t_3 (supreme-t t_1 t_2))]

  [(inf-t (st_1 st_2 st_3 ...) (st_4 st_5)) t_3
                                            
                                            (where t_1 (inf-t (st_1 st_2 st_3 ...)
                                                              st_4))
                                            (where t_2 (inf-t (st_1 st_2 st_3 ...)
                                                              st_5))
                                            (where t_3 (supreme-t t_1 t_2))]
  
  [(inf-t (st_1 st_2 st_3 st_4 ...) (st_5 st_6 st_7 st_8 ...)) t_3
   (where t_1 (inf-t st_1 (st_5 st_6 st_7 st_8 ...)))
   (where t_2 (inf-t (st_2 st_3 st_4 ...) (st_5 st_6 st_7 st_8 ...)))
   (where t_3 (supreme-t t_1 t_2))]
  )

; complement of types
; TODO: determinar si es un nombre adecuado
(define-metafunction crystal-lang+Γ
  comp-t : t -> t

  ; several bases cases to avoid resorting to racket
  ; code
  [(comp-t ⊥) (Nil Bool Int32 String)]

  [(comp-t Nil) (Bool Int32 String) ]

  [(comp-t Bool) (Nil Int32 String)]

  [(comp-t Int32) (Nil Bool String)]

  [(comp-t String) (Nil Bool Int32)]

  [(comp-t (st_1 st_2)) (inf-t (comp-t st_1) (comp-t st_2))]

  [(comp-t (st_1 st_2 st_3 st_4 ...)) (inf-t (comp-t st_1)
                                             (comp-t (st_2 st_3 st_4 ...)))]
  )

(define-metafunction crystal-lang+Γ
  equal-t : t t -> boolean
  [(equal-t t_1 t_2) ,(and (term (upper_bound? t_1 t_2)) (term (upper_bound? t_2 t_1)))] 
  )

(define-metafunction crystal-lang+Γ
  supreme-Γ : Γ Γ -> Γ

  ; TODO: check this
  [(supreme-Γ · Γ) Γ]
  [(supreme-Γ Γ ·) Γ]
  ;[(supreme-Γ · ·) ·]
  ; TODO: creo que no es necesario tomar supremo
  ;  [(supreme-Γ · (Name_1 : t_1 Γ_1)) (Name_1 : (supreme-t t_1 Nil) (supreme-Γ · Γ_1))]
  ;  [(supreme-Γ (Name_1 : t_1 Γ_1) ·) (Name_1 : (supreme-t t_1 Nil) (supreme-Γ Γ_1 ·))]
  [(supreme-Γ (Name_1 : t_1 Γ_1) (Name_1 : t_2 Γ_2))
   (Name_1 : (supreme-t t_1 t_2) (supreme-Γ Γ_1  Γ_2))]
  [(supreme-Γ (Name_1 : t_1 Γ_1) (Name_2 : t_2 Γ_2))
   (Name_1 : (supreme-t t_1 t_3) (Name_2 : (supreme-t t_2 t_4) (supreme-Γ (remove-Γ Γ_1 Name_2) (remove-Γ Γ_2 Name_1))))
   (where (_ t_3) (in-Γ (Name_2 : t_2 Γ_2) Name_1))
   (where (_ t_4) (in-Γ (Name_1 : t_1 Γ_1) Name_2))
   ]
  )

; infimum of typing environments: the most restricted type assumptions
(define-metafunction crystal-lang+Γ
  inf-Γ : Γ Γ -> Γ

  ; · is no type restriction at all
  [(inf-Γ · Γ) Γ]
  [(inf-Γ Γ ·) Γ]
  [(inf-Γ (Name_1 : t_1 Γ_1) (Name_1 : t_2 Γ_2))
   (Name_1 : (inf-t t_1 t_2) (inf-Γ Γ_1  Γ_2))]
  [(inf-Γ (Name_1 : t_1 Γ_1) (Name_2 : t_2 Γ_2))
   (Name_1 : (inf-t t_1 t_3) (Name_2 : (inf-t t_2 t_4) (inf-Γ (remove-Γ Γ_1 Name_2) (remove-Γ Γ_2 Name_1))))
   (where (_ t_3) (in-Γ (Name_2 : t_2 Γ_2) Name_1))
   (where (_ t_4) (in-Γ (Name_1 : t_1 Γ_1) Name_2))
   ]
  )

; removes from the first t the second t
(define-metafunction crystal-lang+Γ
  remove-t : t t -> t
  [(remove-t t_1 t_1) Nil]
  [(remove-t Nil t_1) Nil]
  [(remove-t (st_1 st_2) st_2) st_1]
  [(remove-t (st_1 st_2) st_1) st_2]
  [(remove-t (st_1 st_2 ...) st_1) (st_2 ...)]
  [(remove-t (st_1 st_2 ...) st_3) t_1 (where t_1 ,(remove (term st_3) (term (st_1 st_2 ...))))]
  [(remove-t (st_1 st_2 ...) (st_3 st_4 ...))(remove-t t_1 (st_4 ...)) (where t_1 ,(remove (term st_3) (term (st_1 st_2 ...))))]
  )

(define-metafunction crystal-lang+Γ
  [(remove-Γ · Name) ·]
  [(remove-Γ (Name_1 : t Γ) Name_1) Γ]
  [(remove-Γ (Name_1 : t Γ) Name_2) (Name_1 : t (remove-Γ Γ Name_2))]
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
  [(in-t (st_1 st_2 ...) st_1) #t]
  [(in-t () st_1) #f]
  [(in-t (st st_1 ...) st_2) (in-t (st_1 ...) st_2)]
  )

(define-metafunction crystal-lang+Γ
  [(typeof-Γ Γ Name_1)
   t (where (_ t) (in-Γ Γ Name_1))
   ])
(provide (all-defined-out))