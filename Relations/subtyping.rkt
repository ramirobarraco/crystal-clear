#lang racket
(require redex
         "../grammar.rkt"
         )
; TODOM: documentación
(define-relation crystal-lang
  subtype ⊆ t × t
  ; TODOM: notar que está al revés de como normalmente se ordenan los
  ; tipos involucrados en un predicado de subtyping: t1 <: t2 si t1 es
  ; subtipo de t2
  [(subtype t ())]
  [(subtype t_1 t_2)
   (side-condition (term
                    (upper_bound? t_1 t_2)))
   ]
  )
; given t_1 t_2 returns true if t_1 is an upper bound of t_2
(define-metafunction crystal-lang
  upper_bound? : t t -> boolean
  ; TODOM: sugiero que los casos base sean union types de 2 tipos,
  ; ya que unions de 0 tipos o de 1 tipo no tienen sentido, más allá
  ; de que la gramática lo permita
  [(upper_bound? t ()) #true]
  [(upper_bound? st_1 st_1) #true]
  [(upper_bound? st_1 st_2) #false]
  [(upper_bound? () (st_1 st_2 ...)) #false]
  [(upper_bound? () st_1) #false]
  ; TODOM: indentación, espacios
  [(upper_bound? (st_1 ...) (st_2 st_3 ...)) ,(and (term(upper_bound? (st_1 ...) (st_3 ...))) (term(in st_2 (st_1 ...))))]
  [(upper_bound? (st_1 st_2 ...) st_3) ,(term (in st_3 (st_1 st_2 ...)))]
  )

(define-metafunction crystal-lang
  in : st t -> boolean
  [(in st_1 (st_2 ... st_1 st_3 ...)) #true]
  [(in st_1 (st_2 ...)) #false])

(provide subtype)
(provide upper_bound?)