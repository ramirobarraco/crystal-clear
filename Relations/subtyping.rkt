#lang racket
(require redex
         "../grammar.rkt"
         )
(define-relation crystal-lang
  subtype ⊆ t × t
  [(subtype t ())]
  [(subtype t_1 t_2)
   (side-condition (term
                    (upper_bound? t_1 t_2)))
   ]
  )
; given t_1 t_2 returns true if t_1 is an upper bound of t_2
(define-metafunction crystal-lang
  upper_bound? : t t -> boolean
  [(upper_bound? t ()) #true]
  [(upper_bound? st_1 st_1) #true]
  [(upper_bound? st_1 st_2) #false]
  [(upper_bound? () (st_1 st_2 ...)) #false]
  [(upper_bound? () st_1) #false]
  [(upper_bound? (st_1 ...) (st_2 st_3 ...)) ,(and (term(upper_bound? (st_1 ...) (st_3 ...))) (term(in st_2 (st_1 ...))))]
  [(upper_bound? (st_1 st_2 ...) st_3) ,(term (in st_3 (st_1 st_2 ...)))]
  )
(define-metafunction crystal-lang
  in : st t -> boolean
  [(in st_1 (st_2 ... st_1 st_3 ...)) #true]
  [(in st_1 (st_2 ...)) #false])

(provide subtype)