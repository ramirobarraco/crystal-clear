#lang racket
(require redex
         "../grammar.rkt"
         )
(define-relation crystal-lang
  subtype ⊆ t × t
  [(subtype t ())]
  [(subtype t t)]
  [(subtype t t)
   (side-condition (term
                    (supreme? t t)))
   ]
  )
(define-metafunction crystal-lang
  supreme? : t t -> boolean
  [(supreme? t ()) #true]
  [(supreme? st_1 st_1) #true]
  [(supreme? st_1 st_2) #false]
  [(supreme? () (st_1 st_2 ...)) #false]
  [(supreme? (st_1 ...) (st_2 st_3 ...)) ,(and (term(supreme? (st_1 ...) (st_3 ...))) (term(in st_2 (st_1 ...))))]
  )
(define-metafunction crystal-lang
  in : st t -> boolean
  [(in st_1 ()) #false]
  [(in st_1 (st_2 ... st_1 st_3 ...)) #true]
  [(in st_1 (st_2 ...)) #false])