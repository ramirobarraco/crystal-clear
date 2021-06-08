#lang racket
(require redex
         "../grammar.rkt"
         )

(define-metafunction crystal-lang
  subst : P ((Name r) ...) -> P
  [(subst (P_1 P_2 P_3 ...) ((Name r) ...) )
   ((subst P_1 ((Name r) ...)) (subst P_2 ((Name r) ...))(subst P_3 ((Name r) ...)) ...)]
  
  [(subst (var = P) ((Name r)...))
    ((subst var ((Name r)...)) = (subst P ((Name r)...))) ]

  
)
