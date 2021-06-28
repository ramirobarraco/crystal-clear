#lang racket
(require redex
         "../grammar.rkt"
         )

(define-metafunction crystal-lang
  subst : P ((Name r) ...) -> P
  [(subst (P_1 P_2 P_3 ...) ((Name r) ...) )
   ((subst P_1 ((Name r) ...)) (subst P_2 ((Name r) ...))(subst P_3 ((Name r) ...)) ...)]
  
  [(subst (var = P) ((Name r)...))
    ((subst var ((Name r)...)) = (subst P ((Name r)...)))]

  [(subst (if P_1 then P_2 else P_3) ((Name r)...))
   (if (subst P_1 ((Name r)...)) then (subst P_2 ((Name r) ...)) else (subst P_3 ((Name r) ...)))
   ]

  [(subst (while P_1 P_2) ((Name r)...))
   (while (subst P_1 ((Name r) ...)) (subst P_2 ((Name r) ...)))
   ]

  [(subst (P_1 binop P_2) ((Name r)...))
   ((subst P_1 ((Name r) ...)) binop (subst P_2 ((Name r) ...)))
   ]

  [(subst (unop P))
   (unop (subst P))
   ]

  [(subst Name_1 ((Name_1 r_1) (Name r) ...))
   (r_1)
   ]

  [(subst Name_1 ((Name_2 r_1) (Name r) ...))
   (subst Name_1 ((Name r) ...))
   ]
  
  [(subst r ())
   r
   ]
  
)
