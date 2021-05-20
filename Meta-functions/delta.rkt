#lang racket
(require redex
         "../grammar.rkt"
         )

(define-metafunction crystal-lang
  ; arithmetic operations
  ; coercion
  
  [(δbasic + int32_1 int32_2)
   ,(+ (term int32_1) (term int32_2))]

  [(δbasic - int32_1 int32_2)
   ,(- (term int32_1) (term int32_2))]
  
  [(δbasic * int32_1 int32_2)
   ,(* (term int32_1) (term int32_2))]
 
  [(δbasic / int32_1 int32_2)
   ,(/ (term int32_1) (term int32_2))
  ]
  [(δbasic == v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
  [(δbasic < > v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
  [(δbasic >= v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
  [(δbasic <= v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
  [(δbasic > v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
  [(δbasic & v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
  [(δbasic \| v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
  [(δbasic and v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
  [(δbasic or v_1 v_2)
   ,(eq? (term v_1)(term v_2))
   ]
)