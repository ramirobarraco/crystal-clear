#lang racket
(require redex
         "../grammar.rkt"
         )

(define-metafunction ext-lang
  ; arithmetic operations
  ; coercion
  [(δbasic binop v_1 v_2)
   (δbasic binop Number_1 Number_2)
   (where Number_1 (δbasic v_1 nil))
   (where Number_2 (δbasic v_2 nil))]
  
  [(δbasic + Number_1 Number_2)
   ,(+ (term Number_3) (term Number_4))
   
   (where Number_3 , (term Number_1))
   (where Number_4 , (term Number_2))]

  [(δbasic - Number_1 Number_2)
   ,(- (term Number_3) (term Number_4))
   
   (where Number_3 , (term Number_1))
   (where Number_4 , (term Number_2))]
  
  [(δbasic * Number_1 Number_2)
   ,(* (term Number_3) (term Number_4))
   
   (where Number_3 ,(term Number_1))
   (where Number_4 ,(term Number_2))]
 
  [(δbasic / Number_1 Number_2)
   ,(/ (term Number_3) (term Number_4))
   (where Number_3 ,(term Number_1))
   (where Number_4 ,(term Number_2))
  ]
)