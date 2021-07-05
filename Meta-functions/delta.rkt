#lang racket
(require redex
         "../grammar.rkt"
         )

(define-metafunction crystal-lang
  ; arithmetic operations
  ; coercion
  
  [(δ + int32_1 int32_2)
   ,(+ (term int32_1) (term int32_2))]

  [(δ - int32_1 int32_2)
   ,(- (term int32_1) (term int32_2))]
  
  [(δ * int32_1 int32_2)
   ,(* (term int32_1) (term int32_2))]
 
  [(δ / int32_1 int32_2)
   ,(/ (term int32_1) (term int32_2))]
  
  [(δ == v_1 v_2)
   ,(eq? (term v_1)(term v_2))]
  
  [(δ < v_1 v_2)
   ,(< (term v_1)(term v_2))]
  
  [(δ >= v_1 v_2)
   ,(>= (term v_1)(term v_2))]
  
  [(δ <= v_1 v_2)
   ,(<= (term v_1)(term v_2))]
  
  [(δ > v_1 v_2)
   ,(> (term v_1)(term v_2))]
  
  [(δ and v_1 v_2)
   ,(and (term v_1)(term v_2))]
  
  [(δ or v_1 v_2)
   ,(or (term v_1)(term v_2))]
  
)
(provide δ)