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
  
  [(δ - int32_1)
   ,(- (term int32_1))]
  
  [(δ * int32_1 int32_2)
   ,(* (term int32_1) (term int32_2))]
 
  [(δ / int32_1 int32_2)
   ,(/ (term int32_1) (term int32_2))]
  
  [(δ == v_1 v_2)
   (toBool ,(eq? (term v_1)(term v_2)))]
  
  [(δ < v_1 v_2)
   (toBool ,(< (term v_1)(term v_2)))]
  
  [(δ >= v_1 v_2)
   (toBool ,(>= (term v_1)(term v_2)))]
  
  [(δ <= v_1 v_2)
   (toBool ,(<= (term v_1)(term v_2)))]
  
  [(δ > v_1 v_2)
   (toBool ,(> (term v_1)(term v_2)))]
  
  [(δ and v P)
   v
   (side-condition (is_false_cond? (term v)))]

  [(δ and v P) 
   P
   (side-condition (not (is_false_cond? (term v))))]
  
  [(δ or v P)
   v
   (side-condition (not (is_false_cond? (term v))))]
  
  [(δ or v P)
   P
   (side-condition (is_false_cond? (term v)))]
  
  [(δ not v)
   true

   (side-condition (is_false_cond? (term v)))]
  
  [(δ not v) 
   false]
  
)
(provide δ)

(define-metafunction crystal-lang
  
  [(toBool #t)
   true]
  
  [(toBool #f)
   false])