#lang racket
(require redex
         "../grammar.rkt"
         )

(define-metafunction crystal-lang
  init : σ -> r
  [(init ())
   (ref 0)]
  
  [(init (((ref number_1) v_1) ... ))
   (ref number_2)

   (where number_2 ,(+ (argmax max (term (number_1 ...))) 1))]
  )


(define-metafunction crystal-lang
  addVal : σ (v_1 ...) -> (σ (r_1 ...))
  ; Base case
  [(addVal ((r_1 v_1) ...) ())
   (((r_1 v_1) ...) ())]
  
  ; Inductive case
  [(addVal (rp ...) (v_1 v_2 ...))
   (σ (r_1 r ...))

   (where r_1 (init (rp ...)))
   (where (σ (r ...)) (addVal (rp ... (r_1 v_1)) (v_2 ...)))])

(provide addVal)