#lang racket
(require redex
         "../grammar.rkt"
         )

(define-metafunction crystal-lang
  [(init ())
   0]
  
  [(init ((r_1 v_1) ... ))
   ,(+ (argmax max (term (r_1 ...))) 1)]
  )

(define-metafunction crystal-lang
  addVal : σ (v ...) -> (σ (r ...))
  ; Base case
  [(addVal ((r v) ...) ())
   (((r v) ...) ())]
  
  ; Inductive case
  [(addVal ((r v) ...) (v_1 v_2 ...))
   (σ (r_1 r ...))

   (where r_1 (init ((r v) ...)))
   (where (σ (r ...)) (addVal ((r v) ... (r_1 v_1)) (v_2 ...)))])

(provide addVal)