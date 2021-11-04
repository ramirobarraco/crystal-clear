#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         )
(define progs-rel
  (reduction-relation
   crystal-lang
   #:domain P
   #:arrow -->P

   [-->P (if v then P_1 else P_2)
         P_1
         if-T

         (side-condition(not(is_false_cond? (term v))))]

   [-->P (if v then P_1 else P_2)
         P_2
         if-F

         (side-condition(is_false_cond? (term v)))]

   [-->P (\; P_1)
         P_1
         1-skip]

   [-->P (\; P_1 P_2 P_3 ...)
         (P_1 P_2 P_3 ...)
         more-skip]
   

   [-->P (while P_1 P_2)
         (if P_1 then (P_2 \; (while P_1 P_2)) else \;)
         ]
   
   [-->P (v_1 binop v_2)
           (δ binop v_1 v_2)
           E-BinOp
           ; apply binop, check if the operation was successful
           (side-condition (is_strictbinop? (term binop)))]
   
   ; logical conectives
   [-->P (v binop P)
           (δ binop v P)
           E-LogicOp
        
           (side-condition (is_shortbinop? (term binop)))]

   [-->P (- v)
         (δ - v)
         (side-condition (is_int32? (term v)))]

   [-->P (not v)
         (δ not v)
         (side-condition (is_bool? (term v)))]

   ))
(provide progs-rel)