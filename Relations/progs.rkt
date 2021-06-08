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
   
   [-->P (\; P)
         P]

   [-->P (while v P_1)
         (if v then P_1 \; while v P_1 else \;)
         ]
   
   [-->P (v_1 binop v_2)
           (δbasic binop v_1 v_2)
           E-BinOp
           ; apply binop, check if the operation was successful
           (side-condition (is_strictbinop? (term binop)))]
   
   ; logical conectives
   [-->P (v binop P)
           (δbasic binop v P)
           E-LogicOp
        
           (side-condition (is_shortbinop? (term binop)))]

   ))
(provide progs-rel)