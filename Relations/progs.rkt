#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         )
; TODOM: documentación!
(define progs-rel
  (reduction-relation
   crystal-lang
   #:domain P
   #:arrow -->P

   [-->P (if v then P_1 else P_2)
         P_1
         if-T
         (side-condition (not (is_false_cond? (term v))))]

   [-->P (if v then P_1 else P_2)
         P_2
         if-F

         (side-condition(is_false_cond? (term v)))]

   [-->P (v P_1)
         P_1
         1-e]
   
   [-->P (v P_1 P_2 P_3 ...)
         (P_1 P_2 P_3 ...)
         more-e]

   [-->P (while P_1 P_2)
         (if P_1 then (P_2 (while P_1 P_2)) else nil)
         while
         ]
   
   [-->P (v_1 binop v_2)
         (δ binop v_1 v_2)
         E-BinOp
         (side-condition (is_strictbinop? (term binop)))]
   
   ; logical conectives
   [-->P (v binop P)
         (δ binop v P)
         E-LogicOp
         (side-condition (is_shortbinop? (term binop)))]

   [-->P (- v)
         (δ - v)
         minus]

   [-->P (not v)
         (δ not v)
         negation]

   [-->P (isa? t v)
           (is-a? t v)
           ]
   


   ))
(provide progs-rel)