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
   
   [-->P (v_1 binop v_2)
           v_3

           E-BinOpNumber

           ; arith. or .. op, over numeric or string operands
           (side-condition (or (term (isArithBinOp binop))
                               (is_strconcat? (term binop))))

           ; apply binop, check if the operation was successful
           (where v_3 (δ binop v_1 v_2))

           ; we are assuming the soundness of δ: successful arithops only return
           ; numbers, successful concat only returns strings
           (side-condition (or (is_string? (term v_3))
                               (is_number? (term v_3))))]
   
   ; logical conectives
   [-->P (v binop e)
           (δ binop v e)
           E-LogicOp
        
           (side-condition (term (isBooleanBinOp binop)))]

   ))
