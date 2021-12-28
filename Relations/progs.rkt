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
         ; TODOM: espacios
         (side-condition(not(is_false_cond? (term v))))]

   [-->P (if v then P_1 else P_2)
         P_2
         if-F

         (side-condition(is_false_cond? (term v)))]

   ; TODOM: esto ya no vale, cierto? ya no usás \; cierto?
   [-->P (\; P_1)
         P_1
         1-skip]

   [-->P (\; P_1 P_2 P_3 ...)
         (P_1 P_2 P_3 ...)
         more-skip]

   [-->P (v P_1)
         P_1
         1-e]
   
   [-->P (v P_1 P_2 P_3 ...)
         (P_1 P_2 P_3 ...)
         more-e]

   [-->P (while P_1 P_2)
         ; TODOM: idem caso concatenación de sentencias, esto te va a
         ; dar un error de dominio porque \; ya no es un P válido
         (if P_1 then (P_2 \; (while P_1 P_2)) else \;)
         ]
   
   [-->P (v_1 binop v_2)
           (δ binop v_1 v_2)
           E-BinOp
           ; TODOM: el comentario de abajo no tiene que ver con lo que
           ; se está haciendo, se me hace que viene del código de Lua
           ; apply binop, check if the operation was successful
           (side-condition (is_strictbinop? (term binop)))]
   
   ; logical conectives
   [-->P (v binop P)
           (δ binop v P)
           E-LogicOp
        
           (side-condition (is_shortbinop? (term binop)))]

   [-->P (- v)
         (δ - v)
         ; TODOM: no hace falta chequear esto, porque estamos asumiendo
         ; que lo que llega a ejecutarse está bien tipado
         (side-condition (is_int32? (term v)))]

   [-->P (not v)
         (δ not v)
         ; TODOM: idem comentario anterior
         (side-condition (is_bool? (term v)))]
   
   [-->P (isa? t v)
         (is-a? t v)
           ]

   ))
(provide progs-rel)