#lang racket

(require redex)

(define-language crystal-lang
  
  (P ::=
     (P P P...)
     \;
     var
     (var = P)
     (while P P)
     (P binop P)
     (if P then P else P)
     (unop P)
     v
     )
; next instruction to compute
  (E ::=
     (unop E)
     (E binop P)
     (v strictbinop E)
     (E P P ...)
     (if E then P else P)
     (while E P)
     (var = E)
     hole
     )

  (var ::=
   Name
   r
   )
; Context
  (Ev ::=
     (unop hole)
     (hole binop P)
     (v strictbinop hole)
     (hole P P ...)
     (if hole then P else P)
     (while hole P)
     (var = hole)
       )
  
  ;Type definition
  [v nil bool int32 str union]
  [t Nil Bool Int32 String Union]

  [bool true false]

  [int32 integer]

  [str string]

  [union (union Type ...)]

  [binop shortbinop strictbinop]
  
  ; Not short-circuit binop
  [strictbinop
        ;arith
        + - * / ^ % 
        ; relop 
        < <= > >= ==
        ]

  [shortbinop and or]

  [unop - not typeof]

  ;r is a reference
  [r (ref natural)]

  [σ ((r v) ...)]
  ; Name can be anything except a keyword of the language
  [Name variable-not-otherwise-mentioned]
  )
(provide crystal-lang)

(define is_number?
  (redex-match? crystal-lang
                Number))

(define is_string?
  (redex-match? crystal-lang
                Str))

(define is_nil?
  (redex-match? crystal-lang
                nil))

(define is_bool?
  (redex-match? crystal-lang
                Boolean))

(define is_false?
  (redex-match? crystal-lang
                false))

(define is_shortbinop?
  (redex-match? crystal-lang
                shortbinop))

(define is_strictbinop?
  (redex-match? crystal-lang
                strictbinop))

(define (is_false_cond? t)
  (or (is_false? t)
      (is_nil? t)))

(provide (all-defined-out))