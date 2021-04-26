#lang racket

(require redex)

(define-language ext-lang
  (P ::=
     Name
     Name = v
     (while P P end)
     (if0 P then P else P)
     (strictbinop P P)
     (unop p)
     )
  (E ::=(v ...E e ...)
     []
     )
  ;Type definition
  [v Nil Boolean Number Str Union]
  [t nil bool int32 string union]

  [Boolean true false]

  [Number integer]

  [Str string]

  [Union (union Type ...)]

  ; primitive operators
  [arithop + - * / ^ %]

  [relop < <= > >=]

  ; Not short-circuit binop
  [strictbinop arithop relop == ..]

  [binop strictbinop and or]

  [unop - not typeof]

  [σ ((r v) ...)]
  ; Name can be anything except a keyword of the language
  [Name variable-not-otherwise-mentioned]
  )
(provide ext-lang)

(define is_number?
  (redex-match? ext-lang
                Number))

(define is_string?
  (redex-match? ext-lang
                Str))

(define is_nil?
  (redex-match? ext-lang
                nil))

(define is_bool?
  (redex-match? ext-lang
                Boolean))
