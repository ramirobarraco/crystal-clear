#lang racket

(require redex)

(define-language crystal-lang
  (P ::=
     (P P P...)
     \;
     Name
     (Name = v)
     (while P P)
     (if P then P else P)
     (strictbinop P P)
     (unop P)
     v
     )
  (E ::=
     (unop E)
     (strictbinop E P)
     (E P P...)
     (if E then P else P)
     hole
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

(define (is_false_cond? t)
  (or (is_false? t)
      (is_nil? t)))

(provide (all-defined-out))