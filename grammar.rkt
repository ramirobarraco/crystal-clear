#lang racket

(require redex)

(define-language ext-lang
  (P ::=
     (v)
     (while P P end)
     (if0 P then P else P)
     (strictbinop P P)
     
     )
  ;Type definition
  [v Nil Boolean Number Str Union]
  [t nil bool int32 string union]

  [Boolean true false]
  
  [Number real]
  
  [Str string]

  [Union (union Type ...)]
  
  ; primitive operators
  [arithop + - * / ^ %]
  
  [relop < <= > >=]

  ; Not short-circuit binop
  [strictbinop arithop relop == ..]
  
  [binop strictbinop and or]
  
  [unop - not \#]
  
  ; Name can be anything except a keyword of the language
  [Name variable-not-otherwise-mentioned]
  
  
  
  )
(provide ext-lang)