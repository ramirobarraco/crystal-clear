#lang racket
; TODOM: mínimo comentario describiendo qué hay en este módulo
(require redex)
(define-language crystal-lang
  
  (P ::=
     (P P P ...)
     e
     )
  (e ::=
     v
     var
     (isa? t var)
     (unop P)
     (P binop P)
     (while P P)
     (if P then P else P)
     (var = P)
     (t var = P)
     )
  
; next instruction to compute
  
  (E ::=
     (unop E)
     (E binop P)
     (v strictbinop E)
     (E P P ...)
     (isa? t E)
     (if E then P else P)
     (var = E)
     (t var = E)
     hole
     )
  
; Context
;  (Ev ::=
;     (unop hole)
;     (hole binop P)
;     (v strictbinop hole)
;     (hole P P ...)
;     (if hole then P else P)
;     (var = hole)
;     (t var = hole)
;     (isa? t hole)
;     hole
;       )

  (var ::=
   Name
   ;r
   )
  
  ;Type definition
  [v nil bool int32 str]
  
  ;last type is the union of types
  [t Nil Bool Int32 String union ⊥]
  [st Nil Bool Int32 String]
  [union (st_!_ st_!_ st_!_ ...)]

  [bool true false]

  [int32 integer]

  [str string]

  [binop shortbinop strictbinop]
  
  ; Not short-circuit binop
  [strictbinop
        ;arith
        + - * / ^ % 
        ; relop 
        < <= > >= ==
        ]
  
  [shortbinop and or]

  [unop - not
        ;typeof
        ]
  
  ;r is a reference
  [r (ref natural)]
  ;reference pair
  [rp (r v)]
  ;reference name
  [rn (Name r)]

  [ϵ ((Name_!_1 r) ...)]
  [σ ((r_!_1 v) ...)]
  [σϵprog (σ : ϵ : P)]

  ; we will use this symbol later on the type relation Γ
  [reserved-symbol ·]
  ; Name can be anything except a keyword of the language
  [Name variable-not-otherwise-mentioned]
  )
(provide crystal-lang)

(define-extended-language crystal-lang+Γ crystal-lang
  [Γ · (Name : t Γ)]
  ; SAT solutions
  ; possible type of a variable
  [varsol t
          ; TODO: estoy admitiendo cosas que no corresponde
          (varsol_!_ varsol_!_ varsol_!_ ...)
          ; to allow for the analysis of relops over just
          ; Names
          Name
          (not varsol)
          (varsol ⊔ varsol)
          (varsol ⊓ varsol)]
  ; TODO: symbol? otro nombre?
  ; solutions for every variable
  [SOL · ; the more relaxed solution possible: does not
         ; restrict the type of variables: ∀ x, ·(x) = x 
       (Name : varsol SOL)
       
       ;T
       ]
  
  [arithop + - * / ^ %]
  [relop < <= > >= ==]
  )

(provide crystal-lang+Γ)

(define is_int32?
  (redex-match? crystal-lang
                int32))

(define is_string?
  (redex-match? crystal-lang
                string))

(define is_nil?
  (redex-match? crystal-lang
                nil))

(define is_bool?
  (redex-match? crystal-lang
                bool))

(define is_false?
  (redex-match? crystal-lang
                false))

(define is_shortbinop?
  (redex-match? crystal-lang
                shortbinop))

(define is_strictbinop?
  (redex-match? crystal-lang
                strictbinop))

(define (is_false_cond? P)
  (or (is_false? P)
      (is_nil? P)))

(define-metafunction crystal-lang
  is-a? : t v -> bool
  [(is-a? Int32 int32) true]
  [(is-a? Bool bool) true]
  [(is-a? String str) true]
  [(is-a? Nil nil) true]
  [(is-a? _ _) false]
  )



(provide (all-defined-out))