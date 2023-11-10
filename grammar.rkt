#lang racket
; TODOM: mínimo comentario describiendo qué hay en este módulo
(require redex)
(define-language crystal-lang
  
  (P ::= (P P P ...) e)
  
  (e ::= v
         var
         (isa? t P)
         (unop P)
         (P binop P)
         (while P P)
         (if P P else P)
         (var = P)
         )
  
; next instruction to compute
  
  (E ::= (unop E)
         (E binop P)
         (v strictbinop E)
         (E P P ...)
         (isa? t E)
         (if E P else P)
         (var = E)
         hole
         )

  (var ::= Name)
  
  [v nil bool int32 str]

  ; types
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
        ; string
        ..
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
  
  ; expression describing the possible type of a variable
  [varsol t
          ; to allow for the analysis of relops over 
          ; Names and arbitrary terms
          P
          (not varsol)
          (varsol ⊔ varsol)
          (varsol ⊓ varsol)
          ; to express the restrictions imposed by a guard of the
          ; form Name == P: it restricts the type of Name in the 'if' branch,
          ; but not in the 'else' branch
          (varsol △ varsol)]
  
  ; solutions for every variable
  [SOL · ; the most relaxed solution possible: does not
         ; restrict the type of variables: ∀ x, ·(x) = x 
       (Name : varsol SOL)]

  ; some syntactic categories to ease the definition of typing rules
  [arithop + - * / ^ %]
  [relop < <= > >=]
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