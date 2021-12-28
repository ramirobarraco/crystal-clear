#lang racket

(require redex
         "../grammar.rkt"
         )

; TODOM: documentación!
(define-metafunction crystal-lang
  [(name-in? () Name_1) #f]
  [(name-in? ((Name_1 r_1) (Name_2 r_2) ...) Name_1) #t]
  ; TODOM: acá tranquilamente podés colocar el lado derecho de la ecuación en
  ; la linea de abajo, para no pasarte de las 80 columnas
  [(name-in? ((Name_1 r_1) (Name_2 r_2) ...) Name_3) (name-in?  ((Name_2 r_2) ...) Name_3)])

(define-metafunction crystal-lang
  [(r-in-σ? () r_1) #f]
  ; TODOM: estas ecuaciones no hacen falta
  [(r-in-σ? ((r_1 v_1)) r_1) #t]
  [(r-in-σ? ((r_2 v_1)) r_1) #f]
  [(r-in-σ? ((r_1 v_1) (r_2 v_2) ...) r_1) #t]
  [(r-in-σ? ((r_1 v_1) (r_2 v_2) ...) r_3) (r-in-σ?  ((r_2 v_2) ...) r_3)])

(define-metafunction crystal-lang
  [(r-in-ϵ? () r_1) #f]
  ; TODOM: mismo comentario que en función anterior
  [(r-in-ϵ? ((Name_1 r_1)) r_1) #t]
  [(r-in-ϵ? ((Name_1 r_2)) r_1) #f]
  [(r-in-ϵ? ((Name_1 r_1) (Name_2 r_2) ...) r_1) #t]
  [(r-in-ϵ? ((Name_1 r_1) (Name_2 r_2) ...) r_3) (r-in-ϵ?  ((Name_2 r_2) ...) r_3)])

(define-metafunction crystal-lang
  typeof-v : v -> t
  [(typeof-v int32) Int32]
  [(typeof-v str) String]
  [(typeof-v bool) Bool]
  [(typeof-v nil) Nil]
  )
(provide (all-defined-out))