#lang racket

(require redex
         "../grammar.rkt"
         )

(define-metafunction crystal-lang
  [(name-in? () Name_1) #f]
  [(name-in? ((Name_1 r_1) (Name_2 r_2) ...) Name_1) #t]
  [(name-in? ((Name_1 r_1) (Name_2 r_2) ...) Name_3) (name-in?  ((Name_2 r_2) ...) Name_3)])

(define-metafunction crystal-lang
  [(r-in-σ? () r_1) #f]
  [(r-in-σ? ((r_1 v_1) (r_2 v_2) ...) r_1) #t]
  [(r-in-σ? ((r_1 v_1) (r_2 v_2) ...) r_3) (r-in-σ?  ((r_2 v_2) ...) r_3)])

(define-metafunction crystal-lang
  [(r-in-ϵ? () r_1) #f]
  [(r-in-ϵ? ((Name_1 r_1) (Name_2 r_2) ...) r_1) #t]
  [(r-in-ϵ? ((Name_1 r_1) (Name_2 r_2) ...) r_3) (r-in-ϵ?  ((Name_2 r_2) ...) r_3)])

(provide (all-defined-out))