#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         )
(define σ-rel
  (reduction-relation
   crystal-lang
   #:domain (σ:P)
   #:arrow -->σ/P
   
   ))
   