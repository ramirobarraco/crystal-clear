#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         )
(define full-rel
  (reduction-relation
   crystal-lang
   #:domain (σ:P)
   #:arrow ↦

   (↦ (σ:(in-hole E P_1))
           (σ:(in-hole E P_2))
           (where(P_2),(apply-reduction-relation progs-rel(term P_1)))

           FWD-Pure)
   ))