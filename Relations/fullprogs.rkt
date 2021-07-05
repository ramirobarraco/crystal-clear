#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         "../Relations/progs.rkt"
         "../Relations/sigmaprogs.rkt"
         )
(define full-rel
  (reduction-relation
   crystal-lang
   #:domain (σ : P)
   #:arrow ↦

   (↦ (σ : (in-hole E P_1))
           (σ : (in-hole E P_2))
           (where (P_2) ,(apply-reduction-relation progs-rel (term P_1)))

           FWD-Pure)

   (↦ (σ_1 : (in-hole E P_1))
           (σ_2 : (in-hole E P_2))
           (where (σ_2 : P_2) ,(apply-reduction-relation σ-rel (term (σ_1 : P_1))))

           FWD-σ)
   ))

(provide full-rel)