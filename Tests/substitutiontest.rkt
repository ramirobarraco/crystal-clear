#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/Substitution.rkt"
         "../Relations/sigmaprogs.rkt")

(define (subst-exp-test-suite)
  ; expression without structure
  (test-equal (term (subst nil ((X (ref 1) )))) (term nil))
  (test-equal (term (subst (X + 1) ((X (ref 1))))) (term ((ref 1) + 1)))


  )
(subst-exp-test-suite)