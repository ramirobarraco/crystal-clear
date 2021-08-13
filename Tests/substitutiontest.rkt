#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/Substitution.rkt")

(define (subst-exp-test-suite)
  ; expression without structure
  (test-equal (term (subst nil ((X (ref 1) ))))
              (term nil))
  
  (test-equal (term (subst (X + 1) ((X (ref 1)))))
              (term ((ref 1) + 1)))

  (test-equal (term (subst ((X + 1) (X + 1)) ((X (ref 1)))))
              (term (((ref 1) + 1) ((ref 1) + 1))))

  (test-equal (term (subst (bar = (X + 1)) ((bar (ref 1)) (X (ref 2)))))
              (term ((ref 1) = ((ref 2) + 1))))

  (test-equal (term (subst (bar = (X + 1)) ((bar (ref 1)) (X (ref 2)))))
              (term ((ref 1) = ((ref 2) + 1))))

  (test-equal (term (subst (if (bar == 1) then (bar = 2) else (bar = 2)) ((bar (ref 1)))))
              (term (if ((ref 1) == 1) then ((ref 1) = 2) else ((ref 1) = 2))))


  )
(subst-exp-test-suite)