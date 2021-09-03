#lang racket
(require redex
         "../grammar.rkt"
         "./progressdefs.rkt"
         )

(define (well-formed-test-suite)
  (test-equal
   (judgment-holds
    (WF (x ·)
        ()
        (1 + x)))
   #t)
  (test-equal
   (judgment-holds
    (WF ·
        ()
        (let x = 1 in (1 + x))))
   #t)
  )

(well-formed-test-suite)