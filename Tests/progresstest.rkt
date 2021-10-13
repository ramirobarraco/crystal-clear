#lang racket
(require redex
         "../grammar.rkt"
         "./progressdefs.rkt"
         )

(define (well-formed-test-suite)
  (test-equal
   (judgment-holds
    (WF (x : Int32 ·)
        (((ref 1) 3))
        (1 + (ref 1))
        (Int32)))
   #t)
  (test-equal
   (judgment-holds
    (WF ·
        ()
        (let x = 1 in (1 + x))
        (Int32)))
   #t)
  )

(well-formed-test-suite)