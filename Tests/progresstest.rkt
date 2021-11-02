#lang racket
(require redex
         "../grammar.rkt"
         "./progressdefs.rkt"
         )

(define (supremes-test-suite)
  (test-equal (term (supreme-t (String Bool) Int32))
              (term (Int32 String Bool))
              )
  (test-equal (term (supreme-t Bool Int32))
              (term (Bool Int32))
              )
  (test-equal (term (supreme-t Int32 (String Bool)))
              (term (Int32 String Bool))
              )
  (test-equal (term (supreme-t (String Int32) Int32))
              (term (String Int32))
              )
  (test-equal (term (supreme-Γ (x : (String Bool) ·) (x : (String) ·)))
              (term (x : (String Bool) ·))
              )
  (test-equal(term (supreme-Σ ((ref 0) : (String Bool) ·) ((ref 0) : (String) ·)))
             (term ((ref 0) : (String Bool) ·))
             )
  )
  

(supremes-test-suite)