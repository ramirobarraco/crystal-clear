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
  
  ;value calls
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) x Int32 (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) (ref 0) Int32 (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) 10 Int32 (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) true Bool (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) "asas" String (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) nil Nil (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  
  ;basic functions
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) (1 + 1) Int32 (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) (true and false) Bool (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) (- 1) Int32 (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) (- false) Bool (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Bool ·) ((ref 0) : Int32 ·) ((ref 0) = x) Bool (x : Bool ·) ((ref 0) : Bool ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) · (let x = true in ((ref 0) = x)) Bool (x : Int32 ·) ((ref 0) : Bool ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) · (x + 1) Int32 (x : Int32 ·)  ·)) #t)
  
  ;while
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) (while (x == 1) ((ref 0) = true))
                                  Bool
                                  (x : Int32 ·)
                                  ((ref 0) : (Bool Int32) ·)))
              #t)
  
  ;if
  (test-equal (judgment-holds (TR (x : Int32 ·) ((ref 0) : Int32 ·) (if (x == 1) then ((ref 0) = true) else ((ref 0) = "asaS"))
                                  (Bool String)
                                  (x : Int32 ·)
                                  ((ref 0) : (String Bool) ·)))
              #t)
  
  (test-equal (judgment-holds (TR (x : (Int32 String) ·) ((ref 0) : Int32 ·) (if (isa? Int32 x) then (x + 1) else ((ref 0) = "asaS"))
                                  (Int32 String)
                                  (x : (Int32 String) ·)
                                  ((ref 0) : (String Int32) ·)))
              #t)
  )

(supremes-test-suite)