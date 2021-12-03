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
  
  ;Concat judgment form
  (test-equal (judgment-holds (concat (x : Int32 ·) ((ref 0) : Int32 ·) (\; \; \;) \; Nil (x : Int32 ·) ((ref 0) : Int32 ·))) #t)
  
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
  
;a = 1
;if some_condition
;  a = "hello"
;else
;  a = true
;end
;# a : String | Bool
  (test-equal (judgment-holds (TR ·  ·
                                  (let a = 1 in
                                    ((if (a > 0)
                                         then
                                         (a = "hello")
                                         else
                                         (a = true)) a))
                                  (Bool String)
                                  ·
                                  ·))
              #t)

;b = 1
;if some_condition
;  b = "hello"
;end
;# b : Int32 | String
  (test-equal (judgment-holds (TR ·  ·
                                  (let b = 1 in
                                    ((if (b > 0)
                                         then
                                         (b = "hello")
                                         else
                                         \;) b))
                                  (Int32 String)
                                  ·
                                  ·))
              #t)
  
;if some_condition
;  c = 1
;else
;  c = "hello"
;end
;# c : Int32 | String
  (test-equal (judgment-holds (TR ·  ((ref  0) : Int32 ·)
                                    ((if (1 == 1)
                                         then
                                         ((ref 0) = 1)
                                         else
                                         ((ref 0) = "hello")) (ref 0 ))
                                  (String Int32)
                                  ·
                                  ((ref 0) : (String Int32) ·)))
              #t)

;if some_condition
;  d = 1
;end
;# d : Int32 | Nil
;divergence becouse of diference with how we modeled the enviroment
  (test-equal (judgment-holds (TR ·   ·
                                  ((if (1 == 1)
                                       then
                                       ((ref 0) = 1)
                                       else
                                       \;)(ref 0))
                                  (Int32 Nil)
                                  ·
                                  ((ref 0) : (Int32 Nil) ·)))
              #t)
  
  (test-equal (judgment-holds (TR ·   ·
                                  ((if ((ref 0) = (isa? Int32 (ref 0)))
                                       then
                                       (ref 0)
                                       else
                                       \;)(ref 0))
                                  Bool
                                  ·
                                  ((ref 0) : Bool ·)))
              #t)
  
  (test-results)
  )

(supremes-test-suite)
