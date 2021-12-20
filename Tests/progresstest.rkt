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
  
  ;Concat judgment form
  (test-equal (judgment-holds (concat (x : Int32 ·) (\; \; \;) \; Unit (x : Int32 ·))) #t)
  
  ;value calls
  (test-equal (judgment-holds (TR (x : Int32 ·) x Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) 10 Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) true Bool (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) "asas" String (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) nil Nil (x : Int32 ·))) #t)
  
  ;basic functions
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 + 1) Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (true and false) Bool (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (- 1) Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·)(- false) Bool (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Bool ·) (a = x) Bool (a : Bool (x : Bool ·)))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x + 1) Int32 (x : Int32 ·))) #t)
  
  ;while
  (test-equal (judgment-holds (TR (x : Int32 ·) (while (x == 1) (a = true))
                                  Bool
                                  (x : Int32 (a : (Bool Nil) ·))))
              #t)
  
  ;if
  (test-equal (judgment-holds (TR (x : Int32 ·) (if (x == 1) then (a = true) else (a = "asaS"))
                                  (Bool String)
                                  (a : (Bool String) (x : Int32 ·))))
              #t)
  
  (test-equal (judgment-holds (TR (x : (Int32 String) ·) (if (isa? Int32 x) then (x + 1) else (a = "asaS"))
                                  (Int32 String)
                                  (x : (Int32 String) (a : (String Nil) ·))))
              #t)
  
  ;a = 1
  ;if some_condition
  ;  a = "hello"
  ;else
  ;  a = true
  ;end
  ;# a : String | Bool
  (test-equal (judgment-holds (TR (a : Int32 ·)
                                    ((if (a > 0)
                                         then
                                         (a = "hello")
                                         else
                                         (a = true)) a)
                                  (String Bool)
                                  (a : (String Bool) ·)))
              #t)

  ;b = 1
  ;if some_condition
  ;  b = "hello"
  ;end
  ;# b : Int32 | String
  (test-equal (judgment-holds (TR (b : Int32 ·)
                                    ((if (b > 0)
                                         then
                                         (b = "hello")
                                         else
                                         \;) b)
                                  (String Int32)
                                  (b : (String Int32) ·)))
              #t)
  
  ;if some_condition
  ;  c = 1
  ;else
  ;  c = "hello"
  ;end
  ;# c : Int32 | String
  (test-equal (judgment-holds (TR (c : Int32 ·)  
                                  ((if (c == 1)
                                       then
                                       (c = 1)
                                       else
                                       (c = "hello")) c)
                                  (Int32 String)
                                  (c : (Int32 String) ·)))
              #t)

  ;if some_condition
  ;  d = 1
  ;end
  ;# d : Int32 | Nil
  ;divergence becouse of diference with how we modeled the enviroment
  (test-equal (judgment-holds (TR ·
                                  ((if (1 == 1)
                                       then
                                       (d = 1)
                                       else
                                       \;)d)
                                  (Int32 Nil)
                                  (d : (Int32 Nil) ·)))
              #t)
  
  (test-equal (judgment-holds (TR ·
                                  ((if (a = (isa? Int32 (ref 0)))
                                       then
                                       a
                                       else
                                       \;)a)
                                  Bool
                                  (a : Bool ·)))
              #t)
  
  (test-results)
  )

(supremes-test-suite)
