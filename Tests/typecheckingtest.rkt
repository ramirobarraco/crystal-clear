#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/typingrelation.rkt"
         "../Meta-functions/aux_fun.rkt"
         )

(define (inf-sup-test-suite)
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
  (test-equal (term (supreme-Γ (x : (String Bool) ·) (x : String ·)))
              (term (x : (String Bool) ·))
              )

  ; inf of types
  (test-equal (term (inf-t String Int32))
              (term ⊥)
              )

  (test-equal (term (inf-t String Bool))
              (term ⊥)
              )

  (test-equal (term (inf-t String Nil))
              (term ⊥)
              )

  (test-equal (term (inf-t String String))
              (term String)
              )

  (test-equal (term (inf-t String (Int32 Bool)))
              (term ⊥)
              )

  (test-equal (term (inf-t String (Int32 String)))
              (term String)
              )

  (test-equal (term (inf-t String (Int32 Bool Nil)))
              (term ⊥)
              )

  (test-equal (term (inf-t String (Int32 Bool String)))
              (term String)
              )

  (test-equal (term (inf-t (String Bool) (Int32 Bool)))
              (term Bool)
              )

  (test-equal (term (inf-t (String Bool Int32) (Int32 Bool)))
              (term (Int32 Bool))
              )

  (test-equal (term (inf-t (String Bool Int32) (Int32 Bool String)))
              (term (String Bool Int32))
              )
  
  (test-results))
  
(provide inf-sup-test-suite)

; complement of types
(define (comp-test-suite)
  (test-equal (term (comp-t ⊥))
              (term (Nil Bool Int32 String))
              )
  (test-equal (term (comp-t Nil))
              (term (Bool Int32 String))
              )
  (test-equal (term (comp-t Bool))
              (term (Nil Int32 String))
              )
  (test-equal (term (comp-t Int32))
              (term (Nil Bool String))
              )
  (test-equal (term (comp-t String))
              (term (Nil Bool Int32))
              )
  (test-equal (term (comp-t (Nil Bool)))
              (term (Int32 String))
              )
  (test-equal (term (comp-t (Nil Bool Int32)))
              (term String)
              )
  (test-equal (term (comp-t (Nil Bool Int32 String)))
              (term ⊥)
              )

  (test-results))

(provide comp-test-suite)

(define (inf-sup-contexts-test-suite)
  ; infimum of typing contexts
  (test-equal (term (inf-Γ · ·))
              (term ·)
              )

  (test-equal (term (inf-Γ (x : Bool ·) ·))
              (term ·)
              )

  (test-equal (term (inf-Γ · (x : Bool ·)))
              (term ·)
              )

  (test-equal (term (inf-Γ (x : Int32 ·) (y : Bool ·)))
              (term (x : ⊥ (y : ⊥ ·)))
              )

  (test-equal (term (inf-Γ (x : Int32 ·) (x : Bool ·)))
              (term (x : ⊥ ·))
              )

  (test-equal (term (inf-Γ (x : Int32 ·) (x : (Int32 Bool) ·)))
              (term (x : Int32 ·))
              )

  (test-equal (term (inf-Γ (x : Int32 (y : Int32 ·)) (x : Bool (y : Int32 ·))))
              (term (x : ⊥ (y : Int32 ·)))
              )

  (test-equal (term (inf-Γ (x : Int32 (y : Int32 (z : Int32 ·)))
                           (x : Bool (z : Int32 (y : Int32 ·)))))
              (term (x : ⊥ (y : Int32 (z : Int32 ·))))
              )

  ; supreme of typing context
  (test-equal (term (supreme-Γ · ·))
              (term ·)
              )

  (test-equal (term (supreme-Γ (x : Bool ·) ·))
              (term (x : Bool ·))
              )

  (test-equal (term (supreme-Γ · (x : Bool ·)))
              (term (x : Bool ·))
              )
  ; TODO: no estoy seguro de que el resultado debiera ser este
  (test-equal (term (supreme-Γ (x : Int32 ·) (y : Bool ·)))
              (term (x : (Int32 Nil) (y : (Bool Nil) ·)))
              )

  (test-equal (term (supreme-Γ (x : Int32 ·) (x : Bool ·)))
              (term (x : (Int32 Bool) ·))
              )

  (test-equal (term (supreme-Γ (x : Int32 ·) (x : (Int32 Bool) ·)))
              (term (x : (Int32 Bool) ·))
              )

  (test-equal (term (supreme-Γ (x : Int32 (y : Int32 ·)) (x : Bool (y : Int32 ·))))
              (term (x : (Int32 Bool) (y : Int32 ·)))
              )

  (test-equal (term (supreme-Γ (x : Int32 (y : Int32 (z : Int32 ·)))
                               (x : Bool (z : Int32 (y : Int32 ·)))))
              (term (x : (Int32 Bool) (y : Int32 (z : Int32 ·))))
              )

  (test-results))

(provide inf-sup-contexts-test-suite)

(define (type-checking-test-suite)
  ;Concat judgment form
  ; TODO: where should we put this test? are more tests needed?
  (test-equal (judgment-holds (concat (x : Int32 ·) (nil nil nil) nil Nil (x : Int32 ·))) #t)
  
  ; values
  (test-equal (judgment-holds (TR (x : Int32 ·) x Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR · 10 Int32 ·)) #t)
  (test-equal (judgment-holds (TR · true Bool ·)) #t)
  (test-equal (judgment-holds (TR · "asas" String ·)) #t)
  (test-equal (judgment-holds (TR · nil Nil ·)) #t)

  ; binops
  (test-equal (judgment-holds (TR · (1 + 1) Int32 ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x + 1) Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 + x) Int32 (x : Int32 ·))) #t)
  

  (test-equal (judgment-holds (TR · (1 - 1) Int32 ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x - 1) Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 - x) Int32 (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · (1 * 1) Int32 ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x * 1) Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 * x) Int32 (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · (1 / 1) Int32 ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x / 1) Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 / x) Int32 (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · (1 ^ 1) Int32 ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x ^ 1) Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 ^ x) Int32 (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · (1 % 1) Int32 ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x % 1) Int32 (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 % x) Int32 (x : Int32 ·))) #t)

  ; relop
  (test-equal (judgment-holds (TR · (1 < 1) Bool ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x < 1) Bool (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 < x) Bool (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · (1 <= 1) Bool ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x <= 1) Bool (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 <= x) Bool (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · (1 > 1) Bool ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x > 1) Bool (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 > x) Bool (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · (1 >= 1) Bool ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x >= 1) Bool (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 >= x) Bool (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · (1 == 1) Bool ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (x == 1) Bool (x : Int32 ·))) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (1 == x) Bool (x : Int32 ·))) #t)

  (test-equal (judgment-holds (TR · ("1" == "asd") Bool ·)) #t)
  (test-equal (judgment-holds (TR (x : String ·) (x == "1") Bool (x : String ·))) #t)
  (test-equal (judgment-holds (TR (x : String ·) ("1" == x) Bool (x : String ·))) #t)

  ; boolean op
  (test-equal (judgment-holds (TR · (true and false) Bool ·)) #t)
  (test-equal (judgment-holds (TR · (1 and true) (Int32 Bool) ·)) #t)
  (test-equal (judgment-holds (TR · ("asd" and 1) (String Int32) ·)) #t)
  (test-equal (judgment-holds (TR (x : String ·) (1 and x) (Int32 String) (x : String ·))) #t)

  (test-equal (judgment-holds (TR · (true or false) Bool ·)) #t)
  (test-equal (judgment-holds (TR · (1 or true) (Int32 Bool) ·)) #t)
  (test-equal (judgment-holds (TR · ("asd" or 1) (String Int32) ·)) #t)
  (test-equal (judgment-holds (TR (x : String ·) (1 or x) (Int32 String) (x : String ·))) #t)

  ; unop
  (test-equal (judgment-holds (TR · (- 1) Int32 ·)) #t)
  (test-equal (judgment-holds (TR (x : Int32 ·) (- x) Int32 (x : Int32 ·))) #t)
  
  (test-equal (judgment-holds (TR · (not 1) Bool ·)) #t)
  (test-equal (judgment-holds (TR · (not false) Bool ·)) #t)
  (test-equal (judgment-holds (TR · (not "asf") Bool ·)) #t)
  (test-equal (judgment-holds (TR · (not nil) Bool ·)) #t)

  ; assignment
  (test-equal (judgment-holds (TR (x : Bool ·) (a = x) Bool (a : Bool (x : Bool ·)))) #t)
  
  ;while
  ; TODO: solve while!
  ;  (test-equal (judgment-holds (TR (x : Int32 ·) (while (x == 1) (a = true))
  ;                                  Bool
  ;                                  (x : Int32 (a : (Bool Nil) ·))))
  ;              #t)
  ;
  
  ; if
  (test-equal (judgment-holds (TR (x : Int32 ·)
                                  (if (x == 1) then (a = true) else (a = "asaS"))
                                  (Bool String)
                                  (a : (Bool String) (x : Int32 ·))))
              #t)
  
  (test-equal (judgment-holds (TR (x : (String Int32) ·)
                                  (if (isa? Int32 x) then (x + 1) else (a = "asaS"))
                                  (Int32 String)
                                  (x : (Int32 String) (a : (String Nil) ·))))
              #t)

  ; example based on crystal docs
  ; b = true ? 1 : "hello"
  ; puts (typeof (b))
  ;
  ; if b.is_a?(Int32)
  ;   b = "hello" 
  ; else
  ;   puts (b .. "1")
  ; end
  (test-equal (judgment-holds (TR (a : (Int32 String) ·)
                                  (if (isa? String a)
                                      then
                                      (a = (a .. "asdf"))
                                      else
                                      (a = (a + 1)))
                                  (String Int32)
                                  (a : (String Int32) ·)))
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
                                  (if (b > 0)
                                       then
                                       (b = "hello")
                                       else
                                       nil)
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
                                       nil)d)
                                  (Int32 Nil)
                                  (d : (Int32 Nil) ·)))
              #t)
  
  (test-equal (judgment-holds (TR (x : Int32 ·) 
                                  ((if (a = (isa? Int32 x))
                                       then
                                       a
                                       else
                                       nil)a)
                                  Bool
                                  (a : Bool (x : Int32 ·))))
              #t)
  
  (test-results)
  )

(provide type-checking-test-suite)
