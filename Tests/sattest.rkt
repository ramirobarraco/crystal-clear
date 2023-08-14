#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/sat.rkt"
         )

(define (sat-test-suite)
  ; complement of varsol
  ; complement of types
  (test-equal (term (comp-varsol ⊥))
              (term (Nil Bool Int32 String))
              )
  (test-equal (term (comp-varsol Nil))
              (term (Bool Int32 String))
              )
  (test-equal (term (comp-varsol Bool))
              (term (Nil Int32 String))
              )
  (test-equal (term (comp-varsol Int32))
              (term (Nil Bool String))
              )
  (test-equal (term (comp-varsol String))
              (term (Nil Bool Int32))
              )
  (test-equal (term (comp-varsol (Nil Bool)))
              (term (Int32 String))
              )
  (test-equal (term (comp-varsol (Nil Bool Int32)))
              (term String)
              )
  (test-equal (term (comp-varsol (Nil Bool Int32 String)))
              (term ⊥)
              )
  (test-equal (term (comp-varsol x))
              (term (not x))
              )
  (test-equal (term (comp-varsol (not x)))
              (term x)
              )
  
  ; complement of SOL
  (test-equal (term (comp-SOL ·))
              (term ·)
              )
  (test-equal (term (comp-SOL (x : Nil ·)))
              (term (x : (Bool Int32 String) ·))
              )
  (test-equal (term (comp-SOL (x : (Nil Int32) ·)))
              (term (x : (Bool String) ·))
              )
  (test-equal (term (comp-SOL (x : ⊥ ·)))
              (term (x : (Nil Bool Int32 String) ·))
              )

  ; supremum of varsol
  (test-equal (term (sup-varsol (String Bool) Int32))
              (term (Int32 String Bool))
              )
  (test-equal (term (sup-varsol Bool Int32))
              (term (Bool Int32))
              )
  (test-equal (term (sup-varsol Int32 (String Bool)))
              (term (Int32 String Bool))
              )
  (test-equal (term (sup-varsol (String Int32) Int32))
              (term (String Int32))
              )
  (test-equal (term (sup-varsol x Int32))
              (term (x ⊔ Int32))
              )
  (test-equal (term (sup-varsol (not x) Int32))
              (term ((not x) ⊔ Int32))
              )
  (test-equal (term (sup-varsol (String Int32) x))
              (term ((String Int32) ⊔ x))
              )
  (test-equal (term (sup-varsol (String Int32) (not x)))
              (term ((String Int32) ⊔ (not x)))
              )
  
  ; supremum of SOL
  (test-equal (term (sup-SOL · (x : Bool ·)))
              (term (x : (x ⊔ Bool) ·))
              )
  (test-equal (term (sup-SOL (x : String ·) (x : Bool ·)))
              (term (x : (String Bool) ·))
              )
  (test-equal (term (sup-SOL (x : String ·) (y : Bool ·)))
              (term (x : (String ⊔ x) (y : (Bool ⊔ y) ·)))
              )
  (test-equal (term (sup-SOL (x : String (y : Bool ·)) (z : Int32 ·)))
              (term (x : (String ⊔ x) (z : (Int32 ⊔ z) (y : (Bool ⊔ y) ·))))
              )
  (test-equal (term (sup-SOL (x : String ·) (x : y ·)))
              (term (x : (String ⊔ y) ·))
              )
  (test-equal (term (sup-SOL (x : String ·) (x : (not y) ·)))
              (term (x : (String ⊔ (not y)) ·))
              )

  ; remove-SOL
  (test-equal (term (remove-SOL · y))
              (term ·)
              )
  (test-equal (term (remove-SOL (x : String ·) y))
              (term (x : String ·))
              )
  (test-equal (term (remove-SOL (x : String ·) x))
              (term ·)
              )
  (test-equal (term (remove-SOL (x : String (y : String (z : String ·))) y))
              (term (x : String (z : String ·)))
              )

  ; in-SOL
  (test-equal (term (in-SOL (x : String ·) y))
              (term #f)
              )
  (test-equal (term (in-SOL (x : String (z : String ·)) y))
              (term #f)
              )
  (test-equal (term (in-SOL (x : String (z : String ·)) z))
              (term String)
              )
  (test-equal (term (in-SOL (x : String (y : String (z : String ·))) y))
              (term String)
              )
  (test-equal (term (in-SOL (x : String (y : String (z : String ·))) u))
              (term #f)
              )

  
  ; infimum of varsol
  (test-equal (term (inf-varsol (String Bool) Int32))
              (term ⊥)
              )
  (test-equal (term (inf-varsol Bool Int32))
              (term ⊥)
              )
  (test-equal (term (inf-varsol (String Int32) Int32))
              (term Int32)
              )
  (test-equal (term (inf-varsol x Int32))
              (term (x ⊓ Int32))
              )
  (test-equal (term (inf-varsol (not x) Int32))
              (term ((not x) ⊓ Int32))
              )
  (test-equal (term (inf-varsol (String Int32) x))
              (term ((String Int32) ⊓ x))
              )
  (test-equal (term (inf-varsol (String Int32) (not x)))
              (term ((String Int32) ⊓ (not x)))
              )

  
  ; infimum of SOL
  (test-equal (term (inf-SOL · (x : Bool ·)))
              (term (x : (x ⊓ Bool) ·))
              )
  (test-equal (term (inf-SOL (x : String ·) (x : Bool ·)))
              (term (x : ⊥ ·))
              )
  (test-equal (term (inf-SOL (x : String ·) (y : Bool ·)))
              (term (x : (String ⊓ x) (y : (Bool ⊓ y) ·)))
              )
  (test-equal (term (inf-SOL (x : String (y : Bool ·)) (z : Int32 ·)))
              (term (x : (String ⊓ x) (z : (Int32 ⊓ z) (y : (Bool ⊓ y) ·))))
              )
  (test-equal (term (inf-SOL (x : String ·) (x : y ·)))
              (term (x : (String ⊓ y) ·))
              )
  (test-equal (term (inf-SOL (x : String ·) (x : (not y) ·)))
              (term (x : (String ⊓ (not y)) ·))
              )

  ; instantiation of varsol
  (test-equal (term (inst-varsol x (x : Bool ·)))
              (term Bool)
              )

  (test-equal (term (inst-varsol Int32 (x : Bool ·)))
              (term Int32)
              )

  (test-equal (term (inst-varsol (Bool Int32) (x : Bool ·)))
              (term (Bool Int32))
              )

  (test-equal (term (inst-varsol (not x) (x : Bool ·)))
              (term (Nil Int32 String))
              )

  (test-equal (term (inst-varsol ((not x) ⊔ y) (x : (Nil Int32 String)
                                                  (y : String ·))))
              (term (Bool String))
              )

  (test-equal (term (inst-varsol ((not x) ⊓ y) (x : (Nil Int32 String)
                                                  (y : String ·))))
              (term ⊥)
              )

  ; instantiation of SOL
  (test-equal (term (inst-SOL (y : x ·) (x : Bool ·)))
              (term (y : Bool ·))
              )

  (test-equal (term (inst-SOL (y : Int32 ·) (x : Bool ·)))
              (term (y : Int32 ·))
              )

  (test-equal (term (inst-SOL (y : (Bool Int32) ·) (x : Bool ·)))
              (term (y : (Bool Int32) ·))
              )

  (test-equal (term (inst-SOL (y : (not x) ·) (x : Bool ·)))
              (term (y : (Nil Int32 String) ·))
              )

  (test-equal (term (inst-SOL (z : ((not x) ⊔ y) ·) (x : (Nil Int32 String)
                                                       (y : String ·))))
              (term (z : (Bool String) ·))
              )

  (test-equal (term (inst-SOL (z : ((not x) ⊓ y) ·) (x : (Nil Int32 String)
                                                       (y : String ·))))
              (term (z : ⊥ ·))
              )
  
  ;                                
  ;                                
  ;                                
  ;                                
  ;                                
  ;                                
  ;     ;;;;;      ;;     ;;;;;;;; 
  ;    ;;   ;      ;;        ;;    
  ;    ;          ;;;        ;;    
  ;    ;          ; ;;       ;;    
  ;    ;;         ;  ;       ;;    
  ;     ;;;      ;;  ;       ;;    
  ;      ;;;;    ;   ;;      ;;    
  ;        ;;    ;;;;;;      ;;    
  ;         ;   ;;    ;      ;;    
  ;         ;   ;;    ;;     ;;    
  ;   ;;   ;;   ;     ;;     ;;    
  ;   ;;;;;;   ;;      ;     ;;    
  ;                                
  ;                                
  ;                                
  ;                                
  ;                                

  ; constants
  (test-equal (judgment-holds (SAT nil ·)) #t)

  (test-equal (judgment-holds (SAT false ·)) #t)

  (test-equal (judgment-holds (SAT true ·)) #t)

  (test-equal (judgment-holds (SAT 1 ·)) #t)

  (test-equal (judgment-holds (SAT "asdf" ·)) #t)

  ; type reflection 
  (test-equal (judgment-holds (SAT (isa? Int32 x) (x : Int32 ·))) #t)
  (test-equal (judgment-holds (SAT (isa? (Int32 String) x)
                                   (x : (Int32 String) ·))) #t)

  ; logical operators
  ; or
  (test-equal (judgment-holds (SAT ((isa? Int32 x) or (isa? String x))
                                   (x : (Int32 String) ·))) #t)
  (test-equal (judgment-holds (SAT ((isa? (String Int32) x) or (isa? Bool x))
                                   (x : (Bool String Int32) ·))) #t)
  (test-equal (judgment-holds (SAT (((isa? Int32 x) or (isa? Bool x))
                                    or (isa? String x))
                                   (x : (String Int32 Bool) ·))) #t)
  (test-equal (judgment-holds (SAT (true or (isa? String x))
                                   (x : (x ⊔ String) ·))) #t)
  (test-equal (judgment-holds (SAT ((not true) or (isa? Bool x))
                                   (x : (x ⊔ Bool) ·))) #t)
  (test-equal (judgment-holds (SAT (false or (isa? String x))
                                   (x : (x ⊔ String) ·))) #t)
  

  ; and
  (test-equal (judgment-holds (SAT ((isa? Int32 x) and (isa? String x))
                                   (x : ⊥ ·))) #t)
  (test-equal (judgment-holds (SAT ((isa? (String Int32) x) and (isa? Bool x))
                                   (x : ⊥ ·))) #t)
  (test-equal (judgment-holds (SAT ((isa? (String Int32) x) and (isa? Int32 x))
                                   (x : Int32 ·))) #t)
  ; conjunction of a predicate with type reflection and a predicate without it,
  ; still results in type narrowing
  (test-equal (judgment-holds (SAT (true and (isa? Int32 x))
                                   (x : (x ⊓ Int32) ·))) #t)
  (test-equal (judgment-holds (SAT (false and (isa? Int32 x))
                                   (x : (x ⊓ Int32) ·))) #t)
  (test-equal (judgment-holds (SAT ((not true) and (isa? Bool x))
                                   (x : (x ⊓ Bool) ·))) #t)
  (test-equal (judgment-holds (SAT (((isa? String x) and (isa? Bool x))
                                    or (isa? Int32 x))
                                   (x : Int32 ·))) #t)
  (test-equal (judgment-holds (SAT (((isa? String x) and (isa? Bool x))
                                    or
                                    ((isa? String x) and (isa? Int32 x)))
                                   (x : ⊥ ·))) #t)
  ; NOTE: crystal compiler breaks here:
  ; x = 1
  ; if x = 1 && x.is_a?(Bool)
  ;	puts x + "1"
  ; else
  ;	puts "no"
  ; end
  ; ends with error
  ;   4 | puts x + "1"
  ;            ^
  ; Error: undefined method '+' for Bool
  (test-equal (judgment-holds (SAT ((x = 1) and (isa? Bool x))
                                   (x : (x ⊓ Bool) ·))) #t)
  ; TODO: este ejemplo podría mostrar que el sistema de tipos de
  ; crystal no es sound
  (test-equal (judgment-holds (SAT (x = (isa? Int32 x))
                                   (x : Int32 ·))) #t)
  (test-equal (judgment-holds (SAT ((isa? Int32 x) and (x = true))
                                   (x : (Int32 ⊓ x) ·))) #t)
  ; not
  (test-equal (judgment-holds (SAT (not (isa? Int32 x))
                                   (x : (Nil Bool String) ·))) #t)
  (test-equal (judgment-holds (SAT (not (isa? (Int32 Nil) x))
                                   (x : (Bool String) ·))) #t)
  ; de morgan
  (test-equal (judgment-holds (SAT (not ((isa? Nil x) or (isa? Int32 x)))
                                   (x : (Bool String) ·))) #t)
  (test-equal (judgment-holds (SAT (not ((isa? Nil x)
                                         or ((isa? Int32 x)
                                             or ((isa? Bool x)
                                                 or (isa? String x)))))
                                   (x : ⊥ ·))) #t)
  (test-equal (judgment-holds (SAT (not ((isa? Nil x) and (isa? Int32 x)))
                                   (x : (Nil Bool Int32 String) ·))) #t)

  ; assignment
  (test-equal (judgment-holds (SAT (x = (isa? Nil x))
                                   (x : Nil ·))) #t)

  (test-equal (judgment-holds (SAT ((y = 1) and (isa? Nil x))
                                   (x : (x ⊓ Nil) ·))) #t)

  (test-equal (judgment-holds (SAT ((x = 1) and (isa? Int32 x))
                                   (x : (x ⊓ Int32) ·))) #t)

  ; relops
  ; no restrictions
  (test-equal (judgment-holds (SAT (x == x) ·)) #t)
  ; testing some of the expressions for which it makes sense
  ; the comparison with a Name
  (test-equal (judgment-holds (SAT (x == 1) 
                                   (x : (x ⊓ 1) ·))) #t)
  (test-equal (judgment-holds (SAT (x == (1 + 1)) 
                                   (x : (x ⊓ (1 + 1)) ·))) #t)
  (test-equal (judgment-holds (SAT (x == (true or false))
                                   (x : (x ⊓ (true or false)) ·))) #t)
  (test-equal (judgment-holds (SAT (x == (not (1 + 1)))
                                   (x : (x ⊓ (not (1 + 1))) ·))) #t)
  
  (test-equal (judgment-holds (SAT (1 == x) 
                                   (x : (x ⊓ 1) ·))) #t)
  (test-equal (judgment-holds (SAT ((1 + 1) == x) 
                                   (x : (x ⊓ (1 + 1)) ·))) #t)
  (test-equal (judgment-holds (SAT ((true or false) == x)
                                   (x : (x ⊓ (true or false)) ·))) #t)
  (test-equal (judgment-holds (SAT ((not (1 + 1)) == x)
                                   (x : (x ⊓ (not (1 + 1))) ·))) #t)
  (test-equal (judgment-holds (SAT (x == y)
                                   (x : (x ⊓ y) (y : (y ⊓ x) ·)))) #t)
  (test-equal (judgment-holds (SAT ((x == y) and (isa? String x))
                                   (x : ((x ⊓ y) ⊓ String)
                                      (y : ((y ⊓ x) ⊓ y) ·)))) #t)
  (test-equal (judgment-holds (SAT (x < y)
                                   (x : (x ⊓ y) (y : (y ⊓ x) ·)))) #t)
  (test-equal (judgment-holds (SAT ((x < y) or (isa? String x))
                                   (x : ((x ⊓ y) ⊔ String)
                                      (y : ((y ⊓ x) ⊔ y) ·)))) #t)
  (test-equal (judgment-holds (SAT (x <= y)
                                   (x : (x ⊓ y) (y : (y ⊓ x) ·)))) #t)
  (test-equal (judgment-holds (SAT (x > y)
                                   (x : (x ⊓ y) (y : (y ⊓ x) ·)))) #t)
  (test-equal (judgment-holds (SAT (x >= y)
                                   (x : (x ⊓ y) (y : (y ⊓ x) ·)))) #t)
  (test-equal (judgment-holds (SAT (x < 1) 
                                   (x : (x ⊓ 1) ·))) #t)
  (test-equal (judgment-holds (SAT (x < (1 + 1)) 
                                   (x : (x ⊓ (1 + 1)) ·))) #t)
  (test-equal (judgment-holds (SAT (x < (true or false))
                                   (x : (x ⊓ (true or false)) ·))) #t)
  (test-equal (judgment-holds (SAT (x < (not (1 + 1)))
                                   (x : (x ⊓ (not (1 + 1))) ·))) #t)
  
  (test-results))
