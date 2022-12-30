#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         "../Relations/sigmaprogs.rkt")


(define (full-progs-rel-test-suite)
  ; full "while" loop
  (test-->> full-rel
            (term ((((ref 1) 3) ):
                   ((x (ref 1)))  :
                                 (while (1 < x)
                                        (x = (x - 1))
                                        )))
            
            (term ((((ref 1) 1)):
                   ((x (ref 1)))  :
                    nil)))
  
  (test-->> full-rel
            (term ((((ref 1) false) ):
                  ((x (ref 1)))  :
                                     (if x
                                         then (x = false) else (x = true))))
            
            (term ((((ref 1) true)):
                   ((x (ref 1)))  :
                    nil)))
  
  (test-->> full-rel
            (term ((((ref 1) false) ):
                   ((x (ref 1)))  :
                    (1 1)))
            
            (term ((((ref 1) false)):
                   ((x (ref 1)))  :
                    1)))
  
  (test-->> full-rel
            (term ((((ref 1) false) ):
                                     ((x (ref 1)))  :
                                     (isa? Int32 x)))
            
            (term ((((ref 1) false)):
                                    ((x (ref 1)))  :
                                    false)))

    (test--> full-rel (term ((((ref 1) 1))  :
                                        ((x (ref 1)))  :
                                        (x + 1)))
           (term ((((ref 1) 1)):
                               ((x (ref 1))): (1 + 1))))
  
  (test--> full-rel (term ((((ref 1) 3))  :
                                        ((x (ref 1)))  :
                                        (x < 1 )))
            
           (term ((((ref 1) 3)):
                               ((x (ref 1))): (3 < 1))))
  )


  (test--> full-rel (term ((((ref 1) 3))  :
                                        ((x (ref 1)))  :
                                        (isa? Int32 x)))
            
           (term ((((ref 1) 3)):
                               ((x (ref 1))): (isa? Int32 3)))
           )


(full-progs-rel-test-suite)