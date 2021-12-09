#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         "../Relations/sigmaprogs.rkt")


(define (full-progs-rel-test-suite)
  ; full "while" loop
  (test-->> full-rel
            (term ((((ref 1) 3) ):
                   (((ref 1) x))  :
                                 (while (1 < (ref 1))
                                        ((ref 1) = ((ref 1) - 1))
                                        )))
            
            (term ((((ref 1) 1)):
                   (((ref 1) x))  :
                    \;)))
  
  (test-->> full-rel
            (term ((((ref 1) false) ):
                  (((ref 1) x))  :
                                     (if (ref 1)
                                         then ((ref 1) = false) else ((ref 1) = true))))
            
            (term ((((ref 1) true)):
                   (((ref 1) x))  :
                    \;)))
  
  (test-->> full-rel
            (term ((((ref 1) false) ):
                   (((ref 1) x))  :
                    (1 1)))
            
            (term ((((ref 1) false)):
                   (((ref 1) x))  :
                    1)))
  
  (test-->> full-rel
            (term ((((ref 1) false) ):
                                     (((ref 1) x))  :
                                     (isa? Int32 x)))
            
            (term ((((ref 1) false)):
                                    (((ref 1) x))  :
                                    false)))
  )


(full-progs-rel-test-suite)