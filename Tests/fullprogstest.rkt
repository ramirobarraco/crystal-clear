#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         "../Relations/sigmaprogs.rkt")


(define (full-progs-rel-test-suite)
  ; full "while" loop
  (test-->> full-rel
            (term ((((ref 1) 3) )
                   : (while (1 < (ref 1))
                            ((ref 1) = ((ref 1) - 1))
                            )))
            
            (term ((((ref 1) 1))
                   : \;)))
  
  (test-->> full-rel
            (term ((((ref 1) false) )
                   : (if (ref 1)
                         then ((ref 1) = false) else ((ref 1) = true))))
            
            (term ((((ref 1) true))
                   : \;)))
  )

(full-progs-rel-test-suite)