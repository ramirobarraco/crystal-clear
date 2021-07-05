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
                   : \;))))

(full-progs-rel-test-suite)