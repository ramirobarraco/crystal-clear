#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt")


(define (full-progs-rel-test-suite)
  ; full "while" loop
  (test-->> full-rel
            (term (((r 3) )
                   : (while (1 < r)
                            (r = (r - 1)))))
            
            (term ((r 1)
                   : \;))))