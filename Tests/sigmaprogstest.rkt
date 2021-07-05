#lang racket
(require redex
         racket/trace
         "../grammar.rkt"
         "../Relations/sigmaprogs.rkt"
         "../Relations/progs.rkt"
         )

(define (sigma-progs-test-suite)
  (test--> σ-rel (term ((((ref 1) 1))  :
                                       ((ref 1) + 1)))
              (term ((((ref 1) 1))
                   : (1 + 1))))
  
  (test--> σ-rel(term ((((ref 1) 3))
                         :
                         (while ((ref 1) < 1 ) ((ref 1) = ((ref 1) - 1)))))
            
            (term ((((ref 1) 1))
                   : \;)))
  
  )

(sigma-progs-test-suite)