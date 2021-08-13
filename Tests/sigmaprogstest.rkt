#lang racket
(require redex
         racket/trace
         "../grammar.rkt"
         "../Relations/sigmaprogs.rkt"
         "../Relations/progs.rkt"
         "../Meta-functions/init.rkt"
         "../Meta-functions/Substitution.rkt"
         )

(define (sigma-progs-test-suite)
  (test--> σ-rel (term ((((ref 1) 1))  :
                                       ((ref 1) + 1)))
              (term ((((ref 1) 1))
                   : (1 + 1))))
  
  (test--> σ-rel (term ((((ref 1) 3))
                         :
                         ((ref 1) < 1 )))
            
            (term ((((ref 1) 3))
                   : (3 < 1))))
  
  (test--> σ-rel (term (()
                         :
                         (let bar = 1 in (bar + 1))))
            
            (term ((((ref 1) 1))
                   : \;)))
  
  )

(sigma-progs-test-suite)