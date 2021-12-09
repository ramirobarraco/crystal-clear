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
  (test--> σϵ-rel (term ((((ref 1) 1))  :
                         (((ref 1) x))  :
                                       ((ref 1) + 1)))
              (term ((((ref 1) 1)):
                     (((ref 1) x)): (1 + 1))))
  
  (test--> σϵ-rel (term ((((ref 1) 3))  :
                         (((ref 1) x))  :
                         ((ref 1) < 1 )))
            
            (term ((((ref 1) 3)):
                   (((ref 1) x)): (3 < 1))))
  
  (test--> σϵ-rel (term ((((ref 0) 1)):
                         (((ref 0) bar)):
                         bar))
            
            (term ((((ref 0) 1)):
                   (((ref 0) bar)): (ref 0))))
 


  )
  
(sigma-progs-test-suite)