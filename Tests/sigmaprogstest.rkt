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
            
            (term ((((ref 0) 1))
                   : ((ref 0) + 1))))
  
  (test--> σ-rel (term ((((ref 0) true) )
                        :
                        (isa? Bool (ref 0))))
            
           (term ((((ref 0) true) )
                        :
                        true)))

  (test--> σ-rel (term ((((ref 0) true) )
                        :
                        (isa? Int32 (ref 0))))
            
           (term ((((ref 0) true) )
                        :
                        false)))


  )
  
(sigma-progs-test-suite)