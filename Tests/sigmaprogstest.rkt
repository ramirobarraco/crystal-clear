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

  
  (test--> σϵ-rel (term ((((ref 0) 1)):
                                      ((bar (ref 0))):
                                      bar))
            
           (term ((((ref 0) 1)):
                               ((bar (ref 0))): 1)))

  (test--> σϵ-rel (term ((((ref 0) 1)):
                                      ((bar (ref 0))):
                                      (x = 0)))
            
           (term ((((ref 0) 1) ((ref 1) 0)):
                                           ((x (ref 1)) (bar (ref 0))): nil)))
 
  (test--> σϵ-rel (term ((((ref 0) nil)) : ((b (ref 0)) (a (ref 0))) : a))
           
           (term ((((ref 0) nil)) : ((b (ref 0)) (a (ref 0))) : nil))             
           )

  (test--> σϵ-rel (term ((((ref 0) nil)) : ((b (ref 0)) (a (ref 0))) : (a = nil)))
       
           (term ((((ref 0) nil)) : ((b (ref 0)) (a (ref 0))) : nil))            
           )
  
  (test--> σϵ-rel (term ((((ref 0) nil)) : ((b (ref 0)) (a (ref 0))) : ( Nil a = nil)))
           
           (term ((((ref 0) nil)) : ((b (ref 0)) (a (ref 0))) : nil))
           )

  (test--> σϵ-rel (term (() : () : (x = 0)))
         
           (term ((((ref 0) 0)): ((x (ref 0))): nil))
           )

  (test-results)
  )
  
(sigma-progs-test-suite)