#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullprogs.rkt"
         "../Relations/subtyping.rkt"
         "../Meta-functions/aux_fun.rkt"
         "../Relations/sigmaprogs.rkt"
         "../Relations/progs.rkt"
         "progressdefs.rkt"
         )

;(redex-check crystal-lang+Γ σϵprog (progress-holds?  (term σϵprog)) #:prepare prepare  #:attempts 1000)
;(redex-check crystal-lang+Γ σϵprog (reduce1  (term σϵprog)) #:prepare prepare  #:attempts 1000)
(redex-check crystal-lang+Γ σϵprog (preservation-holds?  (term σϵprog)) #:prepare prepare  #:attempts 1000)


;(define (progress_tr rel attempts debug)
;  (redex-check  crystal-lang any
;                (soundness_wfc_pred (term any) debug)
;                #:prepare close_conf
;                #:attempts attempts
;                #:source rel))