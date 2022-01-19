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

;(redex-check crystal-lang+Γ σϵprog (progress-holds?  (term σϵprog)) #:prepare prepare  #:attempts 10000)
;(redex-check crystal-lang+Γ σϵprog (reduce1  (term σϵprog)) #:prepare prepare  #:attempts 10000)
;(redex-check crystal-lang+Γ σϵprog (preservation-holds?  (term σϵprog)) #:prepare prepare  #:attempts 10000)
;(redex-check crystal-lang+Γ σϵprog (safety?  (term σϵprog)) #:prepare prepare  #:attempts 10000)

(let ([c (make-coverage full-rel)])
    (parameterize ([relation-coverage (list c)])
      (redex-check crystal-lang+Γ σϵprog (safety?  (term σϵprog)) #:prepare prepare  #:attempts 100000))
    (covered-cases c))
