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

(let ([c (make-coverage progs-rel)])
    (parameterize ([relation-coverage (list c)])
      (redex-check crystal-lang σϵprog (safety?  (term σϵprog)) #:prepare prepare  #:attempts 10000 #:source full-rel))
    (covered-cases c))
