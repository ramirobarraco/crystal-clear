#lang racket
(require redex
         racket/trace
         "../grammar.rkt"
         "../Relations/subtyping.rkt"
         )


(judgment-holds (subtype () ())) ;#t
(judgment-holds (subtype Int32 String)) ;#f
(judgment-holds (subtype (Int32 Bool) Int32)) ;#t
(judgment-holds (subtype (Int32 Bool) Bool)) ;#t
(judgment-holds (subtype () Bool)) ;#f
(judgment-holds (subtype (Int32 Bool String) (Int32 Bool))) ;#t
(judgment-holds (subtype (Int32 Bool) String)) ;#f
