(module ix.monad (parse:ixM parse:flat-ixM #;parse:json->ixM ix:unwrapM ix:buildM ix:validateM ix:validate-asM)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.condition)

(import tabulae.monad)
(import (prefix ix.base ix:))
(import (prefix ix.parse parse:))
(import (prefix ix.build ix:))
(import ix.lens)

(define (monadify f) (lambda (#!rest args)
  (to-maybe (condition-case (apply f args) (() 'Nothing)))))

(define parse:ixM (monadify parse:ix))
(define parse:flat-ixM (monadify parse:flat-ix))
;(define parse:json->ixM (monadify parse:json->ix))
(define ix:unwrapM (monadify ix:unwrap))
(define ix:buildM (monadify ix:build))
(define ix:validateM (monadify ix:validate))
(define ix:validate-asM (monadify ix:validate-as))

)
