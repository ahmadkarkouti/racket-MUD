#lang racket

(define multiinsertLR
(lambda (new oldL oldR lat)
(cond
( (null? lat) (quote ()))
((eq? ( car lat) oldL)
( cons new
( cons oldL
( multiinsertLR new oldL oldR
( cdr lat))))) 