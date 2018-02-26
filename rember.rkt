#lang racket

;REMBER
;--------
;--------
(define rember ;This is defining the main function rember
  (lambda (a lat);Lambda is an anonymous function
    
    (cond ;Condition
      
      ((null? lat) (quote ())) ;Check if lat is null
      ((eq? ( car lat) a) ( cdr lat)) ;check if (car lat) = a = (cdr lat)

      ;Else construct a new list excluding the first atom that is equal to a
      (else ( cons ( car lat) 
                   (rember a ( cdr lat)))))))


;Examples to test the program
;-----------------------------
;(rember 'blue '(red blue white blue green blue black yellow blue))
;'(red white blue green blue black yellow blue)