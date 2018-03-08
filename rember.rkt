#lang racket

;REMBER
;--------
;--------
(define rember ;This is defining the main function rember
  (lambda (a lat);Lambda is an anonymous function
    
    (cond ;Condition
      
      ((null? lat) (quote ())) ;Check if lat is null, then output an empty list
      ((eq? ( car lat) a) ( cdr lat)) ;check if (car lat) = a , output cdr lat

      ;Else construct a new list with the first atom of lat
      ;And call rember to the cdr of lat
      (else ( cons ( car lat) 
                   (rember a ( cdr lat)))))))


;Examples to test the program
;-----------------------------
;(rember 'and '(in cdr but not in car we cons car then rember and cdr))
;(rember 'in '(in car we cons cdr))






