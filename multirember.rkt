#lang racket

;TASK 1: MULTIREMBER
;----------------------
;----------------------
(define multirember ;This is defining the function multirember
  (lambda (a lat)   ;Lambda is an anonymous function
    
    (cond           ;Condition
      ((null? lat) (quote ())) ;Check if lat is null
      (else
       (cond                   ;Condition
         ((eq? ( car lat) a)   ; if (car lat) = a

          ;if the first atom of the list == a
          ( multirember a ( cdr lat)))
          ;Jump back to multirember and remove the first atom of the list

         
         (else ( cons ( car lat)
          ;Else construct a new list with every atom that is not equal to a
                      ( multirember a
                                    ( cdr lat)))))))))

;Examples to test the program
;-----------------------------
;(multirember 'blue '(red blue white blue green blue black yellow blue))
;'(red white green black yellow)