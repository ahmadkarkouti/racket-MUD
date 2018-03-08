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

;This notation allows us to trace our recursion after constructing into an empty list '()
;(require racket/trace)
;(trace multirember)
(multirember 'W '(R E C U R S I O N W))

;Examples to test the program
;-----------------------------
;(multirember 'and '(in cdr but not in car we cons car then rember and cdr))
;(multirember 'in '(in car we cons cdr))