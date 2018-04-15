#lang racket

;; Dependencies
(require srfi/1)
(require srfi/13)
(require srfi/48)


;; Objects description
(define objects '((1 "a steel sword")))

;; Rooms description
(define descriptions '((1 "You are in the corridor.")
                       (2 "You are in the cellar.")
                       (3 "You are in a fightroom.")))

;; Initializes the object database
(define objectdb (make-hash))

;; Initializes the inventory database
(define inventorydb (make-hash))

;; Lists of pairs. First we have the user's entry and second we have what our software should understand with that entry
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define drop '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define help '(((help) help)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))



;; Lists using unquote-splicing to dynamically reference all the other lists
(define actions `(,@look ,@pick ,@drop ,@inventory ,@help ,@quit))
(define decisiontable `((1 ((north) 2) ,@actions)
                        (2 ((south) 1) ((north east) 3) ((north west) 4) ,@actions)
                        (3 ((west) 4) ((south west) 2) ((north east) 5) ,@actions)
                        (4 ((south east) 2) ((east) 3) ,@actions)
                        (5 ((south west) 3) ,@actions)))





;; <><>><><><<><><><><><><><<>><><><><||||| Functions ||||| ><><>><><><<><><><><><><><<>><><><> ;;

(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  (printf "> "))





