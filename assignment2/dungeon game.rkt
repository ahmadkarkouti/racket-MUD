#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

(define objects '((1 "a steel sword")
                  (1 "a loaf of bread")
                  (1 "a torch")))

(define places '((1 "You are in the the Hallway")
                 (2 "You are in the Chapel")
                 (3 "You are in the Armory")
                 (4 "You are in the Summoning room")))

(define examine '(((directions) look) ((look) look) ((examine room) look)))
(define exit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define actions '(,@examine ,@exit))
