(define room-type '((0 "cellar")
                    (1 "mystic room")
                    (2 "hallway")
                    (3 "temple")
                    (4 "lobby")
                    (5 "hallway")
                    (6 "court")
                    (7 "pass")))


(define objects '((0 "a silver dagger")
                  (1 "a gold coin")))

(define key_objects '((0 "a sword")))

(define key_objectspic '((0 "./images/sword.png")))

(define look '(((directions) look) ((look) look) ((examine room) look)))

(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))

(define pick '(((get) pick ) ((pickup) pick) ((pick) pick)))

(define directions '(((south) direction) ((north) direction) ((west) direction) ((east) direction)))

;;the put name in here is just the name of the list
;;the name of the action is drop, so we can change the list name to drop
;;to avoid confusion. i'll leave it this way to remember in the future
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))

(define inventory '(((inventory) inventory) ((bag) inventory)))

(define mazemap '(((map) mazemap) ((show map) mazemap)((see map) mazemap) ((look map) mazemap)))

(define actions `(,@look ,@quit ,@pick ,@put ,@inventory,@directions,@mazemap))

(define decisiontable `((1 ,@actions)
                        (2 ((south) 1) ,@actions )
                        (3 ,@actions)))


