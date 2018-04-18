#lang racket

(include "assoc.rkt")
(include "objects.rkt")
(include "maze.rkt")
(include "gui.rkt")

(require srfi/1)
(require srfi/13)
(require srfi/48)
(require racket/draw)
(require redex)


(define objectpicdb (make-hash))
;; DEFINITIONS OF DATABASES AND MAP

(define objectdb (make-hash))  ;;define object hash
(define inventorydb (make-hash)) ;;define bag hash
(define rooms (make-hash)) ;; define hash for carry the rooms names
(define roomspic (make-hash))
(define m (build-maze X Y)) ;;build the maze
(define gatekey "")
;; END OF DEFINITIONS

; THIS FUNCTION WILL DEFINE THE START POINT
(define (startpoint)
  (let*((start_x (random X))
        (start_y (random Y)))
  (list start_x start_y)))

;; refactored functions assq-ref and assv-ref into only one ass-ref
;; we pass what we want as parameter (assq or assv)
(define (ass-ref assqlist id x)
  (cdr (x id assqlist)))

;(random-allocator)
;randomly allocates something to a position in the maze
;a rate can be applied to allocate only to some cells (rooms)
;for instance: if the rate is 50, a room will have 50%
;chance of have a random item.
(define (random-allocator db types rate)
  (for ((j X))
    (for ((i Y))
      (cond ((<= (random 100) rate)
             (cond((equal? db rooms) ; add the name to the room
                   (hash-set! db (list j i) (car( ass-ref types (random (- (length types) 1)) assq))))
                  ;((equal? db roomspic) ; add the name to the room
                   ;(hash-set! db (list j i) (car( ass-ref types (random (- (length types) 1)) assq))))
                  (else ;add to objectdb
                   (add-object db (list j i) (car (ass-ref types (random (- (length types) 1)) assq))))))))))

(define useless 1)
(define (ranny)
  2)
(ranny)
(define randomy 3)


(define (ranny2)
  (random 1))
(define randomy2 ranny2)
;will place one unit of each type of key randomly on the maze
(define (random-key-location db types)
  (for ((i (length types)))
    (add-object db (list (ranny) (ranny)) (car (ass-ref types i assq)))))



;;get the keywords on association table
(define (get-keywords id)
  (let ((keys (ass-ref decisiontable id assq)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0) based on some weightening
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))


;return the index of the highest number on the list provided by the function
;(list-of-lenghts)
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))


;;Receive a function as parameters so it can be reused
;;this function can both get the actions and words attached to it
;;depending on the function passed
(define (call-actions id tokens func)
  (let* ((record (ass-ref decisiontable 1 assv)) ;;get the references
         (keylist (get-keywords 1)) ;;get the keywords
         ;;description in the functions
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) 
    (if index 
        (func (list-ref record index)) ;;return result if match, return false if dont
        #f)))


;;THIS FUNCTION WILL EVALUATE IF THE USER HAVE THE KEY NECESSARY TO OPEN THE GATE
(define (door-handle gatekey)
  (printf "You can see the exit gate, but it is locked. \n")
  (cond ((hash-has-key? inventorydb 'bag)
         (let* ((record (hash-ref inventorydb 'bag)) ;;get items list in bag
                (result (remove (lambda (x) (string-suffix-ci? gatekey x)) record)) ;;result = record - bag
                (item (lset-difference equal? record result))) ;; compare them
           (cond ((null? item) ;;if there is no difference, the key was removed, return true
             (cond
               ((not (hash-empty? inventorydb))
               #t)))
        (else
         #f))))))

(define newimage "")
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define resva 1)


;;START OF ALLOCATION OF ITENS AND ROOM NAMES
(random-allocator rooms room-type 100)
;(random-allocator roomspic room-type 100)

;(random-allocator objectdb objects 70)       ;;allocate items to the rooms
(random-key-location objectdb key_objects)   ;;allocate keys to the rooms
(random-key-location objectpicdb key_objectspic)
;;END OF ALLOCATION

;(define mynumber 0)
;; ADVANCED COMMAND LINE PROCESSOR WITH MAZE
(define (startgame-maze)
  (let* ((gatekey (car (ass-ref key_objects (random(length key_objects)) assq)))
         (gate_x 4)
         (gate_y 4)
         (gatekey "a gold coin")
         (start '(0 0)))
   ;;the following prints will help with testing, telling the developer where the gate is located and what key is the right one
;    (printf "~a \n" gate_x)
;    (printf "~a \n" useless)
;    (printf "~a \n" randomy)
;    (printf "~a \n" randomy)
;    (printf "~a \n" randomy)
;    (printf "~a \n" (randomy2))
;    (printf "~a \n" (randomy2))
;    (printf "~a \n" (randomy2))
;    (printf "~a \n" (randomy2))
;    (printf "~a \n" gate_y)
;    (printf "~a \n" gatekey)
;    (printf "~a \n " start)
    (let loop ((rid start))
      (cond
        ((equal? (hash-ref rooms rid) "cellar")
         (draws-sprite (picy:read-bitmap "./images/cellar.jpg") (pos 0 0)))
        ((equal? (hash-ref rooms rid) "mystic room")
         (draws-sprite (picy:read-bitmap "./images/mystic.jpg") (pos 0 0)))
        ((equal? (hash-ref rooms rid) "hallway")
         (draws-sprite (picy:read-bitmap "./images/hallway.jpg") (pos 0 0)))
        ((equal? (hash-ref rooms rid) "temple")
         (draws-sprite (picy:read-bitmap "./images/temple.jpg") (pos 0 0)))
        ((equal? (hash-ref rooms rid) "lobby")
         (draws-sprite (picy:read-bitmap "./images/lobby.png") (pos 0 0)))
        ((equal? (hash-ref rooms rid) "hallway")
         (draws-sprite (picy:read-bitmap "./images/hallway.jpg") (pos 0 0)))
        ((equal? (hash-ref rooms rid) "court")
         (draws-sprite (picy:read-bitmap "./images/court.png") (pos 0 0)))
        ((equal? (hash-ref rooms rid) "pass")
         (draws-sprite (picy:read-bitmap "./images/pass.jpg") (pos 0 0))))
      (display-objectspic objectpicdb rid)
      ;(printf "You are in the ~a \n>" (hash-ref rooms rid))
      (printf "You are in the ~a \n>" (hash-ref rooms rid))
      ;(printf "~a" (hash-ref roomspic rid))
      ;(set! newimage (picy:read-bitmap (hash-ref roomspic rid)))
      ;(draws-sprite (picy:read-bitmap (hash-ref rooms rid)) (pos 0 0))
      (cond
       ((eq? resva 1)
      (read-line)
      (set! resva 0)))
      (let* ((input (read-line))
             (string-tokens (string-tokenize input))
             (tokens (map string->symbol string-tokens))
             (response (call-actions rid tokens cadr))) ;;get action
        

      
        (cond ((eq? response 'direction)
               (let* ((direction (call-actions rid tokens caar)) ;get direction typed
                      (newlocation (move-room rid direction)))  ;get future location after move
                 (cond((member direction (paths rid)) ;check if direction is in path
                       (cond ((equal? newlocation (list gate_x gate_y)) ;end of game condition
                              (cond ((not (door-handle gatekey))
                                     (printf "It seems that you don't have the key to open the gate. \n")
                                     (loop newlocation))
                                    (else
                                     (printf "You used the key to open the gate. You are free! \n")
                                     (exit))))
                         (else
                          (loop newlocation))));;not in the gate
   
                      (else ;;direction not in path
                       (printf "You can not go that way!\n")
                       (loop rid)))))
            
              ((eq? #f response)
               (format #t "I am sorry, but I didn't understand that!\n")
               (loop rid)
               )
            
              ((eq? response 'look)
              (show-maze m rid)
               (display-objects objectdb rid)
               (loop rid))
              ((eq? response 'start)
               (loop rid))
              ((eq? response 'mazemap)
               (show-maze m rid)
              ;(display-objects objectdb rid)
               (loop rid))
              ((eq? response 'pick)
             ;remove item from room and put into inventory
               (handle-item 'room rid input)
               (pick-pic rid input)
               (loop rid))
            
              ((eq? response 'inventory)
               (display-inventory) ;;show inventorydb
               (loop rid))
            
              ((eq? response 'quit)
               (format #t "- Done by Ahmad Karkouti...\n")
               (draws-sprite exitpic (pos 0 0))
               ( let (( inputer ( read )))
                  ( cond [( eq? inputer 'restart ) (send (startgame-maze) start 100)])
                  ( cond [( eq? inputer 'quit ) ( exit )])))
               ;( exit ))
            
              ((eq? response 'drop)
               (drop-pic rid input)
               ;remove item from inventory and drop on the current room
               (drop-item rid input)
               ;(handle-item 'bag rid input)
               (loop rid)))))))


(define (drop-item id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (remove-object-from-inventory db id str)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
             (result (remove (lambda (x) (string-suffix-ci? str x)) record))
             (item (lset-difference equal? record result)))
      (cond ((null? item)
              (printf "You are not carrying that item!\n"))
             (else
              (printf "Removed ~a from your bag.\n" (first item))
              (add-object objectdb id (first item))
              (hash-set! db 'bag result))))))

(define (remove-object-from-inventorypic db id str)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db 'bag2)
    (let* ((record (hash-ref db 'bag2))
             (result (remove (lambda (x) (string-suffix-ci? str x)) record))
             (item (lset-difference equal? record result)))
      (cond ((null? item)
              (printf "You are not carrying that item!\n"))
             (else
              (add-objectpic objectpicdb id (first item))
              (hash-set! db 'bag2 result))))))
(define (drop-pic id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventorypic inventorypicdb id item)))
;;(startgame-new start)
(define inventorypicdb (make-hash))

(define (remove-object-from-roompic db id str)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room)
    (let* ((record (hash-ref db id))
            ;; Remove the occurrence of the item(based on the sufix, which is the most probable user input e.g. dagger) from the room
            (result (remove (lambda (x) (string-suffix-ci? str x)) record))
            ;; Return the items that record have and result don't
            (item (lset-difference equal? record result)))
      (cond ((null? item)
             ;; If item is null(item is not in the room), reports error
             (cond
               ((eq? id 2)
                (draws-sprite background (pos 0 0))))
             (cond
               ((eq? id 3)
                (draws-sprite background2 (pos 0 0))))
             (cond
               ((eq? id 4)
                (draws-sprite fightroom (pos 0 0)))))
            (else
              (add-objectpic inventorypicdb 'bag2 (first item))
              ;; Checks if the item interacted with is the interdimensional communicator. If it is, the game is over
              (if (eq? (first item) "the key")
                (begin
                  ;; Shows message and exits game
                  (printf "YOU HAVE FOUND THE WAY TO FREEDOM!\n")
                  (exit))
                ;; Removes item from the ground  
                (hash-set! db id result)))))))

(define (pick-pic id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-roompic objectpicdb id item)))

(define (add-objectspic db)
  (for-each
    (lambda (r)
      ;; Adds description(second r) to room id(first r)
      (add-objectpic db (first r) (second r))) key_objectspic))

(define (add-objectpic db id objectpic)
  ;; Returns true if id is stored in the database and false otherwise
  (if (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or )
    (let ((record (hash-ref db id)))
      ;; Assigns to the table key(id) the cons between the actual object and the preexisting objects in the key
      (hash-set! db id (cons objectpic record))
      (display-objectspic objectpicdb id)
      )
    ;; Assigns the object(consed with '() to become a list) to a key(id) in the hash table
    (hash-set! db id (cons objectpic empty))))

(define (display-objectspic db id)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or )
    (let* ((record (hash-ref db id))
            ;; Formats the output(list of items in the room)
            (output (string-join record " and ")))
      ;; Shows items in inventory or in the ground. Adds treatment to cases where the room or the inventory are empty
      (cond
        ((and (equal? output "") (eq? id 'bag2)) (printf "Your inventory is empty.\n"))
        ((and (equal? output "") (list? id)) (printf "The room is empty.\n"))
        ((and (not (equal? output "")) (eq? id 'bag2)) (printf "You are carrying ~a.\n" output))
        (else (when (hash-has-key? db id)
;      (cond
;        ((equal? (hash-ref rooms id) "Entrance")
;         (draws-sprite (picy:read-bitmap "./images/dungeon.jpg") (pos 0 0)))
;        ((equal? (hash-ref rooms id) "hall")
;         (draws-sprite (picy:read-bitmap "./images/background2.jpg") (pos 0 0)))
;        ((equal? (hash-ref rooms id) "hallway")
;         (draws-sprite (picy:read-bitmap "./images/fightroom.jpg") (pos 0 0)))
;        ((equal? (hash-ref rooms id) "corridor")
;         (draws-sprite (picy:read-bitmap "./images/startscreen.jpg") (pos 0 0)))
;        ((equal? (hash-ref rooms id) "lobby")
;         (draws-sprite (picy:read-bitmap "./images/dungeon.jpg") (pos 0 0)))
;        ((equal? (hash-ref rooms id) "hallway")
;         (draws-sprite (picy:read-bitmap "./images/dungeon.jpg") (pos 0 0)))
;        ((equal? (hash-ref rooms id) "court")
;         (draws-sprite (picy:read-bitmap "./images/dungeon.jpg") (pos 0 0)))
;        ((equal? (hash-ref rooms id) "pass")
;         (draws-sprite (picy:read-bitmap "./images/dungeon.jpg") (pos 0 0))))
                (cond
                  ((eq? importantvalue 1)
                (draws-sprite (picy:read-bitmap output) (pos 350 400))
                ))
              ))))))
(define importantvalue 1)
(draws-sprite startscreen (pos 0 0))
(add-objectspic objectpicdb)
(define run
  ( let (( inputeri ( read )))
     (cond [(eq? inputeri 'quit) ( exit )]
           (else
            (startgame-maze))
     )))