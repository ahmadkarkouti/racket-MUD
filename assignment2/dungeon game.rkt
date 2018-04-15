#lang racket
(require racket/gui)
(require racket/draw)
(require racket/mpair)
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(struct pos (x y))


(define width 64)
(define height 47)
(define done 16)
(define message "Welcome To the Dungeon | Please enter a command: ")
(define screen_width (* width done))
(define screen_height (* height done))


(define background (read-bitmap "./dungeon.jpg"))
(define background2 (read-bitmap "./background2.jpg"))
(define sword (read-bitmap "./sword.png"))
(define char (read-bitmap "./knight.png"))
(define startscreen (read-bitmap "./startscreen.jpg"))
(define exitpic (read-bitmap "./exit.png"))
(define fightroom (read-bitmap "./fightroom.jpg"))
(define monster (read-bitmap "./monster.png"))

(define frame (new frame%
                   [label "Ahmad's Dungeon Game"]
                   [width screen_width]
                   [height screen_height])
  )


(define (draws-sprite sprite poss)
  (send dc draw-bitmap sprite (pos-x  poss) (pos-y poss))
  )

(define msg (new picy:message% [parent frame] [label ""]))
(define (canvas-key frame) (class picy:canvas%
                             (define/override (on-char key-event)
                               (cond
                                 [else (send msg set-label "Others")]))
                             (super-new [parent frame])))


(define canvas ( new (canvas-key frame)))
(define dc (send canvas get-dc))
(send frame show #t)


;; Objects description
(define objects '((2 "a gold coin")
                  (3 "a steel sword")))

(define objectspic '((2 "./goldcoin.png")
                     (3 "./sword.png")))

(define descriptions '((1 "Welcome to the game of dungeons")
                       (2 "You are in the Corridor.")
                       (3 "You are in the Cellar.")
                       (4 "You are in the Fight Room.")
                       (5 "You are in the FREE WORLD HURAAAHHH !!!.")))

(define objectdb (make-hash))

(define objectpicdb (make-hash))




;; Initializes the inventory database
(define inventorydb (make-hash))

(define inventorypicdb (make-hash))

;; Lists of pairs. First we have the user's entry and second we have what our software should understand with that entry
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define drop '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((baj) inventory)))
(define help '(((help) help)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define attack '(((fight) attack) ((kill monster) attack) ((slay dragon) attack) ((attack) attack)))
(define north-east '(((north-east) north-east)))


;; Lists using unquote-splicing to dynamically reference all the other lists
(define actions `(,@look ,@pick ,@drop ,@inventory ,@help ,@quit ,@attack ,@north-east))
(define decisiontable `((1 ((start) 2) ,@actions)
                        (2 ((north) 3) ,@actions)
                        (3 ((west) 4) ((south) 2)  ,@actions)
                        (4 ((east) 3) ,@actions)
                        (5 ((restart) 1) ,@actions)))



;; Receives the id(current room number) and a list of symbols that represents the user input(tokens)
(define (lookup id tokens)
  ;; Assigns to record a list with the possible actions for the current room
  (let* ((record (assv-ref decisiontable id))
         ;; Assigns to keylist a list with the valid keywords for the game
         (keylist (get-keywords id))
         ;; By calling list-of-lengths, creates a list of lengths with the most probable commands and then decide which one is the most probable using index-of-largest-number
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    ;; If there is an index(prevent errors), retrieves the command that is present in that index inside the list record(list which contains the valid actions for the current room). Otherwise returns false
    (if index 
      (cadr (list-ref record index))
      #f)))


;; Returns a list of lengths that shows the most probable commands given by the user. e.g. (0 0 0 3 0 0 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     ;; Returns the intersection between the tokens list(command given) and the keyword
     (let ((set (lset-intersection eq? tokens x)))
       ;; If there is an intersection between the lists, the length of set will not be zero, and thus, the result will have some weight
       (* (/ (length set) (length x)) (length set))))
   keylist))

;; Returns the most probable input command
(define (index-of-largest-number list-of-numbers)
  ;; Sorts the list of lengths in descending order and gets the first element(greatest)
  (let ((n (car (sort list-of-numbers >))))
    ;; Checks if the list is not empty(returns #f if the greatest element is 0)
    (if (zero? n)
      #f
      ;; Returns the index of the entry with the greatest weight, so it can be matched with the list of keywords later
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))



(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))


(define (get-keywords id)
  ;; Assigns to keys a list with the possible actions for the current room
  (let ((keys (assq-ref decisiontable id)))
    ;; Return the accepted keywords(not their actions)
    (map (lambda (key) (car key)) keys)))

(define (slist->string l)
  (string-join (map symbol->string l)))

;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;

;; Retrieve what directions you see from the room you are
(define (get-directions id)
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  ;; In list decisiontable, finds the pair that has car equals to id and assign it to record
  (let ((record (assq id decisiontable)))
    ;; record goes through a filter and if the second value of it is a number(this is a room), it is assigned to result. Also gets the length of n(rooms you can go to)
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      ;; Conditional case used to finally check the directions
      (cond ((= 0 n)
             ;; 0 directions were retrieved
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             ;; Extract the directions from result using our slist->string function
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             ;; The first line(losym) in let* remove the indexes(numbers) from the directions. The second one(lostr) transforms the list in a lat with the directions.
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               ;; Takes the atoms from lostr and transform them into a string separated by " and "
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))


(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  (display-objectspic objectpicdb id)
  (printf "> "))

;; Pregame population of rooms with objects
(define (add-objects db)
  (for-each
    (lambda (r)
      ;; Adds description(second r) to room id(first r)
      (add-object db (first r) (second r))) objects))



(define (add-objectspic db)
  (for-each
    (lambda (r)
      ;; Adds description(second r) to room id(first r)
      (add-objectpic db (first r) (second r))) objectspic))



;; Adds a given object to a database(inventory or object dbs)
(define (add-object db id object)
  ;; Returns true if id is stored in the database and false otherwise
  (if (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or )
    (let ((record (hash-ref db id)))
      ;; Assigns to the table key(id) the cons between the actual object and the preexisting objects in the key
      (hash-set! db id (cons object record)))
    ;; Assigns the object(consed with '() to become a list) to a key(id) in the hash table
    (hash-set! db id (cons object empty))))

(define (add-objectpic db id objectpic)
  ;; Returns true if id is stored in the database and false otherwise
  (if (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or )
    (let ((record (hash-ref db id)))
      ;; Assigns to the table key(id) the cons between the actual object and the preexisting objects in the key
      (hash-set! db id (cons objectpic record))
      (display-objectspic objectpicdb id))
    ;; Assigns the object(consed with '() to become a list) to a key(id) in the hash table
    (hash-set! db id (cons objectpic empty))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (display-objects db id)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or )
    (let* ((record (hash-ref db id))
            ;; Formats the output(list of items in the room)
            (output (string-join record " and ")))
      ;; Shows items in inventory or in the ground. Adds treatment to cases where the room or the inventory are empty
      (cond
        ((and (equal? output "") (eq? id 'bag)) (printf "Your inventory is empty.\n"))
        ((and (equal? output "") (number? id)) (printf "The room is empty.\n"))
        ((and (not (equal? output "")) (eq? id 'bag)) (printf "You are carrying ~a.\n" output))
        (else (printf "You see ~a.\n" output))))))

(define (piclocation output)
  (cond
    ((equal? output "./sword.png")
     350))
  (cond
    ((equal? output "./goldcoin.png")
     200)))


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
        ((and (equal? output "") (number? id)) (printf "The room is empty.\n"))
        ((and (not (equal? output "")) (eq? id 'bag2)) (printf "You are carrying ~a.\n" output))
        (else (when (hash-has-key? db id)
                (cond
                  ((equal? id 3)
                   (draws-sprite background2 (pos 0 0))))
                (cond
                  ((equal? id 2)
                   (draws-sprite background (pos 0 0))))
                (cond
                  ((equal? id 4)
                   (draws-sprite fightroom (pos 0 0))))
                (cond
                  ((eq? importantvalue 1)
                (draws-sprite (read-bitmap output) (pos 350 400))
                ))
              ))))))

(define gamestart (new timer%
                   [notify-callback (lambda()
                                      (cond
                                        ((and(eq? world 0) (eq? endy 0))  ; if (car lat) = a
                                         (set! valv (send field-1 get-value))
                                         ;if the first atom of the list == a
                                         
                                      
                                         (draws-sprite startscreen (pos 0 0))
                                         ;(send dc draw-text "Level " (- (* width done) 280) 5)
                                         ;(message-box "Title" "Do you wish to continue?" #f '(yes-no))
                                         (first)))
                                      (cond
                                        ((eq? endy 1)   ; if (car lat) = a
                                         (set! valv (send field-1 get-value))
                                         ;if the first atom of the list == a
                                         
                                      
                                         (draws-sprite exitpic (pos 0 0))
                                         ;(send dc draw-text "Level " (- (* width done) 280) 5)
                                         ;(message-box "Title" "Do you wish to continue?" #f '(yes-no))
                                         (first)))
                                      (cond
                                         ((eq? fightvalue 1)
                                          (draws-sprite fightroom (pos 0 0))
                                          (draws-sprite monster (pos 500 300))
                                          (set! valv (send field-1 get-value))
                                          ))

                                      (cond
                                        ((eq? world 1)   ; if (car lat) = a
                                         
                                         ;if the first atom of the list == a
                                         
                                      
                                         (draws-sprite background (pos 0 0))
                                         ;(send dc draw-text "Level " (- (* width done) 280) 5)
                                         ;(message-box "Title" "Do you wish to continue?" #f '(yes-no))
                                         (set! valv (send field-1 get-value))
                                         ))
                                      
                                      (start)


                                      


                                      )]))




(define timer1 (new timer%
                    [notify-callback (lambda()
                                       (cond
                                         ((and(eq? world 2) (eq? fightvalue 0))
                                          (draws-sprite background2 (pos 0 0))
                                          ;(draws-sprite char (pos 0 0))
                                          ;(message-box "Title" "Do you wish to continue?" #f '(yes-no))
                                          (draws-sprite sword (pos 350 400))
                                          (set! valv (send field-1 get-value))
                                          ))
                                       (start)
                                       )]))

(define timer2 (new timer%
                    [notify-callback (lambda()
                                       (cond
                                         ((eq? world 4)
                                          (cond
                                            ((eq? fightvalue 1)
                                          (draws-sprite fightroom (pos 0 0))
                                          (set! valv (send field-1 get-value))
                                          ))))
                                       (start)
                                       )]))



(define start (lambda()
                (send gamestart start 100)
                ))

(start)