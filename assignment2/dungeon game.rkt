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



;; Remove object from the room and add to your 
(define (remove-object-from-room db id str)
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
             (printf "I don't see that item in the room!\n"))
            (else
              (printf "Added ~a to your baj.\n" (first item))
              (add-objectpic inventorydb 'bag (first item))
              ;; Checks if the item interacted with is the interdimensional communicator. If it is, the game is over
              (if (eq? (first item) "an interdimensional communicator")
                (begin
                  ;; Shows message and exits game
                  (printf "Something strange is happening...\nYOU HAVE FOUND THE WAY TO FREEDOM!\n")
                  (exit))
                ;; Removes item from the ground  
                (hash-set! db id result)))))))

(define importantvalue 1)
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
              (printf "Added ~a to your baj.\n" (first item))
              (add-objectpic inventorypicdb 'bag2 (first item))
              ;; Checks if the item interacted with is the interdimensional communicator. If it is, the game is over
              (if (eq? (first item) "an interdimensional communicator")
                (begin
                  ;; Shows message and exits game
                  (printf "Something strange is happening...\nYOU HAVE FOUND THE WAY TO FREEDOM!\n")
                  (exit))
                ;; Removes item from the ground  
                (hash-set! db id result)))))))


(define (drop-item id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (drop-pic id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventorypic inventorypicdb id item)))

(define (remove-object-from-inventory db id str)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
             (result (remove (lambda (x) (string-suffix-ci? str x)) record))
             (item (lset-difference equal? record result)))
      (cond ((null? item)
              (printf "You are not carrying that item!\n"))
             (else
              (printf "Removed ~a from your baj.\n" (first item))
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
              (printf "Removed ~a from your baj.\n" (first item))
              (add-objectpic objectpicdb id (first item))
              (hash-set! db 'bag2 result))))))


(define (pick-item id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

(define (pick-pic id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-roompic objectpicdb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))

(define (display-inventorypic)
  (display-objectspic inventorypicdb 'bag2))


(define (display-help)

    (picy:message-box "I am here to help you" "Aloha explorers, you must find and kill da monster to leave\n the commands that you
can you are : \n
- look
- pick
- drop
- inventory
- attack
- help
- quit

--~~~~~~ Ahmad Karkouti  ~~~~~~~--" #f '(ok)))



(define monsterlife 1)

(define (gamestart initial-id)
    (let loop ((id initial-id) (description #t))
    (if description
        ;; If there is an available description, shows it on the screen
        (get-location id)
        ;; Else statement. Don't show location(because there isn't any description). Just shows the greater than symbol to incite user to type in text field
        (printf "> "))
    ;; Read input from the keyboard
    (let* ((input (read-line))
           ;; Function contained in the srfi/13 library, tokenize the input into substrings where a space character is found
           (string-tokens (string-tokenize input))
           ;; Creates a list of symbols(not strings) with the input. This is needed to compare the entry with our predefined lists
           (tokens (map string->symbol string-tokens)))
      ;; Decides which action response corresponds to. One of the most important calls in the code
      (let ((response (lookup id tokens)))
        (cond
          ((eq? response 'restart )
           (set! monster 1)))
        (cond
          ((eq? response 1 )
           (draws-sprite startscreen (pos 0 0))))
        (cond
          ((eq? response 2 )
           (draws-sprite background (pos 0 0))))
        (cond
          ((eq? response 3 )
           (draws-sprite background2 (pos 0 0))
           ))
        (cond
          ((and (eq? monster 1) (eq? response 4)
                )
           (set! monster (read-bitmap "./monster.png"))
           (draws-sprite monster (pos 500 300))))
        (cond
          ((eq? response 4 )
           (draws-sprite fightroom (pos 0 0))
          (draws-sprite monster (pos 500 300))))

      (cond
        ((eq? response 5 )
         (draws-sprite exitpic (pos 0 0))))
        ;(printf "Input: ~a\nTokens: ~a\nResponse: ~a\n" input tokens response)
        (cond ((number? response)
               (loop response #t))
              ;; If response meaning couldn't be found after the lookup function, shows error message
              ((eq? #f response)
               (format #t "Huh? I didn't understand that!\n")
               (loop id #f))
              ;; Response action is look at around the room for directions
              ((eq? response 'look)
               (cond
                 ((and (eq? id 4) (eq? monsterlife 0))
                  (format #t "You hear a whisper! <<the real exit is north-east>>\n")))
               ;; Retrieve possible directions
               (get-directions id)
               (loop id #f))
              ;; Response action is to pick an item
              ((eq? response 'pick)
               (pick-item id input)
               (pick-pic id input)
               (cond
                 ((eq? id 2)
                  (draws-sprite background (pos 0 0))))
               (cond
                 ((eq? id 3)
                  (draws-sprite background2 (pos 0 0))))
               (cond
                 ((eq? id 4)
                  (draws-sprite fightroom (pos 0 0))))
               (loop id #f))
              ((eq? response 'attack)

               (when (hash-has-key? inventorydb 'bag)
                 ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or baj)
                 (let* ((record (hash-ref inventorydb 'bag))
                        ;; Formats the output(list of items in the room)
                        (output (string-join record " and ")))
                   ;; Shows items in inventory or in the ground. Adds treatment to cases where the room or the inventory are empty
                   (cond
                     ((and (equal? output "") (eq? id 'baj)) (printf "Your inventory is empty.\n"))
                     ((and (equal? output "") (number? id) (eq? monsterlife 0)) (printf "The room is empty.\n"))
                     ((and (equal? output "") (number? id)) (printf "You need the sword to do that.\n"))
                     ((and (not (equal? output "")) (eq? id 'baj)) (printf "You are carrying ~a.\n" output))
                     (else (printf "You see ~a.\n" output)))
                   (cond
                     ((and (equal? output "a steel sword") (equal? id 4))
                      (format #t "You have Killed the monster, you should find a way to leave!\n")
                      (set! monsterlife 0)
                      (draws-sprite fightroom (pos 0 0))
                      ))))

                              (loop id #f))
              ;; Response action is to drop an item
              ((eq? response 'drop)
               ;; Drop item
               (drop-pic id input)
               (drop-item id input)
               (display-objectspic objectpicdb id)
               (loop id #f))
              ;; Response action is to show inventory
              ((eq? response 'north-east)
               (cond
                 ((and(eq? id 4) (eq? monsterlife 0))
               (draws-sprite exitpic (pos 0 0))
               (drop-item 3 input)
               (send (gamestart 5) start 100)))
               (format #t "You need to kill the monster first!\n")
               (loop id #f)
               )
              ((eq? response 'inventory)
               ;; Displays the inventory
               (display-inventory)
               (display-inventorypic)
               (loop id #f))
              ;; Response action is to display the help file
              ((eq? response 'help)
                ;; Displays Help text on the screen
                (display-help)
                (loop id #f))
              ;; Exit game command
              ((eq? response 'quit)
               ;; Exit the application
               (picy:message-box "Bye" "Hasta lavista baby" #f '(ok))
               (send frame show #f)
               (exit)))))))



(define start (lambda()
                (send gamestart start 100)
                ))


(add-objects objectdb)
(add-objectspic objectpicdb)
(draws-sprite startscreen (pos 0 0))
(send (gamestart 1) start 100)