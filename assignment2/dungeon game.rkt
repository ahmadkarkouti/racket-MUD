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



(define rember ;This is defining the main function rember
  (lambda (a lat);Lambda is an anonymous function
    
    (cond ;Condition
      
      ((null? lat) (quote ())) ;Check if lat is null, then output an empty list
      ((eq? ( car lat) a) ( cdr lat)) ;check if (car lat) = a , output cdr lat

      ;Else construct a new list with the first atom of lat
      ;And call rember to the cdr of lat
      (else ( cons ( car lat) 
                   (rember a ( cdr lat)))))))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ( ( eq? ( car lat) old)
                ( cons old
                       ( cons new ( cdr lat))))
              (else ( cons ( car lat)
                           ( insertR new old
                                     ( cdr lat))))))))) 


(define valv "hi")
(send dc set-font (make-object font% 14 'modern))
(send dc set-text-foreground "white")


(define my-text-field%
  (class text-field%
    (super-new)
    (define/override (on-focus on?)
      (when on? (printf "~a\n" (set! valv (send this get-value)))))))

(define field-1
  (new my-text-field% [label message] [parent frame]) )

(define fightvalue 0)
(define world 0)
(define endy 0)
(define buttony
(new button%
     [parent frame]
     [label "OK"]
     [callback 
      (lambda args
        (cond
          ((equal? valv "exit")
           (send frame show #f)))
        (cond
          ((equal? valv "drop sword")
           (set! inventory(rember '"sword" inventory))
           (set! swordcount 1)
           (set! sword (read-bitmap "./sword.png"))))
        (cond ; Help
          ((equal? valv "help")
           (message-box "Title" "Welcome to the game of dungeons
Done by Ahmad Karkouti
Commands:
1- look
2- pick
3- drop
4- north - south - east - west
5- inventory
6- exit" #f '(ok))))
                (cond ; Inventory
          ((equal? valv "inventory")
           (message-box "Backpack" (~a "" (cdr inventory) ) #f '(ok))
           ))
        (cond ; Start
          ((equal? world 0)
           (cond
             ((equal? valv "start")
              (send field-1 set-label "You are in the corridor")
              (set! inventory '("." ""))
              (set! monster (read-bitmap "./monster.png"))
              (set! monsterlife 1)
              (set! swordcount 1)
              (set! sword (read-bitmap "./sword.png"))
              (set! endy 2)
              (set! world 1)
              
  ))
           (cond
             ((equal? valv "restart")
              (send field-1 set-label "Welcome To the Dungeon | Please enter a command: ")
              (set! inventory '("." ""))
              (set! world 0)
              (set! endy 0)
              (set! startscreen (read-bitmap "./startscreen.jpg"))
              (set! monster (read-bitmap "./monster.png"))
              (set! sword (read-bitmap "./sword.png"))
              (set! swordcount 1)
              (set! monsterlife 1)
))
           ))
        (cond ; World 1
          ((equal? world 1)
           (cond
             ((equal? valv "look")
              (message-box "Level 1" "You are in the corridor, You see a pathway to the north" #f '(ok))))
           (cond
             ((equal? valv "north")
                 
              (send field-1 set-label "You are in the cellar")
      
              (set! world 2)
              (set! start (lambda()
                            (send timer1 start 70)))
              (send frame show #t)))))
        (cond ; World 2
          ((equal? world 2)
           (cond
             ((and(equal? valv "look") (equal? swordcount 1))
              (message-box "Level 2" "You are in the cellar, You find a sword on the floor
You can see 2 pathways east and west that might lead to the exit," #f '(ok))))
          (cond
            ((equal? valv "pick")
             (cond
               ((equal? swordcount 1)
             (set! inventory (insertR '"sword" "." inventory))
             (message-box "Backpack" "You have added a sword to your backpack" #f '(ok))
             (message-box "Backpack" [cadr inventory] #f '(ok))
             (set! swordcount 0)
             ))))
           (cond
             ((equal? valv "east")
                 
              (send field-1 set-label "Congratulations")
              (set! world 0)
              (set! endy 1)
              
             ))
           (cond
             ((equal? swordcount 0)
              (set! sword (read-bitmap "./nostone.png"))
              (cond
             ((equal? valv "look")
              (message-box "Level 2" "You are in the cellar,
You can see 2 pathways east and west that might lead to the exit." #f '(ok))))
              ))
           (cond
             ((equal? valv "west")
                 
              (send field-1 set-label "Get Ready to fight: ")
      
              (set! world 4)
              (set! fightvalue 1)
              (set! start (lambda()
                            (send timer2 start 70)))
              (send frame show #t)))
           ))
        (cond
          ((equal? fightvalue 1)
          (cond
             ((and(equal? valv "look")(equal? monsterlife 1))
              (message-box "Fight Room" "You see a monster, You should Attack it" #f '(ok))))
          (cond
            ((and(equal? valv "attack") (equal? swordcount 1))
                 (message-box "Fight Room" "How can you fight it, YOU DIDN'T PICK UP THE SWORD!!!" #f '(ok))))
          (cond
            ((and(equal? valv "attack") (equal? swordcount 0))
                 (message-box "Fight Room" "YOU HAVE KILLED THE BEAST! FIND A WAY TO LEAVE" #f '(ok))
                 (set! monster (read-bitmap "./monsterdead.png"))
                 (set! monsterlife 0)))
          (cond
            ((and (equal? valv "look") (equal? swordcount 0) (equal? monsterlife 0))
             (message-box "Fight Room" "You find a secret passage hidden on the northeast corner" #f '(ok))
           
             ))
           (cond
             ((and (equal? valv "northeast")(equal? monsterlife 0))
                 
              (send field-1 set-label "Congratulations")
              (set! startscreen (read-bitmap "./exit.png"))
              (set! fightvalue 0)
              (set! swordcount 0)
              (set! world 0)
              (set! endy 0)
              
             ))
                      (cond
             ((and (equal? valv "northeast")(equal? monsterlife 1))
                 
              (message-box "Fight Room" "The monster is blocking the exit, (How did you know it was there?)" #f '(ok))
              
             ))
          ))
           )]

        ))   

(define monsterlife 1)
(define swordcount 1)






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