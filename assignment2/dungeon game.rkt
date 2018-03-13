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

(define frame (new frame%
                   [label "Ahmad's Dungeon Game"]
                   [width screen_width]
                   [height screen_height])
  )


(define (draws-sprite sprite poss)
  (send dc draw-bitmap sprite (pos-x  poss) (pos-y poss))
  )

(define msg (new message% [parent frame] [label ""]))
(define (canvas-key frame) (class canvas%
                             (define/override (on-char key-event)
                               (cond
                                 [(equal? (send key-event get-key-code) #\return) (send buttony callback)]
                                 [(equal? (send key-event get-key-code) 'down) (send msg set-label "DOWN")]
                                 [(equal? (send key-event get-key-code) 'left)(send msg set-label "LEFT")]
                                 [(equal? (send key-event get-key-code) 'right) (send msg set-label "RIGHT")]
                                 [else (send msg set-label "Others")]))
                             (super-new [parent frame])))


(define canvas ( new (canvas-key frame)))
(define dc (send canvas get-dc))
(send frame show #t)




(define inventory '("." ""))

(define (first)
  (set! message "Welcome to the dungeon"))



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


(define world 0)
(define endy 0)
(define buttony
(new button%
     [parent frame]
     [label "OK"]
     [callback 
      (lambda args
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
      
              (set! world 1)
              (set! start (lambda()
                            (send timer start 70)))
              (send frame show #t)))))
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
             ((equal? valv "look")
              (message-box "Level 2" "You are in the cellar, You find a sword on the floor
You can see 2 pathways east and west that might lead to the exit," #f '(ok))))
          (cond
            ((equal? valv "pick")
             (cond
               ((equal? swordcount 1)
             (set! inventory (insertR '" sword" "." inventory))
             (message-box "Backpack" "You have added a sword to your backpack" #f '(ok))
             (message-box "Backpack" [cadr inventory] #f '(ok))
             (set! swordcount 0)
             ))))
           (cond
             ((equal? valv "east")
                 
              (send field-1 set-label "Congratulations")
              (set! world 0)
              (set! endy 1)
              (set! start (lambda()
                            (send gameend start 70)))
              (send frame show #t)))))
        (cond ; exit
          ((equal? world 3)
           (cond
             ((equal? valv "restart")
                 
              (send field-1 set-label "Welcome to the dungeon")
      
              (set! world 0)
              (set! start (lambda()
                            (send gamestart start 70)))
              (send frame show #t)))))
        )]))   


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
                                      
                                      (start)


                                      


                                      )]))



(define timer (new timer%
                   [notify-callback (lambda()
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
                                         ((eq? world 2)
                                          (draws-sprite background2 (pos 0 0))
                                          ;(draws-sprite char (pos 0 0))
                                          ;(message-box "Title" "Do you wish to continue?" #f '(yes-no))
                                          (draws-sprite sword (pos 350 400))
                                          (set! valv (send field-1 get-value))
                                          ))
                                       (start)
                                       )]))


(define gameend (new timer%
                   [notify-callback (lambda()
                                      (cond
                                        ((eq? world 3)   ; if (car lat) = a
                                         
                                         ;if the first atom of the list == a
                                         
                                      
                                         (draws-sprite exitpic (pos 0 0))
                                         ;(send dc draw-text "Level " (- (* width done) 280) 5)
                                         ;(message-box "Title" "Do you wish to continue?" #f '(yes-no))
                                         (set! valv (send field-1 get-value))
                                         ))
                                      
                                      ;(start)


                                      


                                      )]))

(define start (lambda()
                (cond
                  ((equal? world 100)
                  (send gameend start 100)))
                (cond
                ((equal? world 0)
                 (send gamestart start 100)
                ))))

(start)