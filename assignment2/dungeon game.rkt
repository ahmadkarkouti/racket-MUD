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

(define frame (new frame%
                   [label "Ahmad's Dungeon Game"]
                   [width screen_width]
                   [height screen_height])
)


(define (draws-sprite sprite poss)
  (send dc draw-bitmap sprite (pos-x  poss) (pos-y poss))
)

(define (canvas-key frame) (class canvas%
                             (super-new [parent frame])))

(define canvas ( new (canvas-key frame)))
(define dc (send canvas get-dc))
(send frame show #t)


(define inventory '(1 2 3 4))
(define (first)
  (set! message "Welcome to the dungeon"))
                 
              

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

      (new button%
           [parent frame]
           [label "OK"]
           [callback 
            (lambda args
              (cond
                ((equal? valv "north")
                 
                 (send field-1 set-label "You are in the cellar")
                 (set! world 2)
                 (set! start (lambda()
                               (send timer1 start 70)))
                 (send frame show #t)))
              (cond
                ((equal? valv "help")
                 (message-box "Title" "Welcome to the game of dungeons
Done by Ahmad Karkouti
Commands:
1- look
2- pick
3- drop
4- north - south - east - west
5- exit" #f '(ok))))
              (cond
                ((and (equal? valv "look") (equal? world 1) )
                 (message-box "Level 1" "You are in the corridor, You see a pathway to the north" #f '(ok))))
              (cond
                ((and (equal? valv "look") (equal? world 2) )
                 (message-box "Level 2" "You are in the cellar, You find a sword on the floor" #f '(ok))))
)])      

(define world 1)

(define timer (new timer%
                   [notify-callback (lambda()
                                      (cond
                                        ((eq? world 1)   ; if (car lat) = a

                                         ;if the first atom of the list == a
                                         
                                      
                                      (draws-sprite background (pos 0 0))
                                      ;(send dc draw-text "Level " (- (* width done) 280) 5)
                                      ;(message-box "Title" "Do you wish to continue?" #f '(yes-no))
                                      (first)))
                                      
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
                                      ))
                                      (start)


                                      


)]))

(define start (lambda()
                  (send timer start 100)))

(start)