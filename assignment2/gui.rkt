(require (prefix-in picy: racket/gui))
(struct pos (x y))


(define width 64)
(define height 47)
(define done 16)
(define message "Welcome To the Dungeon | Please enter a command: ")
(define screen_width (* width done))
(define screen_height (* height done))

(define background (picy:read-bitmap "./images/cellar.jpg"))
(define background2 (picy:read-bitmap "./images/cellar.jpg"))
(define sword (picy:read-bitmap "./images/sword.png"))
(define char (picy:read-bitmap "./images/knight.png"))
(define startscreen (picy:read-bitmap "./images/startscreen.jpg"))
(define exitpic (picy:read-bitmap "./images/exit.png"))
(define fightroom (picy:read-bitmap "./images/cellar.jpg"))
(define monster (picy:read-bitmap "./images/monster.png"))



(define frame (new picy:frame%
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


;(draws-sprite startscreen (pos 0 0))