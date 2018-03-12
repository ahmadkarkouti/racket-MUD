#lang racket
(require racket/gui)
(require racket/draw)
(require racket/mpair)
(require rackunit)
(require rackunit/text-ui)
(struct pos (x y))


(define width 64)
(define height 47)
(define done 16)
(define screen_width (* width done))
(define screen_height (* height done))


(define background (read-bitmap "./dungeon.jpg"))

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


(define my-text-field%
  (class text-field%
    (super-new)
    (define/override (on-focus on?)
      (when on? (printf "~a\n" (send this get-label))))))

(define field-1 (new my-text-field% [label "Command"] [parent frame]))

(send frame show #t)
(define timer (new timer%
                   [notify-callback (lambda()
                                      (draws-sprite background (pos 0 0))
                                      ;(message-box "Title" "Do you wish to continue?" #f '(yes-no))

)]))


(define start (lambda()
                  (send timer start 70)))

(start)