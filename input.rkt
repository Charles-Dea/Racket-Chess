#lang racket
(require "game-logic.rkt")
(require "main.rkt")
(require "globals.rkt")
(require "sprites.rkt")
(require 2htdp/image)
(define (mousein ws x y event)
  (cond
  [(string=? event "button-down") 
  (let* 
  [(row (int-floor (/ y (* scalar 128)))) 
  (col  (int-floor (/ x (* scalar 128)))) 
  (coord (BCoord row col))
  ]
       (cond
       [(and (< row 8) (< col 8)) (handle-move ws (BCoord row col))]
       [(and 
        (> x (* scalar 1024)) 
        (> y (- (* 512 scalar) (/ (* (image-height BUTTON) scalar) 2))) 
        (< y (+ (* 512 scalar) (/ (* (image-height BUTTON) scalar) 2))))
        (begin (display "The button was clicked\n") ws)]
        [else ws]))]
    
  [else ws]
  ))

(provide mousein)