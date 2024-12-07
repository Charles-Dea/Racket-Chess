#lang racket
(require "game-logic.rkt")
(require "main.rkt")
(require "globals.rkt")
(require "sprites.rkt")
(require 2htdp/image)
(define default-ws (WS boardVector #f #f #t (BCoord 7 4) (BCoord 0 4) NONE))
(define (mousein ws x y event)
  (cond
  [(string=? event "button-down") 
  (let* 
  [(row (int-floor (/ y (* scalar 128)))) 
  (col  (int-floor (/ x (* scalar 128)))) 
  (coord (BCoord row col))
  ]
       (cond
       [(not (= NONE (WS-winner ws))) default-ws]
       [(and (< row 8) (< col 8)) (handle-move ws (BCoord row col))]
       [(and 
        (> x (* scalar 1024)) 
        (> y (- (* 512 scalar) (/ (* (image-height BUTTON) scalar) 2))) 
        (< y (+ (* 512 scalar) (/ (* (image-height BUTTON) scalar) 2))))
        (set-winner
          ws
          (if (WS-isWhiteTurn ws) BLACK WHITE))]
        [else ws]))]
    
  [else ws]
  ))

(provide mousein)