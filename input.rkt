#lang racket
(require "game-logic.rkt")
(require "main.rkt")
(require "globals.rkt")
(define (mousein ws x y event)
  (cond
  [(string=? event "button-down") 
  (let* 
  [(row (int-floor (/ y (* scalar 128)))) 
  (col  (int-floor (/ x (* scalar 128)))) 
  (coord (BCoord row col))
  ]
       
    (handle-move ws coord))]
    
  [else ws]
  ))

(provide mousein)