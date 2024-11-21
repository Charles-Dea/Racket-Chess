#lang racket
(require "game-logic.rkt")
(require "main.rkt")
(require "globals.rkt")
(define (mousein ws x y event)
  (cond
  [(string=? event "button-down") 
  (let* 
  [(row (int-floor (/ y (* scalar 128)))) (col  (int-floor (/ x (* scalar 128)))) (coord (BCoord row col))]
       
      
    (begin (println (piece-at (WS-board ws) coord))(cond 
    [(WS-firstClick ws) (WS (move-piece (WS-firstCoord ws) coord (WS-board ws)) #f #f)]
    [(eq? (piece-at (WS-board ws) coord) 'null) ws]
    [else (WS (WS-board ws) #t coord)])))]
    
  [else ws]
  ))


(provide mousein)