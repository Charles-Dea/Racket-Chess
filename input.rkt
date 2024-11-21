#lang racket
(require "game-logic.rkt")
(require "main.rkt")
(define (mousein ws x y event)
  (cond
  [(string=? event "button-down") 
  (let* 
  [(row (floor (/ y 128))) (col (floor (/ x 128))) (coord (BCoord row col))]
       
      
    (begin (println row)(println col) 
    (cond [(WS-firstClick ws) (WS (move-piece (WS-firstCoord ws) coord (WS-board ws)) #f #f)]
    [else (WS (WS-board ws) #t coord)])))]
    
  [else ws]
  ))


(provide mousein)