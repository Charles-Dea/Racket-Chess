#lang racket
(require 2htdp/image)
(require srfi/43)
(require "main.rkt") 
(require "sprites.rkt")
(require "globals.rkt")
(require "game-logic.rkt")

(define board-sprite
  (above
    (beside LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE)
    (beside DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE)
    (beside LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE)
    (beside DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE)
    (beside LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE)
    (beside DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE)
    (beside LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE)
    (beside DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE)))

(define(draw ws)
  (let* [
    (firstCoord (WS-firstCoord ws)) 
    (firstCol (if (eq? firstCoord #f) -1 (BCoord-col firstCoord)))
    (firstRow (if (eq? firstCoord #f) -1 (BCoord-row firstCoord)))
    (highlight (square 128 "solid" (color 0 255 0 128)))
    ]
  (scale scalar
  (vector-fold
    (lambda(y st row)
      (vector-fold
        (lambda(x img piece)
          (if(eq? piece'null)
            img
            (place-image
              (Piece-sprite piece)
              (+(* x 128)64)
              (+(* y 128)64)
              (if (and (= x firstCol) (= y firstRow)) (place-image highlight (+(* x 128)64) (+(* y 128)64) img) img))))
        st
        row))
    board-sprite
    (WS-board ws)) 
  )))
(provide draw)
