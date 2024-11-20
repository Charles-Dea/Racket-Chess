#lang racket
(require 2htdp/image)
(require srfi/43)
(require"main.rkt") 
(require"sprites.rkt")
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
              img)))
        st
        row))
    board-sprite
    (WS-board ws)))
(provide draw)
