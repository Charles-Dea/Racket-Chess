#lang racket
(require 2htdp/image)
(require srfi/43)
(require "main.rkt") 
(require "sprites.rkt")
(require "globals.rkt")
(require "game-logic.rkt")

(define (whoseTurnText ws)
  (let*
    [
      (isWhite (WS-isWhiteTurn ws))
      (fontSize 70)
    ]
    (cond
      [isWhite (text "White to move" fontSize "black")]
      [else (text "Black to move" fontSize "black")]
    )
  )
)

(define (respective-forfeit-button ws)
(let*
    [
      (isWhite (WS-isWhiteTurn ws))
    ]
    (cond
      [isWhite BUTTON-WHITE]
      [else BUTTON-BLACK]
    )
  )
)

(define board-sprite
   (above
    (beside LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE)
    (beside DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE)
    (beside LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE)
    (beside DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE)
    (beside LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE)
    (beside DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE)
    (beside LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE)
    (beside DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE DARK-SQUARE LIGHT-SQUARE)
  ))

(define (draw ws)
  (cond
  [(= (WS-winner ws) NONE)(let* [
    (firstCoord (WS-firstCoord ws)) 
    (firstCol (if (eq? firstCoord #f) -1 (BCoord-col firstCoord)))
    (firstRow (if (eq? firstCoord #f) -1 (BCoord-row firstCoord)))
    (highlight (square 128 "solid" (color 0 255 0 128)))
    ]
  
  (scale scalar
  (beside
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
  
  (above
    (whoseTurnText ws)
    (respective-forfeit-button ws)
  )
  )
  ))]
  [(= (WS-winner ws) WHITE) (scale scalar WHITE-WINS)]
  [else (scale scalar BLACK-WINS)]))

(provide draw)
(provide respective-forfeit-button)