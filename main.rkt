#lang racket
(require 2htdp/universe)

(struct Piece [name isWhite])
(define emptyCell 'null)
(define boardVector (vector-immutable (Piece "rook" #f) (Piece "knight" #f) (Piece "bishop" #f) (Piece "king" #f) (Piece "king" #f) (Piece "bishop" #f) (Piece "knight" #f) (Piece "rook" #f)))
(struct WS[board])
(define(set-board ws board)
  (WS board))
