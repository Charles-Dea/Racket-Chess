#lang racket
(require 2htdp/universe)
(require "game-logic.rkt")

(struct Piece [name isWhite sprite])
(define emptyCell 'null)


(define boardVector
  (vector-immutable 
  (vector-immutable (Piece "rook" #f) (Piece "knight" #f) (Piece "bishop" #f) (Piece "king" #f) (Piece "king" #f) (Piece "bishop" #f) (Piece "knight" #f) (Piece "rook" #f))
  (vector-immutable (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f))
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f) (Piece "pawn" #f))
  (vector-immutable (Piece "rook" #f) (Piece "knight" #f) (Piece "bishop" #f) (Piece "king" #f) (Piece "king" #f) (Piece "bishop" #f) (Piece "knight" #f) (Piece "rook" #f))
  ))
(struct WS[board])
(define(set-board ws board)
  (WS board))
(define(set-name name piece)
  (Piece name (Piece-isWhite piece) (Piece-sprite piece)))
(define(set-isWhite isWhite piece)
  (Piece (Piece-name piece) isWhite (Piece-sprite piece)))
(define(set-sprite sprite piece)
  (Piece (Piece-name piece) (Piece-isWhite piece) sprite))