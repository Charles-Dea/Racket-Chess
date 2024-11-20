#lang racket
(require 2htdp/universe)

(struct Piece [name isWhite sprite])
(define emptyCell 'null)
(define boardVector
  (vector-immutable
    (vector-immutable(Piece
(struct WS[board])
(define(set-board ws board)
  (WS board))
(define(set-name name piece)
  (Piece name (Piece-isWhite piece) (Piece-sprite piece)))
(define(set-isWhite isWhite piece)
  (Piece (Piece-name piece) isWhite (Piece-sprite piece)))
(define(set-sprite sprite piece)
  (Piece (Piece-name piece) (Piece-isWhite piece) sprite))
