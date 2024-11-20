#lang racket
(require "sprites.rkt")

(struct Piece [name isWhite sprite])
(define emptyCell 'null)


(define boardVector
  (vector-immutable 
  (vector-immutable (Piece "rook" #f BLACK-ROOK) (Piece "knight" #f BLACK-KNIGHT) (Piece "bishop" #f BLACK-BISHOP) (Piece "queen" #f BLACK-QUEEN) (Piece "king" #f BLACK-KING) (Piece "bishop" #f BLACK-BISHOP) (Piece "knight" #f BLACK-KNIGHT) (Piece "rook" #f BLACK-ROOK))
  (vector-immutable (Piece "pawn" #f BLACK-PAWN) (Piece "pawn" #f BLACK-PAWN) (Piece "pawn" #f BLACK-PAWN) (Piece "pawn" #f BLACK-PAWN) (Piece "pawn" #f BLACK-PAWN) (Piece "pawn" #f BLACK-PAWN) (Piece "pawn" #f BLACK-PAWN) (Piece "pawn" #f BLACK-PAWN))
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable (Piece "pawn" #f WHITE-PAWN) (Piece "pawn" #f WHITE-PAWN) (Piece "pawn" #f WHITE-PAWN) (Piece "pawn" #f WHITE-PAWN) (Piece "pawn" #f WHITE-PAWN) (Piece "pawn" #f WHITE-PAWN) (Piece "pawn" #f WHITE-PAWN) (Piece "pawn" #f WHITE-PAWN))
  (vector-immutable (Piece "rook" #f WHITE-ROOK) (Piece "knight" #f WHITE-KNIGHT) (Piece "bishop" #f WHITE-BISHOP) (Piece "queen" #f WHITE-QUEEN) (Piece "king" #f WHITE-KING) (Piece "bishop" #f WHITE-BISHOP) (Piece "knight" #f WHITE-KNIGHT) (Piece "rook" #f WHITE-ROOK))
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
(provide Piece)
(provide boardVector)
(provide WS)
(provide set-board)
(provide set-name)
(provide set-isWhite)
(provide set-sprite)
