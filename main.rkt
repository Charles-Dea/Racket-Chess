#lang racket
(require "sprites.rkt")
(require "globals.rkt")
(require srfi/43)
(define NONE 0)
(define WHITE 1)
(define BLACK 2)
(define (is-piece? p) (and (not (eq? p 'null)) (not (eq? p 'nosquare))))

(struct Piece [name isWhite sprite moved? canEnPassant?])
(define emptyCell 'null)


(define boardVector
  (vector-immutable 
  (vector-immutable (Piece "rook" #f BLACK-ROOK #f #f) (Piece "knight" #f BLACK-KNIGHT #f #f) (Piece "bishop" #f BLACK-BISHOP #f #f) (Piece "queen" #f BLACK-QUEEN #f #f) (Piece "king" #f BLACK-KING #f #f) (Piece "bishop" #f BLACK-BISHOP #f #f) (Piece "knight" #f BLACK-KNIGHT #f #f) (Piece "rook" #f BLACK-ROOK #f #f))
  (vector-immutable (Piece "pawn" #f BLACK-PAWN #f #f) (Piece "pawn" #f BLACK-PAWN #f #f) (Piece "pawn" #f BLACK-PAWN #f #f) (Piece "pawn" #f BLACK-PAWN #f #f) (Piece "pawn" #f BLACK-PAWN #f #f) (Piece "pawn" #f BLACK-PAWN #f #f) (Piece "pawn" #f BLACK-PAWN #f #f) (Piece "pawn" #f BLACK-PAWN #f #f))
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell emptyCell)
  (vector-immutable (Piece "pawn" #t WHITE-PAWN #f #f) (Piece "pawn" #t WHITE-PAWN #f #f) (Piece "pawn" #t WHITE-PAWN #f #f) (Piece "pawn" #t WHITE-PAWN #f #f) (Piece "pawn" #t WHITE-PAWN #f #f) (Piece "pawn" #t WHITE-PAWN #f #f) (Piece "pawn" #t WHITE-PAWN #f #f) (Piece "pawn" #t WHITE-PAWN #f #f))
  (vector-immutable (Piece "rook" #t WHITE-ROOK #f #f) (Piece "knight" #t WHITE-KNIGHT #f #f) (Piece "bishop" #t WHITE-BISHOP #f #f) (Piece "queen" #t WHITE-QUEEN #f #f) (Piece "king" #t WHITE-KING #f #f) (Piece "bishop" #t WHITE-BISHOP #f #f) (Piece "knight" #t WHITE-KNIGHT #f #f) (Piece "rook" #t WHITE-ROOK #f #f))
  ))

(struct WS [board firstClick firstCoord isWhiteTurn whiteKingPos blackKingPos winner])

(define (set-board ws board)
  (WS board (WS-firstClick ws) (WS-firstCoord ws) (WS-isWhiteTurn ws) (WS-whiteKingPos ws) (WS-blackKingPos ws) (WS-winner ws)))

(define (set-winner ws winner)
  (WS (WS-board ws) (WS-firstClick ws) (WS-firstCoord ws) (WS-isWhiteTurn ws) (WS-whiteKingPos ws) (WS-blackKingPos ws) winner))

(define (set-name name piece)
  (Piece name (Piece-isWhite piece) (Piece-sprite piece) (Piece-moved? piece)))
(define (set-isWhite isWhite piece)
  (Piece (Piece-name piece) isWhite (Piece-sprite piece) (Piece-moved? piece)))
(define (set-sprite sprite piece)

  (Piece (Piece-name piece) (Piece-isWhite piece) sprite (Piece-moved? piece)))

(define (invert-isWhiteTurn ws) 
  (WS
    (WS-board ws)
    (WS-firstClick ws)
    (WS-firstCoord ws)
    (not (WS-isWhiteTurn ws))
    (WS-whiteKingPos ws)
    (WS-blackKingPos ws)
    (WS-winner ws)
  )
)
(define (hasMoved move? piece coord)
  (Piece (Piece-name piece) (Piece-isWhite piece) (Piece-sprite piece) move? 
  
  (if (and (string=? "pawn" (Piece-name piece)) (not (Piece-moved? piece)) (or (and (Piece-isWhite piece) (= (BCoord-row coord) 4)) (and (not (Piece-isWhite piece)) (= (BCoord-row coord) 3)))) #t
  (Piece-canEnPassant? piece))
  
  ))

(define (removeEnPassant ws) (WS 

  (vector-map (lambda (row fullRow)

    (vector-map (lambda (col element)
      (let* 
        [
          (board (WS-board ws))
          (belowCoord (BCoord (min 7 (add1 row)) col))
          (aboveCoord (BCoord (max 0 (sub1 row)) col))
          (belowPiece (piece-at board belowCoord))
          (abovePiece (piece-at board aboveCoord))
          (isWhite (and (is-piece? element) (Piece-isWhite element)))
          (currentPieceCanEnPassant (and (is-piece? element) (Piece-canEnPassant? element)))
          (oppPawnAbove (and (is-piece? abovePiece) (is-piece? element) (string=? "pawn" (Piece-name abovePiece)) (not (boolean=? (Piece-isWhite element) (Piece-isWhite abovePiece)))))
          (oppPawnBelow (and (is-piece? belowPiece) (is-piece? element) (string=? "pawn" (Piece-name belowPiece)) (not (boolean=? (Piece-isWhite element) (Piece-isWhite belowPiece)))))
        ]
      (cond  
        [(or (and isWhite currentPieceCanEnPassant oppPawnBelow) (and (not isWhite) currentPieceCanEnPassant oppPawnAbove)) 'null]
        [else element]))

    ) fullRow)

  ) (WS-board ws))

(WS-firstClick ws) (WS-firstCoord ws) (WS-isWhiteTurn ws) (WS-whiteKingPos ws) (WS-blackKingPos ws) (WS-winner ws)))

(define (alterEnPassant ws) (WS 

  (vector-map (lambda (row fullRow)

    (vector-map (lambda (col element)
      (let* 
        [
          (board (WS-board ws))
        ]
      (cond  
        [(and (is-piece? element) (boolean=? (Piece-isWhite element) (WS-isWhiteTurn ws)) (Piece-canEnPassant? element)) (Piece 
          (Piece-name element) (Piece-isWhite element) (Piece-sprite element) (Piece-moved? element) #f)]
        [(and (is-piece? element) (string=? (Piece-name element) "pawn") (or (= row 7) (= row 0))) (Piece "queen" (Piece-isWhite element) (if (Piece-isWhite element) WHITE-QUEEN BLACK-QUEEN) #t #f)]
        [else element]))

    ) fullRow)

  ) (WS-board ws))

(WS-firstClick ws) (WS-firstCoord ws) (WS-isWhiteTurn ws) (WS-whiteKingPos ws) (WS-blackKingPos ws) (WS-winner ws)))

(provide Piece)
(provide Piece-name)
(provide Piece-isWhite)
(provide Piece-sprite)
(provide Piece-moved?)
(provide Piece-canEnPassant?)
(provide boardVector)
(provide WS)
(provide WS-board)
(provide WS-firstClick)
(provide WS-firstCoord)
(provide WS-isWhiteTurn)
(provide WS-winner)

(provide set-board)
(provide set-winner)
(provide set-name)
(provide set-isWhite)
(provide set-sprite)
(provide invert-isWhiteTurn)
(provide hasMoved)

(provide alterEnPassant)
(provide removeEnPassant)

(provide WS-blackKingPos)
(provide WS-whiteKingPos)

(provide is-piece?)
(provide NONE)
(provide WHITE)
(provide BLACK)