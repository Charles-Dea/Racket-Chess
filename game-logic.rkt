#lang racket

(require srfi/43)
(require "main.rkt")
; the coordinate on the board
(struct BCoord [row col])
(define (move-piece startCoord endCoord board) 
  (let*
    [(startRow (BCoord-row startCoord))
     (startCol (BCoord-col startCoord))
     (endRow (BCoord-row endCoord))
     (endCol (BCoord-col endCoord))
     (startPiece (vector-ref (vector-ref board startRow) startCol))]

   (vector-map (lambda (row fullRow) 
    (vector-map (lambda (col element) 
    (cond
        [(and (= row endRow) (= col endCol)) startPiece]
        [(and (= row startRow) (= col startCol)) 'null]
        [else (vector-ref fullRow col)] 
    )) fullRow))
   board)))

(define (piece-at board coord) 
(vector-ref (vector-ref board (BCoord-row coord)) (BCoord-col coord)))

(define (handle-move ws coord) 
  (let* 
    [(selected-piece (piece-at (WS-board ws) coord))]
    
  (cond 
    ;Get the place the player wants to move to, then don't update the the worldstate until we have checked if the move is actually possible
    #|(and (WS-firstClick ws) (move-is-possible ws coord))|#
    [(and (WS-firstClick ws) (not (eq? 'null selected-piece)) (boolean=? (Piece-isWhite selected-piece) (WS-isWhiteTurn ws))) (WS (WS-board ws) #f #f (WS-isWhiteTurn ws))]
    [(WS-firstClick ws)  (WS (move-piece (WS-firstCoord ws) coord (WS-board ws)) #f #f (not (WS-isWhiteTurn ws)))]
    [(eq? selected-piece 'null) ws]
    [(boolean=? (Piece-isWhite selected-piece) (WS-isWhiteTurn ws)) (WS (WS-board ws) #t coord (WS-isWhiteTurn ws))]
    [else ws])))

;Are you selecting a moveable piece(is it white's turn when you click a white piece) and 

;(define (move-is-possible) (cond \
;  []))
;(define (possible-moves ws coord) )
;check if a special move or special scenario is occurring or about to occur(en passant or pawn -> queen or smt)
;(define (special-moves ws coord) )

(provide handle-move)
(provide BCoord)
(provide BCoord-row)
(provide BCoord-col)
(provide move-piece)
(provide piece-at)