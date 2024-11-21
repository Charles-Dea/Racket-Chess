#lang racket

(require srfi/43)
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
(provide BCoord)
(provide BCoord-row)
(provide BCoord-col)
(provide move-piece)