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
    (println (Piece-moved? startPiece)) (cond
        [(and (= row endRow) (= col endCol)) (begin (println (Piece-moved? startPiece)) (hasMoved #t startPiece))]
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
    [(and (WS-firstClick ws)  (not (eq? 'null selected-piece))  (boolean=? (Piece-isWhite selected-piece) (WS-isWhiteTurn ws))) (WS (WS-board ws) #f #f (WS-isWhiteTurn ws))]
    [(and (WS-firstClick ws) (move-is-possible ws coord))  (WS (move-piece (WS-firstCoord ws) coord (WS-board ws)) #f #f (not (WS-isWhiteTurn ws)))]
    [(eq? selected-piece 'null) ws]
    [(boolean=? (Piece-isWhite selected-piece) (WS-isWhiteTurn ws)) (WS (WS-board ws) #t coord (WS-isWhiteTurn ws))]
    [else ws])))

(define (is-up-unobstructed y destCoord board) 
  (let* 
  [
    (x (BCoord-col destCoord))
    (space (piece-at board (BCoord y x)))
    (destY (BCoord-row destCoord))
  ]

  (cond
  [(= y destY) #t]
  [(eq? space 'null) (is-up-unobstructed (sub1 y) destCoord board)]
  [else #f]
  ))
  )
(define (is-down-unobstructed y destCoord board)
(let* 
  [
    (x (BCoord-col destCoord))
    (space (piece-at board (BCoord y x)))
    (destY (BCoord-row destCoord))
  ]
  (cond
  [(= y destY) #t]
  [(eq? space 'null) (is-down-unobstructed (add1 y) destCoord board)]
  [else #f]
  )
  )
)
(define (is-right-unobstructed x destCoord board)
(let* 
  [
    (y (BCoord-row destCoord))
    (space (piece-at board (BCoord y x)))
    (destX (BCoord-col destCoord))
  ]
  (cond
  [(= x destX) #t]
  [(eq? space 'null) (is-right-unobstructed (add1 x) destCoord board)]
  [else #f]
  )
  )
)
(define (is-left-unobstructed x destCoord board)
(let* 
  [
    (y (BCoord-row destCoord))
    (space (piece-at board (BCoord y x)))
    (destX (BCoord-col destCoord))
  ]
  (cond
  [(= x destX) #t]
  [(eq? space 'null) (is-left-unobstructed (sub1 x) destCoord board)]
  [else #f]
  )
  )
)

(define (is-ne-unobstructed x y destCoord board)
(let*
  [(pc (BCoord y x))]
  (cond
    [(and (= (BCoord-col destCoord) x) (= (BCoord-row destCoord) y)) #t]
    [(eq? (piece-at board pc) 'null) (is-ne-unobstructed (+ x 1) (- y 1) destCoord board)]
    [else #f])))
(define (is-se-unobstructed x y destCoord board)
(let*
  [(pc (BCoord y x))]
  (cond
    [(and (= (BCoord-col destCoord) x) (= (BCoord-row destCoord) y)) #t]
    [(eq? (piece-at board pc) 'null) (is-se-unobstructed (+ x 1) (+ y 1) destCoord board)]
    [else #f])))
(define (is-sw-unobstructed x y destCoord board)
(let*
  [(pc (BCoord y x))]
  (cond
    [(and (= (BCoord-col destCoord) x) (= (BCoord-row destCoord) y)) #t]
    [(eq? (piece-at board pc) 'null) (is-sw-unobstructed (- x 1) (+ y 1) destCoord board)]
    [else #f])))
(define (is-nw-unobstructed x y destCoord board)
(let*
  [(pc (BCoord y x))]
  (cond
    [(and (= (BCoord-col destCoord) x) (= (BCoord-row destCoord) y)) #t]
    [(eq? (piece-at board pc) 'null) (is-nw-unobstructed (- x 1) (- y 1) destCoord board)]
    [else #f])))

;very deicdedly unfinished 
(define (valid-pawn-move ws destCoord)
  (let* 
  [ 
    (board (WS-board ws))
    (startCoord (WS-firstCoord ws))
    (firstX (BCoord-col startCoord))
    (firstY (BCoord-row startCoord))
    (endX (BCoord-col destCoord))
    (endY (BCoord-row destCoord))
    (deltaX (abs (- firstX endX)))
    (deltaY (abs (- firstY endY)))]
    (and (= deltaX 0)
    (cond 
    [(and (not (= firstY 0)) (< endY firstY)) (is-up-unobstructed (sub1 firstY) destCoord board)]
    [(and (not (= firstY 7)) (> endY firstY)) (is-down-unobstructed (add1 firstY) destCoord board)]
    [(and (not (= firstX 7)) (> endX firstX)) (is-right-unobstructed (add1 firstX) destCoord board)]
    [(and (not (= firstX 0)) (< endX firstX)) (is-left-unobstructed (sub1 firstX) destCoord board)]
    [else #f]
    ))
    )
  )

(define (valid-knight-move ws destCoord)
  (let* 
  [ (startCoord (WS-firstCoord ws))
    (firstX (BCoord-col startCoord))
    (firstY (BCoord-row startCoord))
    (endX (BCoord-col destCoord))
    (endY (BCoord-row destCoord))
    (deltaX (abs (- firstX endX)))
    (deltaY (abs (- firstY endY)))
    (piece (piece-at (WS-board ws) startCoord))]
    (and (= 3 (+ deltaY deltaX)) (not (or (= deltaX 0) (= deltaY 0))))
    )
  )

  (define (valid-rook-move ws destCoord)
  (let* 
  [ 
    (board (WS-board ws))
    (startCoord (WS-firstCoord ws))
    (firstX (BCoord-col startCoord))
    (firstY (BCoord-row startCoord))
    (endX (BCoord-col destCoord))
    (endY (BCoord-row destCoord))
    (deltaX (abs (- firstX endX)))
    (deltaY (abs (- firstY endY)))]
    (and (or (= deltaX 0) (= deltaY 0)) 
    (cond 
    [(and (not (= firstY 0)) (< endY firstY)) (is-up-unobstructed (sub1 firstY) destCoord board)]
    [(and (not (= firstY 7)) (> endY firstY)) (is-down-unobstructed (add1 firstY) destCoord board)]
    [(and (not (= firstX 7)) (> endX firstX)) (is-right-unobstructed (add1 firstX) destCoord board)]
    [(and (not (= firstX 0)) (< endX firstX)) (is-left-unobstructed (sub1 firstX) destCoord board)]
    [else #f]
    ))
    )
  )


  (define (valid-bishop-move ws destCoord)
  (let* 
  [ (board (WS-board ws))
    (startCoord (WS-firstCoord ws))
    (firstX (BCoord-col startCoord))
    (firstY (BCoord-row startCoord))
    (endX (BCoord-col destCoord))
    (endY (BCoord-row destCoord))
    (deltaX  (- endX firstX))
    (deltaY (- endY firstY))
    (north (< deltaY 0))
    (south (> deltaY 0))
    (west (< deltaX 0))
    (east (> deltaX 0))]
    (and (= (abs deltaX) (abs deltaY))
    (cond
    [(and north east) (is-ne-unobstructed (add1 firstX) (sub1 firstY) destCoord board)]
    [(and south east) (is-se-unobstructed (add1 firstX) (add1 firstY) destCoord board)]
    [(and south west) (is-sw-unobstructed (sub1 firstX) (add1 firstY) destCoord board)]
    [(and north west) (is-nw-unobstructed (sub1 firstX) (sub1 firstY) destCoord board)]
    ))
  )
)

(define (valid-king-move ws destCoord)
  (let* 
  [ (startCoord (WS-firstCoord ws))
    (firstX (BCoord-col startCoord))
    (firstY (BCoord-row startCoord))
    (endX (BCoord-col destCoord))
    (endY (BCoord-row destCoord))
    (deltaX (abs (- firstX endX)))
    (deltaY (abs (- firstY endY)))]
    
    (and (<= (abs deltaX) 1) (<= (abs deltaY) 1))
    )
  )
;Are you selecting a moveable piece(is it white's turn when you click a white piece) and 

(define (move-is-possible ws destCoord) (let* 
    [
    (startCoord (WS-firstCoord ws))
    (piece (piece-at (WS-board ws) startCoord))
    (piece-name (Piece-name piece))
    ]

  (cond 
  [(string=? piece-name "knight") (valid-knight-move ws destCoord)]
  [(string=? piece-name "rook") (valid-rook-move ws destCoord)]
  [(string=? piece-name "bishop") (valid-bishop-move ws destCoord)]
  [(string=? piece-name "queen") (or (valid-rook-move ws destCoord) (valid-bishop-move ws destCoord))]
  [(string=? piece-name "king") (valid-king-move ws destCoord)]
  ;valid pawn move function is not currently completed
  ;[(string=? piece-name "pawn") (valid-pawn-move ws destCoord)]
  [else #t]
  
  )))

;(define (possible-moves ws coord) )
;check if a special move or special scenario is occurring or about to occur(en passant or pawn -> queen or smt)
;(define (special-moves ws coord) )

(provide handle-move)
(provide BCoord)
(provide BCoord-row)
(provide BCoord-col)
(provide move-piece)
(provide piece-at)
