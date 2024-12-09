#lang racket

(require srfi/43)
(require "main.rkt")
(require "globals.rkt")
; the coordinate on the board

(define (move-piece startCoord endCoord board) 
  (let*
    [(startRow (BCoord-row startCoord))
     (startCol (BCoord-col startCoord))
     (endRow (BCoord-row endCoord))
     (endCol (BCoord-col endCoord))
     (startPiece (vector-ref (vector-ref board startRow) startCol))]

   (vector-map (lambda (row fullRow) 
    (vector-map (lambda (col element) 
    ;(Piece-moved? startPiece) 
    (cond
        [(and (= row endRow) (= col endCol)) (hasMoved #t startPiece (BCoord row col))]
        [(and (= row startRow) (= col startCol)) 'null]
        [else (vector-ref fullRow col)] 
    )) fullRow))
   board)))

(define (newKingPos ws coord isWhite) (let* [
  (board (WS-board ws))
  (first-coord (WS-firstCoord ws))
  (selected-piece (piece-at board first-coord))
  (selected-piece-is-king (and (is-piece? selected-piece) (string=? (Piece-name selected-piece) "king")))
  (selected-piece-isWhite (and (is-piece? selected-piece) (Piece-isWhite selected-piece)))
  (currentKingPos (cond
    [isWhite (WS-whiteKingPos ws)]
    [else (WS-blackKingPos ws)]))
]

  (cond
    [(and (boolean=? isWhite selected-piece-isWhite) selected-piece-is-king) coord]
    [else currentKingPos]
  )))

(define (print-king-poses ws)
  (let* 
  [
    (bKingPos (WS-blackKingPos ws))
    (wKingPos (WS-whiteKingPos ws))
  ] 
  
  (println (string-append "black: " (number->string (BCoord-row bKingPos)) " " (number->string (BCoord-col bKingPos)) " " "white: " (number->string (BCoord-row wKingPos)) " " (number->string (BCoord-col wKingPos)))))
)

(define (hypothetical-move ws coord)
  (let*
    [
      (board (WS-board ws))
      (firstCoord (WS-firstCoord ws))
      (moving-piece (piece-at board firstCoord))
      (moving-piece-is-king (string=? (Piece-name moving-piece) "king"))
    ]
    (invert-isWhiteTurn
      (alterEnPassant
        (removeEnPassant
          (WS
            (move-piece firstCoord coord board)
            #f 
            firstCoord
            (not (WS-isWhiteTurn ws))
            (newKingPos ws coord #t)
            (newKingPos ws coord #f)
            NONE
          )
        )
      )
    )
  )
)

(define (is-valid-castle ws coord)

  (let*
    [
      (board (WS-board ws))
      (pcoord (WS-firstCoord ws))
      (selected-piece (piece-at board pcoord))
      (isWhite (Piece-isWhite selected-piece))
      (destPiece (piece-at board coord))
      (isSelectedPieceKing (string=? "king" (Piece-name selected-piece)))
      (destPieceIsAllyRook (and (is-piece? destPiece) (string=? (Piece-name destPiece) "rook") (boolean=? (Piece-isWhite destPiece) isWhite)))
    ]
    (
        (and 
          isSelectedPieceKing 
          (or destPieceIsAllyRook (= (abs (- (BCoord-col coord) (BCoord-col pcoord))) 2) (= (BCoord-row coord) (BCoord-row pcoord)))

        )
    )
  )

)

(define (handle-move ws coord) 
  (let* 
    [
      (selected-piece (piece-at (WS-board ws) coord))
      (board (WS-board ws))
    ]
  
  (cond 
    ;Get the place the player wants to move to, then don't update the the worldstate until we have checked if the move is actually possible
   ; [(and (WS-firstClick ws) (move-is-possible ws coord) (is-in-check (hypothetical-move ws coord))) ws]
    ;[(is-valid-castle ws coord) (castle ws)]
    [(and (WS-firstClick ws)  (not (eq? 'null selected-piece))  (boolean=? (Piece-isWhite selected-piece) (WS-isWhiteTurn ws))) (WS (WS-board ws) #f #f (WS-isWhiteTurn ws) (WS-whiteKingPos ws) (WS-blackKingPos ws) (WS-winner ws))]
    [(and (WS-firstClick ws) (move-is-possible ws coord)) (alterEnPassant (removeEnPassant (WS (move-piece (WS-firstCoord ws) coord (WS-board ws)) #f #f (not (WS-isWhiteTurn ws)) (newKingPos ws coord #t) (newKingPos ws coord #f) (WS-winner ws))))]
    [(eq? selected-piece 'null) ws]
    [(boolean=? (Piece-isWhite selected-piece) (WS-isWhiteTurn ws)) (WS (WS-board ws) #t coord (WS-isWhiteTurn ws) (WS-whiteKingPos ws) (WS-blackKingPos ws) (WS-winner ws))]
    [else ws])))

(define (is-in-check ws isWhite)

  (let* 
  [
    (kingPos (if isWhite (WS-whiteKingPos ws) (WS-blackKingPos ws)))
    (board (WS-board ws))
    (x (BCoord-col kingPos))
    (y (BCoord-row kingPos))    
    (opposingColor (not isWhite))
  ]
  (or 
      (pawn-can-hit ws kingPos opposingColor)
      (knight-can-hit ws kingPos opposingColor)
      (king-can-hit ws kingPos opposingColor)
      (not (is-north-safe x (sub1 y) board isWhite))
      (not (is-south-safe x (add1 y) board isWhite))
      (not (is-west-safe (sub1 x) y board isWhite))
      (not (is-east-safe (add1 x) y board isWhite))
      (not (is-nw-safe (sub1 x) (sub1 y) board isWhite))
      (not (is-ne-safe (add1 x) (sub1 y) board isWhite))
      (not (is-sw-safe (sub1 x) (add1 y) board isWhite))
      (not (is-se-safe (add1 x) (add1 y) board isWhite))
  )
  )
)

(define (type-of-piece-at board coord isWhite)
  (let*
  ((piece (piece-at board coord)))
  (cond
    [(or (eq? piece 'nosquare) (eq? piece 'null)) piece]
    [(not (boolean=? isWhite (Piece-isWhite piece))) "ally"]
    [else (Piece-name piece)])))
(define (pawn? piece)
  (if (string? piece)
    (string=? piece "pawn")
    #f))

(define (king-can-hit ws destCoord isWhite)

  (let*
    [
      (board (WS-board ws))
      (r (BCoord-row destCoord))
      (c (BCoord-col destCoord))
      (u1 (piece-at board (BCoord (sub1 r) c)))
      (u1r1 (piece-at board (BCoord (sub1 r) (add1 c))))
      (r1 (piece-at board (BCoord r (add1 c))))
      (d1r1 (piece-at board (BCoord (add1 r) (add1 c))))
      (d1 (piece-at board (BCoord (add1 r) c)))
      (d1l1 (piece-at board (BCoord (add1 r) (sub1 c))))
      (l1 (piece-at board (BCoord r (sub1 c))))
      (u1l1 (piece-at board (BCoord (sub1 r) (sub1 c))))
    ]
    (ormap 
    (lambda (piece) 
      (and (is-piece? piece) 
      (boolean=? (Piece-isWhite piece) isWhite) 
      (string=? "king" (Piece-name piece)))) 
    (list u1 u1r1 r1 d1r1 d1 d1l1 l1 u1l1)
  )

))
(define (pawn-can-hit ws destCoord isWhite)
  (let*
  ((up (if isWhite 1 -1))
  (r (BCoord-row destCoord))
  (c (BCoord-col destCoord))
  (piece1-type (type-of-piece-at (WS-board ws) (BCoord (+ r up) (add1 c)) isWhite))
  (piece2-type (type-of-piece-at (WS-board ws) (BCoord (+ r up) (sub1 c)) isWhite)))
  (or (pawn? piece1-type) (pawn? piece2-type))))

(define (knight-can-hit ws destCoord isWhite)
  (let*
  [
    (board (WS-board ws))
    (u2l1 (piece-at board (BCoord (- (BCoord-row destCoord) 2) (- (BCoord-col destCoord) 1))))
    (u2r1 (piece-at board (BCoord (- (BCoord-row destCoord) 2) (+ (BCoord-col destCoord) 1))))
    (u1r2 (piece-at board (BCoord (- (BCoord-row destCoord) 1) (+ (BCoord-col destCoord) 2))))
    (d1r2 (piece-at board (BCoord (+ (BCoord-row destCoord) 1) (+ (BCoord-col destCoord) 2))))
    (d2r1 (piece-at board (BCoord (+ (BCoord-row destCoord) 2) (+ (BCoord-col destCoord) 1))))
    (d2l1 (piece-at board (BCoord (+ (BCoord-row destCoord) 2) (- (BCoord-col destCoord) 1))))
    (l2d1 (piece-at board (BCoord (+ (BCoord-row destCoord) 1) (- (BCoord-col destCoord) 2))))
    (l2u1 (piece-at board (BCoord (- (BCoord-row destCoord) 1) (- (BCoord-col destCoord) 2))))
  ]
  (ormap 
    (lambda (piece) 
      (and (is-piece? piece) 
      (boolean=? (Piece-isWhite piece) isWhite) 
      (string=? "knight" (Piece-name piece)))) 
    (list u2l1 u2r1 u1r2 d1r2 d2r1 d2l1 l2d1 l2u1)
  )
  )
)

(define (is-north-safe x y board kingIsWhite)
(let*
  [
    (coord (BCoord y x))
    (piece (piece-at board coord))
    (piece-name (if (is-piece? piece) (Piece-name piece) ""))
    (piece-isWhite (if (is-piece? piece) (Piece-isWhite piece) #f))
  ]
  (cond
    [(eq? piece 'nosquare)  #t]
    [(and (is-piece? piece) (or (string=? "queen" piece-name) (string=? "rook" piece-name)) (not (boolean=? kingIsWhite piece-isWhite))) #f]
    [(not (string=? "" piece-name))  #t]
    [else (is-north-safe x (sub1 y) board kingIsWhite)]
  )
)
)

(define (is-south-safe x y board kingIsWhite)
(let*
  [
    (coord (BCoord y x))
    (piece (piece-at board coord))
    (piece-name (if (is-piece? piece) (Piece-name piece) ""))
    (piece-isWhite (if (is-piece? piece) (Piece-isWhite piece) #f))
  ]
  (cond
    [(eq? piece 'nosquare)  #t]
    [(and (is-piece? piece) (or (string=? "queen" piece-name) (string=? "rook" piece-name)) (not (boolean=? kingIsWhite piece-isWhite))) #f]
    [(not (string=? "" piece-name)) #t]
    [else (is-south-safe x (add1 y) board kingIsWhite)]
  )
)
)

(define (is-west-safe x y board kingIsWhite)
(let*
  [
    (coord (BCoord y x))
    (piece (piece-at board coord))
    (piece-name (if (is-piece? piece) (Piece-name piece) ""))
    (piece-isWhite (if (is-piece? piece) (Piece-isWhite piece) #f))
  ]
  (cond
    [(eq? piece 'nosquare) #t]
    [(and (is-piece? piece) (or (string=? "queen" piece-name) (string=? "rook" piece-name)) (not (boolean=? kingIsWhite piece-isWhite))) #f]
    [(not (string=? "" piece-name)) #t]
    [else (is-west-safe (sub1 x) y board kingIsWhite)]
  )
)
)

(define (is-east-safe x y board kingIsWhite)
(let*
  [
    (coord (BCoord y x))
    (piece (piece-at board coord))
    (piece-name (if (is-piece? piece) (Piece-name piece) ""))
    (piece-isWhite (if (is-piece? piece) (Piece-isWhite piece) #f))
  ]
  (cond
    [(eq? piece 'nosquare) #t]
    [(and (is-piece? piece) (or (string=? "queen" piece-name) (string=? "rook" piece-name)) (not (boolean=? kingIsWhite piece-isWhite))) #f]
    [(not (string=? "" piece-name)) #t]
    [else (is-east-safe (add1 x) y board kingIsWhite)]
  )
)
)

(define (is-nw-safe x y board kingIsWhite)
  (let*
    ((piece (piece-at board (BCoord y x))))
    (cond
      [(eq? piece 'null) (is-nw-safe (sub1 x) (sub1 y) board kingIsWhite)]
      [(eq? piece 'nosquare) #t]
      [(boolean=? kingIsWhite (Piece-isWhite piece)) #t]
      [(or (string=? (Piece-name piece) "bishop") (string=? (Piece-name piece) "queen")) #f]
      [else #t])))

(define (is-ne-safe x y board kingIsWhite)
  (let*
    ((piece (piece-at board (BCoord y x))))
    (cond
      [(eq? piece 'null) (is-ne-safe (add1 x) (sub1 y) board kingIsWhite)]
      [(eq? piece 'nosquare) #t]
      [(boolean=? kingIsWhite (Piece-isWhite piece)) #t]
      [(or (string=? (Piece-name piece) "bishop") (string=? (Piece-name piece) "queen")) #f]
      [else #t])))

(define (is-se-safe x y board kingIsWhite)
  (let*
    ((piece (piece-at board (BCoord y x))))
    (cond
      [(eq? piece 'null) (is-se-safe (add1 x) (add1 y) board kingIsWhite)]
      [(eq? piece 'nosquare) #t]
      [(boolean=? kingIsWhite (Piece-isWhite piece)) #t]
      [(or (string=? (Piece-name piece) "bishop") (string=? (Piece-name piece) "queen")) #f]
      [else #t])))

(define (is-sw-safe x y board kingIsWhite)
  (let*
    ((piece (piece-at board (BCoord y x))))
    (cond
      [(eq? piece 'null) (is-sw-safe (sub1 x) (add1 y) board kingIsWhite)]
      [(eq? piece 'nosquare) #t]
      [(boolean=? kingIsWhite (Piece-isWhite piece)) #t]
      [(or (string=? (Piece-name piece) "bishop") (string=? (Piece-name piece) "queen")) #f]
      [else #t])))

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
  [ (board (WS-board ws))
    (startCoord (WS-firstCoord ws))
    (firstX (BCoord-col startCoord))
    (firstY (BCoord-row startCoord))
    (endX (BCoord-col destCoord))
    (endY (BCoord-row destCoord))
    (dXSigned (- endX firstX))
    (dYSigned (- endY firstY))
    (deltaX (abs (- firstX endX)))
    (deltaY (abs (- firstY endY)))
    (piece (piece-at (WS-board ws) startCoord))
    (isWhite (Piece-isWhite piece))
    (destPiece (piece-at (WS-board ws) destCoord))
    (destHasPiece (not (eq? destPiece 'null)))
    (destPieceIsOpp (and destHasPiece (not (boolean=? (Piece-isWhite destPiece) isWhite))))
    (belowCoord (BCoord (add1 (BCoord-row destCoord)) (BCoord-col destCoord)))
    (aboveCoord (BCoord (sub1 (BCoord-row destCoord)) (BCoord-col destCoord)))
    (belowPiece (piece-at board belowCoord))
    (abovePiece (piece-at board aboveCoord))
    (abovePieceCanEnPassant (and (is-piece? abovePiece) (not (boolean=? isWhite (Piece-isWhite abovePiece))) (Piece-canEnPassant? abovePiece)))
    (belowPieceCanEnPassant (and (is-piece? belowPiece) (not (boolean=? isWhite (Piece-isWhite belowPiece))) (Piece-canEnPassant? belowPiece)))
    ]
    (and (<= deltaX 1) (<= deltaY 2)
    (cond 
    ;[(and (not))]
    ;[]
    ;normal forward movement for white
    [(and isWhite (not destHasPiece) (= endY (sub1 firstY)) (= deltaX 0))  (is-up-unobstructed (sub1 firstY) destCoord board)]
    ;normal forward movement for black
    [(and (not isWhite) (not destHasPiece) (= endY (add1 firstY)) (= deltaX 0)) #t]
    ;optional 2 space move for first move - white
    [(and isWhite (not destHasPiece) (= endY (- firstY 2)) (= deltaX 0) (not (Piece-moved? piece))) (is-up-unobstructed (sub1 firstY) destCoord board)]
    ;optional 2 space move for first move - black
    [(and (not isWhite) (not destHasPiece) (= endY (+ firstY 2)) (= deltaX 0) (not (Piece-moved? piece))) (is-down-unobstructed (add1 firstY) destCoord board)]
    ; munch for white pawns
    [(and isWhite destPieceIsOpp (negative? dYSigned) (= 1 deltaX)) #t]
    ; munch for black pawns
    [(and (not isWhite) destPieceIsOpp (positive? dYSigned) (= 1 deltaX)) #t]
    ;en passant detection(up and to the right)
    [(and isWhite belowPieceCanEnPassant (= -1 dYSigned) (= 1 deltaX)) #t]
    [(and (not isWhite) abovePieceCanEnPassant (= 1 dYSigned) (= 1 deltaX)) #t]
    ;en passant detection(up and to the left)
    ;en passant detection(down and to the right)
    ;en passant detection(down and to the left)
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
    (deltaY (abs (- firstY endY)))]
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



(define (move-is-possible ws destCoord)
  (let* 
    [
    (startCoord (WS-firstCoord ws))
    (piece (piece-at (WS-board ws) startCoord))
    (piece-name (Piece-name piece))
    ]

  (cond 
  [(is-in-check (hypothetical-move ws destCoord) (WS-isWhiteTurn ws)) #f]
  [(string=? piece-name "knight") (valid-knight-move ws destCoord)]
  [(string=? piece-name "rook") (valid-rook-move ws destCoord)]
  [(string=? piece-name "bishop") (valid-bishop-move ws destCoord)]
  [(string=? piece-name "queen") (or (valid-rook-move ws destCoord) (valid-bishop-move ws destCoord))]
  [(string=? piece-name "king") (valid-king-move ws destCoord)]
  [(string=? piece-name "pawn") (valid-pawn-move ws destCoord)]
  
  ;pawn to queen detection(end of board reached) --> needs to be separate funciton that can have different parameters
  [else #t]
  
  )))

;(define (possible-moves ws coord) )
;check if a special move or special scenario is occurring or about to occur(en passant or pawn -> queen or smt)
;(define (special-moves ws coord) )

(provide handle-move)
(provide move-piece)