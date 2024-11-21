#lang racket
(define scalar .75)
(define (int-floor x) (inexact->exact (floor x)))
(provide int-floor)
(provide scalar)