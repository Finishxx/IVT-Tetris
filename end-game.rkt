#lang racket

(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         test-engine/racket-gui)
(provide (all-defined-out))

;; end-game:
;; 1. top-off? ✓

;; Tet -> Bool
;; ends the game, if blocks reach y = 21 or 21 <= y
;; 1. Ends game => y => 21 #t
;; 2. Is ok => #f
(define (end-game? tet)
  (cond
    [(top-off? (tet-blocks tet)) #t]
    [else #f]))

;; ListOf(Posn)(tet-blocks) -> Bool
;; 1. y => 21 #t
;; 2. else #f
(define (top-off? blocks)
  (cond
    [(empty? blocks) #f]
    [(or (posn>=21? (first blocks))
         (top-off? (rest blocks))) #t]
    [else #f]))

;; Posn(posn-y(tet-blocks)) -> Bool
;; returns true, if y is >= 21
(define (posn>=21? pos)
  (>= (posn-y pos) 21))
