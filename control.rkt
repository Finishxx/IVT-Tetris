#lang racket

(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         test-engine/racket-gui
         "tock.rkt")
(provide (all-defined-out))

;; CTRL:
;; 1. check-left ✓
;; 2. check-right ✓
;; 3. move-left ✓
;; 4. move-right ✓

;; Tet Ke -> Tet
;; moves the tetrimono left or right if there is nothing blocking it's path
;; 1. "left" - moves the tetrimono one left on the grid, does not move if it is at the left border or there is another tetrimono one to the left
;; 2. "right" - moves the tetrimono one right on the grid, does not move if it is a the right border or there is another tetrimono one to the right
;; 3. "down" - moves the block to the lowest viable position on the same x
(define (control tet ke)
  (cond
    [(and (string=? ke "left") (check-left (tet-hand tet) (tet-blocks tet))) (make-tet (move-left (tet-hand tet)) (tet-blocks tet))]
    [(and (string=? ke "right") (check-right (tet-hand tet) (tet-blocks tet))) (make-tet (move-right (tet-hand tet)) (tet-blocks tet))]
    [(string=? ke "down") (move-down (tet-hand tet) (tet-blocks tet))]
    [else tet]))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; returns false, if any of the tet-blocks are tet-hand(x-1,y)or if they are on the border
;; 1. there is -> false
;; 2. there isn't -> true
;; 3. empty list -> true
(define (check-left hand blocks)
  (cond
    [(= (posn-x hand) 1) #f]
    [(aux-blocked? (make-posn (- (posn-x hand) 1) (posn-y hand)) blocks) #f]
    [else #t]))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; returns false, if any of the tet-blocks are tet-hand(x+1,y)
;; 1. there is -> false
;; 2. there isn't -> true
;; 3. empty list -> true
(define (check-right hand blocks)
  (cond
    [(= (posn-x hand) 10) #f]
    [(aux-blocked? (make-posn (+ (posn-x hand) 1) (posn-y hand)) blocks) #f]
    [else #t]))

;; Posn(hand) -> Posn
;; substracts 1 from posn-x and moves the block left
(define (move-left hand)
  (make-posn (- (posn-x hand) 1) (posn-y hand)))

;; Posn(hand) -> Posn
;; adds 1 to posn-x and moves the block left
(define (move-right hand)
  (make-posn (+ (posn-x hand) 1) (posn-y hand)))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks)-> Tet
;; Moves the tet-hand block one down if not blocked, if so sticks it to the tet-blocks
;; 1. There are no blocks on the x coordinate -> (x 1)
;; 2. There are some blocks -> Depends
(define (move-down hand blocks)
  (cond
    [(is-blocked? hand blocks) (block-row hand blocks)]
    [else (make-tet (make-posn (posn-x hand) (- (posn-y hand) 1)) blocks)]))
