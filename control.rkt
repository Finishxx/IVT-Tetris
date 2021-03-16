#lang racket

(require 2htdp/image
         "const+aux.rkt"
         lang/posn
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
    [(and (string=? ke "left") (check-left (tet-hand tet) (tet-blocks tet)))
     (make-tet (move-left (tet-hand tet)) (tet-blocks tet) (tet-bag tet) (tet-score tet))]
    [(and (string=? ke "right") (check-right (tet-hand tet) (tet-blocks tet)))
     (make-tet (move-right (tet-hand tet)) (tet-blocks tet) (tet-bag tet) (tet-score tet))]
    [(string=? ke "down")
     (move-down (tet-hand tet) (tet-blocks tet) (tet-bag tet) (tet-score tet))]
    [else tet]))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; returns false, if any of the tet-blocks are tet-hand(x-1,y)or if they are on the border
;; 1. there is -> false
;; 2. there isn't -> true
;; 3. empty list -> true
(define (check-left hand blocks)
  (cond
    [(is-at-x? hand 1) #f]
    [(aux-blocked? (move-left hand) blocks) #f]
    [else #t]))

;; ListOfBlocks Num -> Bool
;; checks if any block of Hand is at x
(define (is-at-x? hand x)
  (ormap (lambda (block)
           (= (posn-x (block-posn block)) x))
         hand))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; returns false, if any of the tet-blocks are tet-hand(x+1,y)
;; 1. there is -> false
;; 2. there isn't -> true
;; 3. empty list -> true
(define (check-right hand blocks)
  (cond
    [(is-at-x? hand 10) #f]
    [(aux-blocked? (move-right hand) blocks) #f]
    [else #t]))

;; Posn(hand) -> Posn
;; substracts 1 from posn-x and moves the block left
(define (move-left hand)
  (block-placement hand (make-posn -1 0)))

;; Posn(hand) -> Posn
;; adds 1 to posn-x and moves the block right
(define (move-right hand)
  (block-placement hand (make-posn 1 0)))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks)-> Tet
;; Moves the tet-hand block one down if not blocked, if so sticks it to the tet-blocks
;; 1. There are no blocks on the x coordinate -> (x 1)
;; 2. There are some blocks -> Depends
(define (move-down hand blocks bag score)
  (cond
    [(is-blocked? hand blocks) (block-row hand blocks bag score)]
    [else (make-tet
           (block-placement hand (make-posn 0 -1))
           blocks bag score)]))
