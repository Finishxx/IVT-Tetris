#lang racket

(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         test-engine/racket-gui)
(provide (all-defined-out))


;; conds returning bools don't need to have result-expr, but they can't be in tail position!!

;; tock:
;; 1. is-blocked? ✓
;; 2. fall ✓
;; 3. block ✓
;; 4. aux-blocked? ✓
;; 5. posn-equal? ✓
;; 6. clear-row? 


;; Tet -> Tet
;; Depending on the posn of the tet-hand block
;; 1. tet-hand can fall down and there isn't anything obstructing him -> change posn-y by -1
;; 2. tet-hand has a block below it and it is obstructed -> append tet-hand to tet-blocks and spawn a new block on posn(5 22)
(define (tock tet)
  (cond
    [(is-blocked? (tet-hand tet) (tet-blocks tet))
     (make-tet (make-posn 5 22) (cond
                                  [(clear-row? (tet-hand tet) (tet-blocks tet)) (clear-row! (tet-hand tet) (tet-blocks tet))]
                                  [else (block (tet-hand tet) (tet-blocks tet))]))]
    [else
     (make-tet (fall (tet-hand tet)) (tet-blocks tet))]))



;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; Returns true if either one of these is true:
;; 1. The block is at position (x 1)
;; 2. The block is directly above any of the blocks -> tet-hand(x y+1) tet-blocks(x y)
;; Otherwise returns false
(define (is-blocked? hand blocks)
  (cond
    [(= (posn-y hand) 1) #t]
    [(aux-blocked? (make-posn (posn-x hand) (- (posn-y hand) 1)) blocks) #t] ;; the hard part
    [else #f]))



;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; checks if hand(x,y-1) is equal to the list of blocks, returns true if it is
(define (aux-blocked? hand-1 blocks)
  (cond
    [(empty? blocks) #f]
    [(or (posn-equal? hand-1 (first blocks))
         (aux-blocked? hand-1 (rest blocks)))]
    [else #f]))


;; Posn Posn -> Bool
;; compares the posn values to return #t if they are equal or #f if they are not
(define (posn-equal? hand blocks)
  (and (= (posn-x hand) (posn-x blocks)) (= (posn-y hand) (posn-y blocks))))


;; Posn(tet-hand) -> Posn(tet-hand)
;; changes the value for tet-hand posn(x, y-1)
(define (fall hand)
  (make-posn (posn-x hand) (- (posn-y hand) 1)))



;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> ListOf(Posn)(tet-blocks)
;; appends the value of tet-hand to list tet-blocks and returns new tet-blocks
(define (block hand blocks)
  (append blocks (list hand)))
  

;; Posn(hand) ListOf(posn)(blocks) -> Bool
;; checks if a row of 10 is cleared
;; checks the row on the hand-x if it is full
(define (clear-row? hand blocks)
  #f)




;; Posn(hand) ListOf(posn)(blocks) -> ListOf(posn)(blocks)
;; clears a row of 10 and lets the other blocks fall down
;; every block higher than hand(x y) goes (x y-1)
(define (clear-row! hand blocks)
  #t)



