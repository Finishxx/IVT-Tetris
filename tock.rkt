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
     (block-row (tet-hand tet) (tet-blocks tet))]
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


;; Posn ListOfPosn -> Tet
;; spawns a new block and puts the previou one on the field or clears row with it
(define (block-row hand blocks)
  (make-tet (make-posn 5 22) (cond
                                  [(clear-row? (append (list hand) blocks) 0)
                                   (clear-row! (append (list hand) blocks) (which-row (append (list hand) blocks) 1))]
                                  [else (block hand blocks)])))



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
  

;; ListOf(posn)(blocks) Num -> Bool
;; checks if a row of 10 is cleared
;; checks the row on the hand-x if it is full
(define (clear-row? blocks y)
  (cond
    [(is-ten? blocks y) #t]
    [(>= y 20) #f]
    [else (clear-row? blocks (+ y 1))]))

;; ListOf(posn)(blocks) Num -> ListOf(posn)(blocks)
;; clears a row of 10 and lets the other blocks fall down
;; every block higher than hand(x y) goes (x y-1)
(define (clear-row! blocks target)
  (cond
    [(number? target)
     (clear-row! (dirty-work blocks target) (which-row (dirty-work blocks target) 1))]
    [else blocks]))

(define (dirty-work blocks target)
  (move-row (kill-row blocks (select-row blocks target)) target))

;; ListOfPosn Num -> Num
;; returns y pos of the first row from bottom that is full
(define (which-row blocks y)
  (cond
    [(is-ten? blocks y) y]
    [(>= y 20) #f]
    [else (which-row blocks (+ y 1))]))

;; ListOfPosn Num -> Bool
;; checks if there is 10 blocks on one row given the blocks and row num
(define (is-ten? blocks y)
  (= 10 (length (select-row blocks y))))

;; ListOfPosn Num -> ListOfPosn
;; selects all blocks that are in y row from blocks
(define (select-row blocks y)
  (filter (lambda (arg) (= (posn-y arg) y)) blocks))


;; ListOfPosn ListOfPosn Num -> ListOfPosn
;; select all rows above y and stores them into rows
(define (select-above blocks rows y)
  (cond [(< y 22) (select-above blocks (append rows (select-row blocks y)) (+ 1 y))]
        [else rows]))

(define (select-below blocks rows y)
  (cond [(> y 0) (select-below blocks (append rows (select-row blocks y)) (- y 1))]
        [else rows]))


;; ListOfPosn ListOfPosn -> ListOfPosn
;; given a list of blocks deletes them
(define (kill-row blocks row)
  (remove* row blocks))


;; ListOfPosn Num -> ListOfPosn
;; given a list of blocks moves all above the y (posn x y-1)
;; selects the row above the deleted one and goes on untill row 21
(define (move-row blocks y)
  (append (select-below blocks (list) y) (fall-down (select-above blocks (list) y))))

;; ListOfPosn
;; Given a row of blocks moves them down by (posn x -1)
(define (fall-down blocks)
  (map (lambda (arg)
         (make-posn (posn-x arg) (- (posn-y arg) 1))) blocks))