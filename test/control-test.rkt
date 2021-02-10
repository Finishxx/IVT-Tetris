#lang racket

(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "../control.rkt"
         "../const+aux.rkt")


;; control:
;; hand:
(define HAND-CTRL-EX1 (make-posn 5 10)) ;; middle
(define HAND-CTRL-EX2 (make-posn 1 10)) ;; left corner
(define HAND-CTRL-EX3 (make-posn 10 10)) ;; right corner
(define HAND-CTRL-EX4 (make-posn 3 4)) ;; where it could be blocked by blocks
;; designed with move-down in mind:
(define HAND-CTRL-EX5 (make-posn 1 1)) ;; left bottom cant move down
(define HAND-CTRL-EX6 (make-posn 1 2)) ;; left one above bottom
(define HAND-CTRL-EX7 (make-posn 5 3)) ;; middle two above bottom
;; blocks:
(define BLOCKS-CTRL-EX1 (list)) ;; no blocks
(define BLOCKS-CTRL-EX2 (list (make-posn 2 10))) ;; 1 to the right of left corner
(define BLOCKS-CTRL-EX3 (list (make-posn 9 10))) ;; 1 to the left of right corner
(define BLOCKS-CTRL-EX4 (list (make-posn 2 4) (make-posn 4 4))) ;; blocking the HAND-CTR-EX4 from both sides
;; designed with move-down in mind:
(define BLOCKS-CTRL-EX5 (list (make-posn 1 1))) ;; 1 no the left bottom
(define BLOCKS-CTRL-EX6 (list (make-posn 5 1) (make-posn 5 2))) ;; 2 on top of each other in the middle

;; OK
(define CTRL-L-EX1 (make-tet HAND-CTRL-EX1 BLOCKS-CTRL-EX1)) ;; middle on empty field
(define CTRL-L-EX2 (make-tet HAND-CTRL-EX3 BLOCKS-CTRL-EX2)) ;; right corner, blocked on the left
;; blocked                    
(define CTRL-L-EX3 (make-tet HAND-CTRL-EX3 BLOCKS-CTRL-EX3)) ;; blocked by block and wall, slams into block
(define CTRL-L-EX4 (make-tet HAND-CTRL-EX2 BLOCKS-CTRL-EX4)) ;; blocked by left corner
(define CTRL-L-EX5 (make-tet HAND-CTRL-EX4 BLOCKS-CTRL-EX4)) ;; blocked by block to the left
(define CTRL-L-EX6 (make-tet HAND-CTRL-EX2 BLOCKS-CTRL-EX1)) ;; blocked by left corner, no blocks around
;; OK
(define CTRL-R-EX1 (make-tet HAND-CTRL-EX1 BLOCKS-CTRL-EX1)) ;; middle on empty field
(define CTRL-R-EX2 (make-tet HAND-CTRL-EX2 BLOCKS-CTRL-EX3)) ;; left corner block on right corner
;; Blocked
(define CTRL-R-EX3 (make-tet HAND-CTRL-EX2 BLOCKS-CTRL-EX2)) ;;blocked by block and wall slams into block
(define CTRL-R-EX4 (make-tet HAND-CTRL-EX3 BLOCKS-CTRL-EX4)) ;; blocked by right corner
(define CTRL-R-EX5 (make-tet HAND-CTRL-EX4 BLOCKS-CTRL-EX4)) ;; blocked by block to the right
(define CTRL-R-EX6 (make-tet HAND-CTRL-EX3 BLOCKS-CTRL-EX1)) ;; blocked by right corner, no block around
;; move-down
(define CTRL-D-EX1 (make-tet HAND-CTRL-EX5 BLOCKS-CTRL-EX1)) ;; can't move down because of the floor
(define CTRL-D-EX2 (make-tet HAND-CTRL-EX6 BLOCKS-CTRL-EX5)) ;; can't move down because it is blocked by a block
(define CTRL-D-EX3 (make-tet HAND-CTRL-EX7 BLOCKS-CTRL-EX6)) ;; can't move down because it is blocked by a block

;; blocked by all sides
(define CTRL-ALL-SIDES (make-tet (make-posn 5 3)
                             (list (make-posn 4 3) (make-posn 4 2) (make-posn 4 1) (make-posn 6 3)  (make-posn 6 2) (make-posn 6 1) (make-posn 5 1))))



(test/gui
 (test-suite
  "Control"
  (test-suite
   "control"
   (test-suite
    "OK"
    (test-case "move left in middle, not blocked"
               
               (control CTRL-L-EX1 "left")
               (make-tet (make-posn 4 10) (list)))
    (test-case "move left in right corner, block on opposite side, not blocked"
               (control CTRL-L-EX2 "left")
               (make-tet (make-posn 9 10) (list (make-posn 2 10))))
    (test-case "move right in middle, not blocked"
               (control CTRL-R-EX1 "right")
               (make-tet (make-posn 6 10) (list)))
    (test-case "move right on the left block on opposite side, not blocked"
               (control CTRL-R-EX2 "right")
               (make-tet (make-posn 2 10) (list (make-posn 9 10)))))
   (test-suite
    "Blocked by wall"
    (test-case "move left blocked by left corner"
               (control CTRL-L-EX4 "left")
               CTRL-L-EX4)
    (test-case "move left blocked by left corner on empty field"
               (control CTRL-L-EX6 "left")
               CTRL-L-EX6)
    (test-case "move right blocked by right corner"
               (control CTRL-R-EX4 "right")
               CTRL-R-EX4)
    (test-case "move right blocked by right corner on empty field"
               (control CTRL-R-EX6 "right")
               CTRL-R-EX6))
   (test-suite
    "Blocked from block/both sides"
    (test-case "move right blocked from all sides by blocks"
               (control CTRL-ALL-SIDES "right")
               CTRL-ALL-SIDES)
    (test-case "move left blocked from all sides by blocks"
               (control CTRL-ALL-SIDES "left")
               CTRL-ALL-SIDES)
    (test-case "move left blocked by left corner"
               (control CTRL-L-EX3 "left")
               CTRL-L-EX3)
    (test-case "move left blocked by block"
               (control CTRL-L-EX5 "left")
               CTRL-L-EX5)
    (test-case "move right blocked by block"
               (control CTRL-R-EX3 "right")
               CTRL-R-EX3)
    (test-case "move right blocked by block"
               (control CTRL-R-EX5 "right")
               CTRL-R-EX5)))
   (test-suite
    "Move-down"
    (test-case "can't move because of the floor"
               (control CTRL-D-EX1 "down")
               (make-tet (make-posn 5 22) (list (make-posn 1 1)))) ;; add positive outcomes too
    (test-case "can't move because of block one up"
               (control CTRL-D-EX2 "down")
               (make-tet (make-posn 5 22) (list (make-posn 1 1) (make-posn 1 2))))
    (test-case "can't move because of block two up"
               (control CTRL-D-EX3 "down")
               (make-tet (make-posn 5 22) (list (make-posn 5 1) (make-posn 5 2) (make-posn 5 3)))))
    
   (test-suite
    "Check left"
    (test-case "middle of the field, passes"
               (check-left HAND-CTRL-EX1 BLOCKS-CTRL-EX1)
               #t)
    (test-case "right corner, passes"
               (check-left HAND-CTRL-EX3 BLOCKS-CTRL-EX2)
               #t)
    (test-case "right corner, blocked by block"
               (check-left HAND-CTRL-EX3 BLOCKS-CTRL-EX3)
               #f)
    (test-case "left corner, blocked by corner"
               (check-left HAND-CTRL-EX2 BLOCKS-CTRL-EX4)
               #f)
    (test-case "somewhere, blocked by block"
               (check-left HAND-CTRL-EX4 BLOCKS-CTRL-EX4)
               #f)
    (test-case "left corner blocked by corner"
               (check-left HAND-CTRL-EX2 BLOCKS-CTRL-EX1)
               #f))
   (test-suite
    "Check-right"
    (test-case "middle of the field, passes"
               (check-right HAND-CTRL-EX1 BLOCKS-CTRL-EX1)
               #t)
    (test-case "left corner, passes"
               (check-right HAND-CTRL-EX2 BLOCKS-CTRL-EX3)
               #t)
    (test-case "left corner, blocked by block"
               (check-right HAND-CTRL-EX2 BLOCKS-CTRL-EX2)
               #f)
    (test-case "right corner, blocked by corner"
               (check-right HAND-CTRL-EX3 BLOCKS-CTRL-EX4)
               #f)
    (test-case "somewhere, blocked by block"
               (check-right HAND-CTRL-EX4 BLOCKS-CTRL-EX4)
               #f)
    (test-case "right corner, blocked by corner"
               (check-right HAND-CTRL-EX3 BLOCKS-CTRL-EX1)
               #f))
   (test-suite
    "Move left"
    (test-case "5 10 -> 4 10"
               (move-left HAND-CTRL-EX1)
               (make-posn 4 10))
    (test-case "10 10 -> 9 10"
               (move-left HAND-CTRL-EX3)
               (make-posn 9 10))
    (test-case "3 4 -> 2 4"
               (move-left HAND-CTRL-EX4)
               (make-posn 2 4)))
   (test-suite
    "Move right"
    (test-case "5 10 -> 6 10"
               (move-right HAND-CTRL-EX1)
               (make-posn 6 10))
    (test-case "1 10 -> 2 10"
               (move-right HAND-CTRL-EX2)
               (make-posn 2 10))
    (test-case "3 4 -> 4 4"
               (move-right HAND-CTRL-EX4)
               (make-posn 4 4)))
   (test-suite
    "Move down"
    (test-case "5 10 -> 5 9 on empty field"
               (move-down HAND-CTRL-EX1 BLOCKS-CTRL-EX1)
               (make-tet (make-posn 5 9) (list)))
    (test-case "10 10 -> 10 9 not blocked"
               (move-down HAND-CTRL-EX3 BLOCKS-CTRL-EX2)
               (make-tet (make-posn 10 9) (list (make-posn 2 10))))
    (test-case "1 10 -> 1 9 not blocked"
               (move-down HAND-CTRL-EX2 BLOCKS-CTRL-EX3)
               (make-tet (make-posn 1 9) (list (make-posn  9 10))))
    (test-case "3 4 -> 3 3 not blocked"
               (move-down HAND-CTRL-EX4 BLOCKS-CTRL-EX2)
               (make-tet (make-posn 3 3) (list (make-posn 2 10))))
    (test-case "new block, hit bottom on empty field"
               (move-down HAND-CTRL-EX5 BLOCKS-CTRL-EX1)
               (make-tet (make-posn 5 22) (list (make-posn 1 1))))
    (test-case "new block, hit block one above bottom"
               (move-down HAND-CTRL-EX6 BLOCKS-CTRL-EX5)
               (make-tet (make-posn 5 22) (list (make-posn 1 1) (make-posn 1 2))))
    (test-case "new block, hit block two above bottom"
               (move-down HAND-CTRL-EX7 BLOCKS-CTRL-EX6)
               (make-tet (make-posn 5 22) (list (make-posn 5 1) (make-posn 5 2) (make-posn 5 3)))))))
               
    
               




       
