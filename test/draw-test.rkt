#lang racket
(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "../draw.rkt"
         "../const+aux.rkt")

;; schematic->actual
(define S->A-EX1 (list (make-posn 1 1) (make-posn 10 1)))
(define S->A-EX2 (list (make-posn 1 20) (make-posn 10 20)))
(define S->A-EX3 (list (make-posn 1 10) (make-posn 10 10)))
(define S->A-EX4 (list (make-posn 5 1) (make-posn 5 20)))
;; block-list
(define BL-EX1 1)
(define BL-EX2 5)
(define BL-EX3 7)
;; draw-blocks + draw
(define DB-EX1 (make-tet (make-posn 3 4) S->A-EX1))
(define DB-EX2 (make-tet (make-posn 6 8) S->A-EX2))
(define DB-EX3 (make-tet (make-posn 2 2) S->A-EX3))
(define DB-EX4 (make-tet (make-posn 4 15) S->A-EX4))


(test/gui
 (test-suite
  "Schemati->Actual"
  (test-case "pos 1 1, pos 10 1"
             (check-equal? (schematic->actual S->A-EX1)
                           (list (make-posn 55 247) (make-posn 145 247))))
  (test-case "pos 1 20, pos 10 20"
             (check-equal? (schematic->actual S->A-EX2)
                           (list (make-posn 55 57) (make-posn 145 57))
                           ))
  (test-case "pos 1 10, pos 10 10"
             (check-equal? (schematic->actual S->A-EX3)
                           (list (make-posn 55 157) (make-posn 145 157))
                           ))
  (test-case "pos 5 1, pos 5 20"
             (check-equal? (schematic->actual S->A-EX4)
                           (list (make-posn 95 247) (make-posn 95 57))
                           )))
 
 (test-suite
  "Block-list"
  (test-case "1 block"
             (check-equal? (block-list BL-EX1)
                           (list BLOCK)
                           ))
  (test-case "5 blocks"
             (check-equal? (block-list BL-EX2)
                           (list BLOCK BLOCK BLOCK BLOCK BLOCK)
                           ))
  (test-case "7 blocks"
             (check-equal? (block-list BL-EX3)
                           (list BLOCK BLOCK BLOCK BLOCK BLOCK BLOCK BLOCK)
                           )))
 
 (test-suite
  "Draw-blocks"
 
  (test-case "hand: 3,4 blocks: 1,1; 10,1" (check-equal? (draw-blocks DB-EX1 LAYOUT)
                                                         (place-images
                                                          (list
                                                           BLOCK
                                                           BLOCK
                                                           BLOCK)
                                                          (list
                                                           (make-posn 55 247) (make-posn 145 247)
                                                           (make-posn 75 217))
                                                          LAYOUT)
                                                         ))
  (test-case "hand: 6,8 blocks: 1,20; 10,20"
             (check-equal? (draw-blocks DB-EX2 LAYOUT)
                           (place-images
                            (list
                             BLOCK
                             BLOCK
                             BLOCK)
                            (list
                             (make-posn 55 57) (make-posn 145 57)
                             (make-posn 105 177))
                            LAYOUT)
                           ))
  (test-case "hand: 2,2 blocks: 1,10; 10,10"
             (check-equal? (draw-blocks DB-EX3 LAYOUT)
                           (place-images
                            (list
                             BLOCK
                             BLOCK
                             BLOCK)
                            (list
                             (make-posn 55 157) (make-posn 145 157)
                             (make-posn 65 237))
                            LAYOUT)
                           ))
  (test-case "hand: 4,15 blocks: 5,1; 5,20"
             (check-equal? (draw-blocks DB-EX4 LAYOUT)
                           (place-images
                            (list
                             BLOCK
                             BLOCK
                             BLOCK)
                            (list
                             (make-posn 95 247) (make-posn 95 57)
                             (make-posn 85 107))
                            LAYOUT)
                           ))))
 



         