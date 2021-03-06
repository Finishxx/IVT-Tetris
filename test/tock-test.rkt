#lang racket

(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "../tock.rkt"
         "../const+aux.rkt"
         racket/struct
         "../tetriminos.rkt")
(provide THREE-ROW ONE-ROW)

;; need tests for tock

;; tock:
;; falls:
(define TOCK-F-EX1 (make-tet (list (make-block (make-posn 5 21) "light blue")) (list) DEFAULT-BAG 0))
(define TOCK-F-EX2 (make-tet (list (make-block (make-posn 5 3) "light blue")) (list (make-block (make-posn 5 1) "light blue")) DEFAULT-BAG 0))
(define TOCK-F-EX3 (make-tet (make-posn 1 10) (list (make-posn 1 8)) #t #t))
(define TOCK-F-EX4 (make-tet (make-posn 5 3)
                             (list (make-posn 4 3) (make-posn 4 2) (make-posn 4 1) (make-posn 6 3)  (make-posn 6 2) (make-posn 6 1) (make-posn 5 1))
                             #t #t))

;; is blocked:
(define TOCK-B-EX1 (make-tet (make-posn 1 2) (list (make-posn 1 1)) #t #t))
(define TOCK-B-EX2 (make-tet (make-posn 5 1) (list) #t #t))
(define TOCK-B-EX3 (make-tet (make-posn 1 1) (list) #t #t))
(define TOCK-B-EX4 (make-tet (make-posn 5 15) (list (make-posn 5 14)) #t #t))
(define TOCK-B-EX5 (make-tet (make-posn 10 2) (list (make-posn 10 1)) #t #t))

;; aux for building rows
;; Num Num Num List -> ListOfPosn
(define (make-row x y max lst)
  (cond
    [(and (<= 10 x) (= y max)) lst]
    [(<= 10 x) (make-row 0 (+ 1 y) max lst)]
    [else (make-row (+ 1 x) y max (append (list (make-block (make-posn (+ x 1) y) "red") lst)))]))

;; clear-row?/clear-row!
(define ONE-ROW (flatten (make-row 0 1 1 (list))))
(define TWO-ROW (flatten (make-row 0 1 2 (list))))
(define THREE-ROW (flatten (make-row 0 1 3 (list))))
(define ONLY-SECOND-ROW (flatten (make-row 0 2 2 (list))))
(define ONLY-THIRD-ROW (flatten (make-row 0 3 3 (list))))


;; easy sorting
(define (easy-sort blocks)
  (sort-out blocks (list) 1))

;; aux for sorting things for tests
;; ListOfPosn List Num -> ListOfPosn
;; sorts the list from the bottom and left by rows
(define (sort-out blocks sorted y)
  (cond
    [(> y (posn-y (block-posn (highest-block blocks)))) sorted]
    [else (sort-out
           blocks
           (append (sort (select-row blocks y)  (lambda (x y)
                                          (< (posn-x (block-posn x)) (posn-x (block-posn y)))))
                   sorted)
           (+ y 1))]))


;; aux for sorting things for tests
;; ListOfPosn -> Num
;; returns the highest y position of a block from the list
(define (highest-block blocks)
  (last (sort blocks (lambda (x y)
                       (< (posn-y (block-posn x)) (posn-y (block-posn y)))))))

(test/gui
 (test-suite
  "Tock"
  (test-suite
   "Is-blocked?"
   (test-suite
    "Falls"
    (test-equal? "T in air"
               (is-blocked? T (list))
               #f)
    (test-equal? "I one above L"
               (is-blocked? (block-placement I (make-posn 0 -16))
                            (block-placement L (make-posn 0 -19)))
               #f)
    (test-equal? "O one above ground"
               (is-blocked? (block-placement O (make-posn 0 -19))
                            (list))
               #f))
   (test-suite
    "Blocked"
    (test-equal? "Z on ground"
               (is-blocked? (block-placement Z (make-posn 0 -20))
                            (list))
               #t)
    (test-equal? "S on top of J"
               (is-blocked? (block-placement S (make-posn 0 -18))
                            (block-placement J (make-posn 0 -20)))
               #t)
    (test-equal? "O on top of O in air"
               (is-blocked? (block-placement O (make-posn -1 -18))
                            (block-placement O (make-posn 0 -20)))
               #t))
  (test-suite
   "Aux-blocked?"
   (test-equal? "I, J"
                (aux-blocked? I J)
                #t)
   (test-equal? "T+3x-2y, Z-3x-2y"
                (aux-blocked?
                 (block-placement T (make-posn 3 -2))
                 (block-placement Z (make-posn -3 -2)))
                #f)
   (test-equal? "L-5y, J-5y"
                (aux-blocked?
                 (block-placement L (make-posn 0 -5))
                 (block-placement J (make-posn 0 -5)))                          
                #t))
  (test-suite
   "is-bottom?"
   (test-equal? "I at bottom"
                (is-bottom? (list (make-block (make-posn 5 1) "light blue")
                                  (make-block (make-posn 5 2) "light blue")
                                  (make-block (make-posn 5 3) "light blue")
                                  (make-block (make-posn 5 4) "light blue")))
                #t)
   (test-equal? "S not bottom"
                (is-bottom? (list (make-block (make-posn 4 3) "green")
                                 (make-block (make-posn 5 3) "green")
                                 (make-block (make-posn 5 2) "green")
                                 (make-block (make-posn 6 2) "green")))
                #f)
   (test-equal? "O at bottom"
                (is-bottom? (list (make-block (make-posn 9 2) "yellow")
                                 (make-block (make-posn 10 2) "yellow")
                                 (make-block (make-posn 9 1) "yellow")
                                 (make-block (make-posn 10 1) "yellow")))
                #t))
  (test-suite
   "Posn-equal?"
   (test-equal? "I, block in I"
              (posn-equal? I (make-block (make-posn 7 21) "does not matter"))
              #t)
   (test-equal? "I at bottom, block not in I"
              (posn-equal? (block-placement I (make-posn 0 -10))
                           (make-block (make-posn 5 22) "haha"))
              #f)
   (test-equal? "S -10y, second Z -10y"
              (posn-equal? (block-placement S (make-posn 0 -10))
                           (second (block-placement Z (make-posn 0 -10))))
              #t)))
  (test-suite
   "Fall"
   (test-equal? "I"
                (fall I)
                (list (make-block (make-posn 4 20) "light blue")
                      (make-block (make-posn 5 20) "light blue")
                      (make-block (make-posn 6 20) "light blue")
                      (make-block (make-posn 7 20) "light blue")))
   (test-equal? "O"
                (fall O)
                (list (make-block (make-posn 5 21) "yellow")
                      (make-block (make-posn 6 21) "yellow")
                      (make-block (make-posn 5 20) "yellow")
                      (make-block (make-posn 6 20) "yellow")))
   (test-equal? "S"
                (fall S)
                (list (make-block (make-posn 5 21) "green")
                      (make-block (make-posn 6 21) "green")
                      (make-block (make-posn 4 20) "green")
                      (make-block (make-posn 5 20) "green"))))
  (test-suite
   "Block!"
   (test-equal? "I at bottom to J at bottom"
              (block! (block-placement I (make-posn -3 -20))
                      (block-placement J (make-posn 0 -20)))
              (append (block-placement I (make-posn -3 -20))
                      (block-placement J (make-posn 0 -20))))
   (test-equal? "O at bottom"
              (block! (block-placement O (make-posn 0 -20))
                      (list))
              (block-placement O (make-posn 0 -20)))
   (test-equal? "S on top of Z"
              (block! (block-placement S (make-posn 0 -18))
                      (block-placement Z (make-posn 0 -20)))
              (append (block-placement S (make-posn 0 -18))
                      (block-placement Z (make-posn 0 -20)))))
  (test-suite
   "block-row"
   (test-equal? "T at bottom, empty, default bag, 0"
                (block-row (block-placement T (make-posn 0 -20)) (list) DEFAULT-BAG 0)
                (make-tet I (block-placement T (make-posn 0 -20)) (rest DEFAULT-BAG) 0))
   (test-equal? "I, 1O 1I (clear), default bag, 0"
                (block-row (block-placement I (make-posn -3 -20))
                           (append (block-placement O (make-posn 0 -20))
                                   (block-placement I (make-posn +3 -20)))
                           DEFAULT-BAG 0)
                (make-tet I (list (make-block (make-posn 5 1) "yellow")
                                  (make-block (make-posn 6 1) "yellow"))
                          (rest DEFAULT-BAG) 100)))
  (test-suite
   "Clear-row?"
   (test-equal? "1 row; 1 -> #t"
              (clear-row? ONE-ROW 1)
              #t)
   (test-equal? "2 row; 1 -> #t"
              (clear-row? TWO-ROW 1)
              #t)
   (test-equal? "3 row; 3 -> #t"
              (clear-row? THREE-ROW 3)
              #t)
   (test-equal? "1 row; 2 -> #f"
              (clear-row? ONE-ROW 2)
              #f)
   (test-equal? "3 row; 4 -> #f"
              (clear-row? THREE-ROW 4)
              #f))
  (test-suite
   "is-ten?"
   (test-equal? "1 row; 1 -> #t"
              (is-ten? ONE-ROW 1)
              #t)
   (test-equal? "2 row; 1 -> #t"
              (is-ten? TWO-ROW 1)
              #t)
   (test-equal? "1 row; 2 -> #f"
              (is-ten? ONE-ROW 2)
              #f)
   (test-equal? "3 row; 4 -> #f"
              (is-ten? THREE-ROW 4)
              #f)
   (test-equal? "only second row; 3 -> #f"
                (is-ten? ONLY-SECOND-ROW 3)
                #f))
  (test-suite
   "select-row"
   (test-equal? "1 row; 1 -> #t"
              (select-row ONE-ROW 1)
              ONE-ROW)
   (test-equal? "1 row; 2 -> #f"
              (select-row ONE-ROW 2)
              (list))
   (test-equal? "3 row; 4 -> #f"
              (select-row ONLY-THIRD-ROW 4)
              (list))
   (test-equal? "only second row; 3 -> #f"
                (select-row ONLY-SECOND-ROW 3)
                (list))
   (test-equal? "only second row; 2 -> #t"
                (select-row ONLY-SECOND-ROW 2)
                ONLY-SECOND-ROW))
   
  (test-suite
   "Clear-row!"
   (test-suite
    "clear-row!"
    (test-equal? "1 row -> empty"
                (clear-row! ONE-ROW 1)
                (list))
   (test-equal? "2 row -> empty"
                (clear-row! TWO-ROW 1)
                (list))
   (test-equal? "9 in row + 1 in hand -> empty"
                (clear-row! (append (list (make-block (make-posn 10 1) "red")) (rest ONE-ROW)) 1)
                (list))
   (test-equal? "3 row -> empty"
                (clear-row! THREE-ROW 1)
                (list)))
   (test-suite
    "kill-row"
    (test-equal? "1 row; 1 row -> empty"
               (kill-row ONE-ROW ONE-ROW)
               (list))
    (test-equal? "2 row; first row -> only 2nd row"
                 (kill-row TWO-ROW ONE-ROW)
                 ONLY-SECOND-ROW)
    (test-equal? "3 row; second row -> only 1st and 3rd row"
                 (kill-row THREE-ROW ONLY-SECOND-ROW)
                 (append ONLY-THIRD-ROW ONE-ROW)))
   (test-suite
    "which-row"
    (test-equal? "1 row -> 1"
                 (which-row ONE-ROW 1)
                 1)
    (test-equal? "3 row -> 3"
                 (which-row THREE-ROW 1)
                 1)
    (test-equal? "only 2nd row -> 2"
                 (which-row ONLY-SECOND-ROW 1)
                 2))
   (test-suite
    "fall-down"
    (test-equal? "2nd row -> first row"
                 (fall-down ONLY-SECOND-ROW)
                 ONE-ROW)
    (test-equal? "3rd row -> second row"
                 (fall-down ONLY-THIRD-ROW)
                 ONLY-SECOND-ROW))
   (test-suite
    "move-row"
    (test-equal? "1st and 3rd row; 2 -> 2 row"
                 (easy-sort (move-row (append ONE-ROW ONLY-THIRD-ROW) 2))
                 (easy-sort TWO-ROW))
    (test-equal? "2nd and 3rd row; 1 -> 2 row"
                 (easy-sort (move-row (append ONLY-SECOND-ROW ONLY-THIRD-ROW) 1))
                 (easy-sort TWO-ROW)))
   (test-suite
    "select-above"
    (test-equal? "3 row; 2 -> 3rd+2nd row"
                 (easy-sort (select-above THREE-ROW (list) 2))
                 (easy-sort (append ONLY-SECOND-ROW ONLY-THIRD-ROW))))
   (test-suite
    "select-below"
    (test-equal? "3 row; 3 -> 3 row"
                 (easy-sort (select-below THREE-ROW (list) 3))
                 (easy-sort THREE-ROW))
    (test-equal? "3 row; 2 -> 2 row"
                 (easy-sort (select-below THREE-ROW (list) 2))
                 (easy-sort TWO-ROW)))
   (test-suite
    "update-score"
    (test-equal? "Empty 100"
                 (update-score (list) 100)
                 100)
    (test-equal? "ONE ROW 0"
                 (update-score ONE-ROW 0)
                 100)
    (test-equal? "THREE ROW 500"
                 (update-score THREE-ROW 500)
                 800)))))



