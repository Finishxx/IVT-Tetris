#lang racket

(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "../tock.rkt"
         "../const+aux.rkt"
         racket/struct)


;; tock:
;; falls:
(define TOCK-F-EX1 (make-tet (make-posn 5 21) (list)))
(define TOCK-F-EX2 (make-tet (make-posn 5 3) (list (make-posn 5 1))))
(define TOCK-F-EX3 (make-tet (make-posn 1 10) (list (make-posn 1 8))))
(define TOCK-F-EX4 (make-tet (make-posn 5 3)
                             (list (make-posn 4 3) (make-posn 4 2) (make-posn 4 1) (make-posn 6 3)  (make-posn 6 2) (make-posn 6 1) (make-posn 5 1))))

;; is blocked:
(define TOCK-B-EX1 (make-tet (make-posn 1 2) (list (make-posn 1 1))))
(define TOCK-B-EX2 (make-tet (make-posn 5 1) (list)))
(define TOCK-B-EX3 (make-tet (make-posn 1 1) (list)))
(define TOCK-B-EX4 (make-tet (make-posn 5 15) (list (make-posn 5 14))))
(define TOCK-B-EX5 (make-tet (make-posn 10 2) (list (make-posn 10 1))))

;; aux for building rows
;; Num Num Num List -> ListOfPosn
(define (make-row x y max lst)
  (cond
    [(and (<= 10 x) (= y max)) lst]
    [(<= 10 x) (make-row 0 (+ 1 y) max lst)]
    [else (make-row (+ 1 x) y max (append (list (make-posn (+ x 1) y)) lst))]))

;; clear-row?/clear-row!
(define ONE-ROW (make-row 0 1 1 (list)))
(define TWO-ROW (make-row 0 1 2 (list)))
(define THREE-ROW (make-row 0 1 3 (list)))
(define ONLY-SECOND-ROW (make-row 0 2 2 (list)))
(define ONLY-THIRD-ROW (make-row 0 3 3 (list)))


;; easy sorting
(define (easy-sort blocks)
  (sort-out blocks (list) 1))

;; aux for sorting things for tests
;; ListOfPosn List Num -> ListOfPosn
;; sorts the list from the bottom and left by rows
(define (sort-out blocks sorted y)
  (cond
    [(> y (posn-y (highest-block blocks))) sorted]
    [else (sort-out
           blocks
           (append (sort (select-row blocks y)  (lambda (x y)
                                          (< (posn-x x) (posn-x y))))
                   sorted)
           (+ y 1))]))


;; aux for sorting things for tests
;; ListOfPosn -> Num
;; returns the highest y position of a block from the list
(define (highest-block blocks)
  (last (sort blocks (lambda (x y)
                       (< (posn-y x) (posn-y y))))))

(test/gui
 (test-suite
  "Tock"
 (test-suite
  "Tock"
  (test-suite
   "Falls"
   
(test-equal? "a"  (make-tet (make-posn 1 1) (list)) (make-tet (make-posn 1 1) (list)))
   (test-equal? "5 21; empty -> 5 20; empty"
              (tock TOCK-F-EX1)
              (make-tet (make-posn 5 20) (list)))
   (test-equal? "5 3; 5 1 -> 5 2; 5 1"
              (tock TOCK-F-EX2)
              (make-tet (make-posn 5 2) (list (make-posn 5 1))))
   (test-equal? "1 10; 1 8 -> 1 9; 1 8"
              (tock TOCK-F-EX3)
              (make-tet (make-posn 1 9) (list (make-posn 1 8))))
   (test-equal? "5 3; blocked from sides -> 5 2; blocked from sides"
              (tock TOCK-F-EX4)
              (make-tet (make-posn 5 2) (list (make-posn 4 3) (make-posn 4 2) (make-posn 4 1) (make-posn 6 3)  (make-posn 6 2) (make-posn 6 1) (make-posn 5 1)))))
  (test-suite
   "Blocked"
   (test-equal? "1 2; 1 1 -> 5 22; 1 1, 1 2"
              (tock TOCK-B-EX1)
              (make-tet (make-posn 5 22) (list (make-posn 1 1) (make-posn 1 2))))
   (test-equal? "5 1; empty -> 5 22; 5 1"
              (tock TOCK-B-EX2)
              (make-tet (make-posn 5 22) (list (make-posn 5 1))))
   (test-equal? "10 2; 10 1 -> 5 22; 10 2, 10 1"
              (tock TOCK-B-EX5)
              (make-tet (make-posn 5 22) (list (make-posn 10 1) (make-posn 10 2)))))
  (test-suite
   "clears row!"
   (test-equal? "10 1; ONE-ROW excet 10 1 -> 5 22; empty"
                (tock (make-tet (make-posn 10 1) (rest ONE-ROW)))
                (make-tet (make-posn 5 22) (list)))
   (test-equal? "10 2; TWO ROW except 10 2 -> 5 22; empty"
                (tock (make-tet (make-posn 10 2) (rest TWO-ROW)))
                (make-tet (make-posn 5 22) (list)))
   (test-equal? "10 1; TWO ROW except 10 1, 10 2 -> 5 22; ONE-ROW except 10 1"
                (tock (make-tet (make-posn 10 1) (append (rest ONE-ROW) (rest ONLY-SECOND-ROW))))
                (make-tet (make-posn 5 22) (rest ONE-ROW)))))
  (test-suite
   "Is-blocked?"
   (test-suite
    "Falls"
    (test-equal? "5 21; empty -> #f"
               (is-blocked? (tet-hand TOCK-F-EX1) (tet-blocks TOCK-F-EX1))
               #f)
    (test-equal? "5 3; 5 1 -> #f"
               (is-blocked? (tet-hand TOCK-F-EX2) (tet-blocks TOCK-F-EX2))
               #f)
    (test-equal? "1 10; 1 8 -> #f"
               (is-blocked? (tet-hand TOCK-F-EX3) (tet-blocks TOCK-F-EX3))
               #f)
    (test-equal? "5 3; blocked from sides -> #f"
               (is-blocked? (tet-hand TOCK-F-EX4) (tet-blocks TOCK-F-EX4))
               #f))
   (test-suite
    "Blocked"
    (test-equal? "1 2; 1 1 -> #t"
               (is-blocked? (tet-hand TOCK-B-EX1) (tet-blocks TOCK-B-EX1))
               #t)
    (test-equal? "5 1; empty -> #t"
               (is-blocked? (tet-hand TOCK-B-EX2) (tet-blocks TOCK-B-EX2))
               #t)
    (test-equal? "1 1; empty -> #t"
               (is-blocked? (tet-hand TOCK-B-EX3) (tet-blocks TOCK-B-EX3))
               #t)
    (test-equal? "5 15; 5 14 -> #t"
               (is-blocked? (tet-hand TOCK-B-EX4) (tet-blocks TOCK-B-EX4))
               #t)
    (test-equal? "10 2; 10 1 -> #t"
               (is-blocked? (tet-hand TOCK-B-EX5) (tet-blocks TOCK-B-EX5))
               #t)))
  (test-suite
   "Aux-blocked?"
   (test-equal? "0 0; empty -> #f"
              (aux-blocked? (make-posn 0 0) (list))
              #f)
   (test-equal? "0 0; 0 0 -> #t"
              (aux-blocked? (make-posn 0 0) (list (make-posn 0 0)))
              #t)
   (test-equal? "0 0; 1 1, 1 2 0 0 -> #t"
              (aux-blocked? (make-posn 0 0) (list (make-posn 1 1) (make-posn 1 2) (make-posn 0 0)))
              #t))
  (test-suite
   "Posn-equal?"
   (test-equal? "1 1; 1 1 -> #t"
              (posn-equal? (make-posn 1 1) (make-posn 1 1))
              #t)
   (test-equal? "0 1; 1 1 -> #f"
              (posn-equal? (make-posn 0 1) (make-posn 1 1))
              #f)
   (test-equal? "-23 10; -23 10 -> #t"
              (posn-equal? (make-posn -23 10) (make-posn -23 10))
              #t))
  (test-suite
   "Fall?"
   (test-equal? "5 21 -> 5 20"
              (fall (tet-hand TOCK-F-EX1))
              (make-posn 5 20))
   (test-equal? "5 3 -> 5 2"
              (fall (tet-hand TOCK-F-EX2))
              (make-posn 5 2))
   (test-equal? "5 3 -> 5 2"
              (fall (tet-hand TOCK-F-EX4))
              (make-posn 5 2)))
  (test-suite
   "Block"
   (test-equal? "1 2; 1 1 -> 1 1, 1 2"
              (block (tet-hand TOCK-B-EX1) (tet-blocks TOCK-B-EX1))
              (list (make-posn 1 1) (make-posn 1 2)))
   (test-equal? "1 1; empty -> 1 1"
              (block (tet-hand TOCK-B-EX3) (tet-blocks TOCK-B-EX3))
              (list (make-posn 1 1)))
   (test-equal? "10 2; 10 1 -> 10 1, 10 2"
              (block (tet-hand TOCK-B-EX5) (tet-blocks TOCK-B-EX5))
              (list (make-posn 10 1) (make-posn 10 2))))
  (test-suite
   "block-row"
   (test-equal? "1 1; empty -> 5 22; 1 1"
                (block-row (make-posn 1 1) (list))
                (make-tet (make-posn 5 22) (list (make-posn 1 1))))
   (test-equal? "10 1; ONE-ROW except 10 1 -> 5 22; empty"
                (block-row (make-posn 10 1) (rest ONE-ROW))
                (make-tet (make-posn 5 22) (list))))
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
                (clear-row! (append (list (make-posn 10 1)) (rest ONE-ROW)) 1)
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
                 (easy-sort TWO-ROW))))))



