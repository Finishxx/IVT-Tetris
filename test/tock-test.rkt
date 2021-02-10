#lang racket

(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "../tock.rkt"
         "../const+aux.rkt")


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

(test/gui
 (test-suite
  "Tock"
  (test-suite
   "Falls"

   (test-case "5 21; empty -> 5 20; empty"
              (tock TOCK-F-EX1)
              (make-tet (make-posn 5 20) (list)))
   (test-case "5 3; 5 1 -> 5 2; 5 1"
              (tock TOCK-F-EX2)
              (make-tet (make-posn 5 2) (list (make-posn 5 1))))
   (test-case "1 10; 1 8 -> 1 9; 1 8"
              (tock TOCK-F-EX3)
              (make-tet (make-posn 1 9) (list (make-posn 1 8))))
   (test-case "5 3; blocked from sides -> 5 2; blocked from sides"
              (tock TOCK-F-EX4)
              (make-tet (make-posn 5 2) (list (make-posn 4 3) (make-posn 4 2) (make-posn 4 1) (make-posn 6 3)  (make-posn 6 2) (make-posn 6 1) (make-posn 5 1)))))
  (test-suite
   "Blocked"
   (test-case "1 2; 1 1 -> 5 22; 1 1, 1 2"
              (tock TOCK-B-EX1)
              (make-tet (make-posn 5 22) (list (make-posn 1 1) (make-posn 1 2))))
   (test-case "5 1; empty -> 5 22; 5 1"
              (tock TOCK-B-EX2)
              (make-tet (make-posn 5 22) (list (make-posn 5 1))))
   (test-case "1 1; empty -> 5 22; 1 1"
             
              (tock TOCK-B-EX3)
              (make-tet (make-posn 5 22) (list (make-posn 1 1))))
   (test-case "5 15; 5 14 -> 5 22; 5 15, 5 14"
              (tock TOCK-B-EX4)
              (make-tet (make-posn 5 22) (list (make-posn 5 14) (make-posn 5 15))))
   (test-case "10 2; 10 1 -> 5 22; 10 2, 10 1"
              (tock TOCK-B-EX5)
              (make-tet (make-posn 5 22) (list (make-posn 10 1) (make-posn 10 2)))))
  (test-suite
   "Is-blocked?"
   (test-suite
    "Falls"
    (test-case "5 21; empty -> #f"
               (is-blocked? (tet-hand TOCK-F-EX1) (tet-blocks TOCK-F-EX1))
               #f)
    (test-case "5 3; 5 1 -> #f"
               (is-blocked? (tet-hand TOCK-F-EX2) (tet-blocks TOCK-F-EX2))
               #f)
    (test-case "1 10; 1 8 -> #f"
               (is-blocked? (tet-hand TOCK-F-EX3) (tet-blocks TOCK-F-EX3))
               #f)
    (test-case "5 3; blocked from sides -> #f"
               (is-blocked? (tet-hand TOCK-F-EX4) (tet-blocks TOCK-F-EX4))
               #f))
   (test-suite
    "Blocked"
    (test-case "1 2; 1 1 -> #t"
               (is-blocked? (tet-hand TOCK-B-EX1) (tet-blocks TOCK-B-EX1))
               #t)
    (test-case "5 1; empty -> #t"
               (is-blocked? (tet-hand TOCK-B-EX2) (tet-blocks TOCK-B-EX2))
               #t)
    (test-case "1 1; empty -> #t"
               (is-blocked? (tet-hand TOCK-B-EX3) (tet-blocks TOCK-B-EX3))
               #t)
    (test-case "5 15; 5 14 -> #t"
               (is-blocked? (tet-hand TOCK-B-EX4) (tet-blocks TOCK-B-EX4))
               #t)
    (test-case "10 2; 10 1 -> #t"
               (is-blocked? (tet-hand TOCK-B-EX5) (tet-blocks TOCK-B-EX5))
               #t)))
  (test-suite
   "Aux-blocked?"
   (test-case "0 0; empty -> #f"
              (aux-blocked? (make-posn 0 0) (list))
              #f)
   (test-case "0 0; 0 0 -> #t"
              (aux-blocked? (make-posn 0 0) (list (make-posn 0 0)))
              #t)
   (test-case "0 0; 1 1, 1 2 0 0 -> #t"
              (aux-blocked? (make-posn 0 0) (list (make-posn 1 1) (make-posn 1 2) (make-posn 0 0)))
              #t))
  (test-suite
   "Posn-equal?"
   (test-case "1 1; 1 1 -> #t"
              (posn-equal? (make-posn 1 1) (make-posn 1 1))
              #t)
   (test-case "0 1; 1 1 -> #f"
              (posn-equal? (make-posn 0 1) (make-posn 1 1))
              #f)
   (test-case "-23 10; -23 10 -> #t"
              (posn-equal? (make-posn -23 10) (make-posn -23 10))
              #t))
  (test-suite
   "Fall?"
   (test-case "5 21 -> 5 20"
              (fall (tet-hand TOCK-F-EX1))
              (make-posn 5 20))
   (test-case "5 3 -> 5 2"
              (fall (tet-hand TOCK-F-EX2))
              (make-posn 5 2))
   (test-case "1 10 -> 1 9"
              (fall (tet-hand TOCK-F-EX3))
              (make-posn 1 9))
   (test-case "5 3 -> 5 2"
              (fall (tet-hand TOCK-F-EX4))
              (make-posn 5 2)))
  (test-suite
   "Block"
   (test-case "1 2; 1 1 -> 1 1, 1 2"
              (block (tet-hand TOCK-B-EX1) (tet-blocks TOCK-B-EX1))
              (list (make-posn 1 1) (make-posn 1 2)))
   (test-case "5 1; empty -> 5 1"
              (block (tet-hand TOCK-B-EX2) (tet-blocks TOCK-B-EX2))
              (list (make-posn 5 1)))
   (test-case "1 1; empty -> 1 1"
              (block (tet-hand TOCK-B-EX3) (tet-blocks TOCK-B-EX3))
              (list (make-posn 1 1)))
   (test-case "5 15; 4 14 -> 5 14, 5 15"
              (block (tet-hand TOCK-B-EX4) (tet-blocks TOCK-B-EX4))
              (list (make-posn 5 14) (make-posn 5 15)))
   (test-case "10 2; 10 1 -> 10 1, 10 2"
              (block (tet-hand TOCK-B-EX5) (tet-blocks TOCK-B-EX5))
              (list (make-posn 10 1) (make-posn 10 2))))
  (test-suite
   "Clear-row?"
   #t #t)))
