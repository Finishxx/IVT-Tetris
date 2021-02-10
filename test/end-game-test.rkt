#lang racket

(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "../end-game.rkt"
         "../const+aux.rkt")

(test/gui
 (test-suite
  "End-game?"
  (test-suite
   "end-game?"
   (test-case "5 21 -> #f"
              (end-game? (make-tet (make-posn 5 21) (list)))
              #f)
   (test-case "1 10; 5 20, 4 20 -> #f"
              (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 20) (make-posn 4 20))))
              #f)
   (test-case "1 10; 5 21 -> #t"
              (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 21))))
              #t)
   (test-case "1 10; 5 23 -> #t"
              (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 23))))
              #t))
  (test-suite
   "top-off?"
   (test-case "empty -> #f"
              (top-off? (list))
              #f)
   (test-case "5 21 -> #t"
              (top-off? (list (make-posn 5 21)))
              #t)
   (test-case "5 22 -> #t"
              (top-off? (list (make-posn 5 22)))
              #t))))