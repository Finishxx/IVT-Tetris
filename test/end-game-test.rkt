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
   (test-equal? "5 21 -> #f"
              (end-game? (make-tet (make-posn 5 21) (list)))
              #f)
   (test-equal? "1 10; 5 20, 4 20 -> #f"
              (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 20) (make-posn 4 20))))
              #f)
   (test-equal? "1 10; 5 21 -> #t"
              (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 21))))
              #t)
   (test-equal? "1 10; 5 23 -> #t"
              (end-game? (make-tet (make-posn 1 10) (list (make-posn 5 23))))
              #f))
  (test-suite
   "top-off?"
   (test-equal? "empty -> #f"
              (top-off? (list))
              #f)
   (test-equal? "5 21 -> #t"
              (top-off? (list (make-posn 5 21)))
              #t)
   (test-equal? "5 22 -> #t"
              (top-off? (list (make-posn 5 22)))
              #t))))