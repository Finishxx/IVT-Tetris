#lang racket
(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         test-engine/racket-gui)
(provide (all-defined-out))

;; draw:
;; 1. draw-blocks ✓
;; 2. schematic->actual ✓
;; 3. block-list ✓
;; 4. actual->schematic

(define (draw tet)
  (place-images
   (list GRID
         WHITE-SPACE)
   (list
    (make-posn HALF-SCENE-WIDTH
               HALF-SCENE-HEIGHT)
    (make-posn (half SCENE-WIDTH)
               WHITE-SPACE-Y-POS))
    (draw-blocks tet MTSC)))

;; Tet bckg -> Image
;; places blocks in accordance with schematic pos given onto bckg
(define (draw-blocks tet bckg)
  (place-images
   (block-list (+ (length (tet-blocks tet)) 1))
   (schematic->actual (append (list (tet-hand tet)) (tet-blocks tet)))
   bckg))

;; ListOf(Posn) -> ListOf(Posn)
;; converts the schematic description of posn on the board to actual applicable values
(define (schematic->actual pos)
  (map (lambda (posit)
         (make-posn (inexact->exact (+ X-OFFSET (* (posn-x posit) CUBE-LENGTH)))
                    (inexact->exact (- Y-OFFSET (* (posn-y posit) CUBE-LENGTH))))) pos))

;; Num -> ListOf(Blocks = Images)
;; Creates a list containing as many picts of BLOCKS as high is the number n
(define (block-list n)
  (for/list([x (in-range n)])
    BLOCK))

