#lang racket
(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         "tetriminos.rkt")
(provide (all-defined-out))



;; draw:
;; 1. draw-blocks ✓
;; 2. schematic->actual ✓
;; 3. block-list ✓
;; 4. actual->schematic

(define (draw tet)
  (draw-score (tet-score tet) (place-images
                               (list GRID
                                     TETRIS-SPACE)
                               (list (make-posn HALF-SCENE-WIDTH
                                                HALF-SCENE-HEIGHT)
                                     (make-posn HALF-SCENE-WIDTH
                                                (* CUBE-LENGTH 2.5)))
                                     (draw-blocks (append (tet-hand tet)
                                                          (tet-blocks tet))
                                                  (block-preview (tet-bag tet) PLACED-MTSC-PREVIEW)))))

;; blocks bckg -> Image
;; places blocks in accordance with schematic pos given onto bckg
(define (draw-blocks blocks bckg)
  (place-images
   (block-list (list-col blocks))
   (schematic->actual (blocks->posn blocks))
   bckg))

;; ListOfBlocks -> ListOfPosn
;; provided a list of blocks returns a list of posns
(define (blocks->posn blocks)
  (map (lambda (block)
         (block-posn block))
         blocks))

;; ListOf(Posn) -> ListOf(Posn)
;; converts the schematic description of posn on the board to actual applicable values
(define (schematic->actual pos)
  (map (lambda (posit)
         (make-posn (inexact->exact (+ X-OFFSET (* (posn-x posit) CUBE-LENGTH)))
                    (inexact->exact (- Y-OFFSET (* (posn-y posit) CUBE-LENGTH))))) pos))

;; ListOfBlocks -> ListOfColor
;; given a list of blocks returns a list of colors in order
(define (list-col blocks)
  (map (lambda (block)
         (block-col block))
       blocks))

;; ListOfCol -> ListOf(Blocks = Images)
;; Creates a list of blocks from given list of colors in order
(define (block-list col)
  (map (lambda (col)
         (square CUBE-LENGTH "solid" col))
       col))

;; Bag -> Img
;; renders block preview
(define (block-preview bag img)
  (draw-blocks (preview-pos (take bag 3)) img))



;; ListOfBlocks -> ListOfBlocks
;; moves the tetriminos to preview positions
(define (preview-pos blocks)
  (append (preview-y-pos (preview-x-pos (first blocks) -5))
          (preview-y-pos (second blocks))
          (preview-y-pos (preview-x-pos (third blocks) 5))))

;; ListOfBlocks -> ListOfBlocks
;; sets the y pos of a set of blocks to preview compatible level
(define (preview-y-pos blocks)
  (map (lambda (block)
         (make-block (make-posn (posn-x (block-posn block))
                                (- (posn-y (block-posn block)) 23.25))
                     (block-col block)))                               
       blocks))

;; ListOfBlocks Num -> ListOfBlocks
;; changes the x pos of a list of blocks by Num
(define (preview-x-pos blocks i)
  (map (lambda (block)
         (make-block (make-posn (+ (posn-x (block-posn block)) i)
                                (posn-y (block-posn block)))
                     (block-col block)))
         blocks))

;; Num -> Img
;; draws score
(define (draw-score score img)
  (place-image (text (number->string score) CUBE-LENGTH "black")
               (* 17.5 CUBE-LENGTH)
               CUBE-LENGTH
               img))


(define J-yes (list (make-block (make-posn -1 -1.25) "dark blue")
                    (make-block (make-posn -1 -2.25) "dark blue")
                    (make-block (make-posn 0 -2.25) "dark blue")
                    (make-block (make-posn 1 -2.25) "dark blue")))
(define I-yes (list (make-block (make-posn -1 -2.25) "light blue")
                    (make-block (make-posn 0 -2.25) "light blue")
                    (make-block (make-posn 1 -2.25) "light blue")
                    (make-block (make-posn 2 -2.25) "light blue")))
(define I-second (list (make-block (make-posn 4 -2.25) "light blue")
                       (make-block (make-posn 5 -2.25) "light blue")
                       (make-block (make-posn 6 -2.25) "light blue")
                       (make-block (make-posn 7 -2.25) "light blue")))
(define I-third (list (make-block (make-posn 9 -2.25) "light blue")
                      (make-block (make-posn 10 -2.25) "light blue")
                      (make-block (make-posn 11 -2.25) "light blue")
                      (make-block (make-posn 12 -2.25) "light blue")))

(draw-blocks J-yes PLACED-MTSC-PREVIEW)
(draw-blocks (append J-yes I-second I-third) PLACED-MTSC-PREVIEW)

(place-image (text "120080" 10 "black")
             (* 17.5 CUBE-LENGTH)
             CUBE-LENGTH
             PLACED-MTSC-PREVIEW)

