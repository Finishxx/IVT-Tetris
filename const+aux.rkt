#lang racket

(require lang/posn 2htdp/image)
(provide (all-defined-out))



;; ================ DATA DEFINITIONS: ================

(define-struct tet [hand blocks])
;; Tetr is a Structure
;; (make-tetr (Posn, ListOf(Posn))
;; Implementation:
;; tetr-hand describes where the current falling controllable block is located
;; tetr-blocks describes where the other already fallen blocks are

;;================ Quality of life aux. functions: ================
;; debatable, if tests are valid here, still should fit in somehow ̄\_(ツ)_/̄

;; Num -> Num
;; halves the given number
(define (half num)
  (/ num 2))

(define PEN
  (make-pen "black" 1 "solid" "round" "round"))

;; Num Image -> Image
;; An auxiliary function for drawing x portion of the grid
(define (aux-grid-x num img)
  (cond
    [(= num X-LINES) img]
    [else (aux-grid-x
           (+ num 1)
           (add-line
            img
            0
            (aux-x-position num)
            (image-width BORDER)
            (aux-x-position num)
            PEN
             ))]))

;; Num Image -> Image
;; An auxiliary function for drawing y portion of the grid
(define (aux-grid-y num img)
  (cond
    [(= num Y-LINES) img]
    [else (aux-grid-y
           (+ num 1)
           (add-line
            img
            (aux-y-position num)
            0
            (aux-y-position num)
            (image-height BORDER)
            PEN
             ))]))

;; Num -> Num
;; Auxiliary for aux-grid-x for calculating the actual start and end point of the line
(define (aux-x-position num)
  (* (+ SHIVER num) CUBE-LENGTH))

;; Num -> Num
;; Auxiliary for aux-grid-y for calculating the actual start and end point of the line
(define (aux-y-position num)
  (* num CUBE-LENGTH))


;; ================ Mathematical constants (mostly): ================
(define CUBE-LENGTH 10)
(define SCENE-WIDTH-INDEX 20)
(define SCENE-HEIGHT-INDEX 30)
(define SCENE-WIDTH (* SCENE-WIDTH-INDEX CUBE-LENGTH))
(define SCENE-HEIGHT (* SCENE-HEIGHT-INDEX CUBE-LENGTH))
(define HALF-SCENE-WIDTH (half SCENE-WIDTH))
(define HALF-SCENE-HEIGHT (half SCENE-HEIGHT)) 
(define MTSC (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define SHIVER 0.4)

(define X-OFFSET (* 4.5 CUBE-LENGTH)) ;; breaks when SCENE-WIDTH//HEIGHT-INDEX is changed, careful!!! 
(define Y-OFFSET (* 25.7 CUBE-LENGTH)) ;; breaks when SCENE-WIDTH//HEIGHT-INDEX is changed, careful!!! 
  
;; these are just for drawing purposes and they mean how many || are there to each axis in a grid
(define X-LINES 20)
(define Y-LINES 10)

(define CLOCK-SPEED 0.25)

(define WHITE-SPACE-Y-POS (half (* 5.5 CUBE-LENGTH)))

;; ================ Graphical constants (mostly): ================

(define BLOCK (square CUBE-LENGTH "solid" "red"))
(define BORDER (rectangle (* Y-LINES CUBE-LENGTH) (* (+ X-LINES SHIVER) CUBE-LENGTH) "outline" "black"))
(define GRID-X
  (aux-grid-x 0 BORDER))
(define GRID-Y
  (aux-grid-y 0 BORDER))
(define GRID (aux-grid-x 0 (aux-grid-y 1 BORDER)))
(define WHITE-SPACE (rectangle (half SCENE-WIDTH) (- SCENE-HEIGHT Y-OFFSET) "solid" "white"))

;;================ Pictures: ================
;; Basic layout:
(define LAYOUT (place-image GRID HALF-SCENE-WIDTH HALF-SCENE-HEIGHT MTSC))

;; ================ Testing sets: ================




                   

