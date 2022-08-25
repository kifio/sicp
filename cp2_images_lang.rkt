#lang racket

(require "cp2.rkt")

(require racket/draw)
(require racket/gui)

(define (split large-image-layout small-image-layout)
  (define (generic-split painter n) 
    (if (= n 0)
      painter
      (let ((smaller (generic-split painter (- n 1)))) (large-image-layout painter (small-image-layout smaller smaller)))))
  generic-split )

;(define right-split (split beside below))
;(define up-split (split below beside))

; list, чтобы можно было использовать функции типа accumulate-n 
(define (make-vect x y)
  (list x y))

(define (xcor-vect v) (car v))
(define (ycor-vect v) (car (cdr v)))

(define (add-vect v1 v2) (accumulate-n + 0 (list v1 v2)))
(define (sub-vect v1 v2) (accumulate-n - 0 (list v1 v2)))
(define (scale-vect scale v) (map (lambda (x) (* scale x)) v))

#|(add-vect (make-vect 0 0) (make-vect 5 5))
(sub-vect (make-vect 0 0) (make-vect 5 5))
(scale-vect 5 (make-vect 5 5))|#

(define (make-frame origin  edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (car (cdr (cdr frame))))

; еее маппинг координат в единичный квадрат
(define (frame-coord-map frame)
  (lambda (v) (add-vect (origin-frame frame) (add-vect (scale-vect (xcor-vect v) (edge1-frame frame)) (scale-vect (ycor-vect v) (edge2-frame frame))))))

#|(define (screen)
  (let ((origin (make-vect 0 0))
       (edge1 (make-vect 2560 0))
       (edge2 (make-vect 0 1920)))
  (make-frame origin edge1 edge2)))

(origin-frame (screen))
(edge1-frame (screen))
(edge2-frame (screen))

((frame-coord-map (screen)) (make-vect 0.5 0.5))|#

#|(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))|#