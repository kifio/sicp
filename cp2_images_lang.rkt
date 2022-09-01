#lang racket

(require "cp2.rkt")

(require racket/gui/base)

(define picture-size 500)

(define bitmap
  (make-object bitmap% (+ picture-size 1) (+ picture-size 1)))

(define bitmap-dc
  (new bitmap-dc% [bitmap bitmap]))

(define frame
  (new frame% [label "SICP Picture Language"]))

(define canvas
  (new canvas%
     [parent frame]
     [min-width (+ picture-size 1)]
     [min-height (+ picture-size 1)]
     [paint-callback (lambda (canvas dc)
                       (send dc draw-bitmap bitmap 0 0))]))

(define (draw-line start end)
  (send bitmap-dc 
        draw-line
        (xcor-vect start)
        (ycor-vect start)
        (xcor-vect end)
        (ycor-vect end)))


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

(define window (make-frame (make-vect 0 picture-size)
                           (make-vect picture-size 0)
                           (make-vect 0 (- 0 picture-size))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

#|(define (screen)
  (let ((origin (make-vect 0 0))
       (edge1 (make-vect 2560 0))
       (edge2 (make-vect 0 1920)))
  (make-frame origin edge1 edge2)))

(origin-frame (screen))
(edge1-frame (screen))
(edge2-frame (screen))

((frame-coord-map (screen)) (make-vect 0.5 0.5))|#

(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (display segment)
  (newline)
  (car segment))

(define (end-segment segment)
  (car (cdr segment)))

(define rect
  (segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 1 0))
                      (make-segment (make-vect 1 0) (make-vect 1 1))
                      (make-segment (make-vect 1 1) (make-vect 0 1))
                      (make-segment (make-vect 0 1) (make-vect 0 0)))))

(define cross
  (segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 1 1))
                      (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond
  (segments->painter (list
                      (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                      (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                      (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                      (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))


#|(send frame show #t)
(rect window)
(cross window)
(diamond window)|#
;wave рисовать не буду, мне хватило одного перепечатывания координат картины "Кот в сапогах" с миллиметровки в C++ Builder 6. 