#lang racket

(require racket/draw)
(require racket/gui)

(define (split large-image-layout small-image-layout)
  (define (generic-split painter n) 
    (if (= n 0)
      painter
      (let ((smaller (generic-split painter (- n 1)))) (large-image-layout painter (small-image-layout smaller smaller)))))
  generic-split )

(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v) (add-vect
               (origin-frame frame) (add-vect
                                              (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))
    ))

(define (beside painter1 painter2)
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
(define wave4 (below wave2 wave2))