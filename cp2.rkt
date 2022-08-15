#lang racket

(require "cp1.rkt")

; Рациональные числа
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sign n)
  (if (> n 0)
    1
    -1))

(define (make-rat n d)
  (let (
	(g (gcd (abs n) (abs d)))
	(signed_n (/ n (sign d)))
	(signed_d (/ d (sign d)))
	)
    (cons (/ signed_n g) (/ signed_d g))))

(define (numer x) 
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom y) (denom x))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

; Числитель - целое число, знаменатель - натуральное число.
; При создании рационального числа это надо учитывать и знаменатель всегда должен быть натуральным.

(print-rat (make-rat -1 3)) ; -1/3
(print-rat (make-rat 1 -3)) ; -1/3
(print-rat (make-rat -1 -3)) ; 1/3
(print-rat (make-rat 1 3)) ; 1/3

; Представление отрезков прямой на плоскости
(define (make-point x y)
  (cons x y))

(define (make-segment x-start y-start x-end y-end)
  (cons (make-point x-start y-start) (make-point x-end y-end)))

(define (x-point x)
  (car x))

(define (y-point x)
  (cdr x))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Нахождение середины отрезка
(define (midpoint-segment segment)
  (let (
	(start (start-segment segment))
	(end (end-segment segment))
	)
  (make-point (/ (+ (x-point start) (x-point end)) 2) (/ (+ (y-point start) (y-point end)) 2))))

(print-point (midpoint-segment (make-segment -7 3 2 4)))

; Пример двух вариантов объявления прямоугольника (конструктор + селекторы)
; при которых неизменными остаются высокоуровневые функции периметра и площади

; Конструктор прямоугольника через две точки
(define (make-rect left bottom right top)
  (cons (make-point left bottom) (make-point right top)))

; Конструктор прямоугольника через точку, ширину и высоту
(define (make-rect-alt x y w h)
  (cons (cons (x y)) (cons (w h))))

(define (lb rect)
  (start-segment rect))

(define (rt rect)
  (end-segment rect))

; Селектор ширины прямоугольника обявленного через 2 точки
;(define (rect-width rect)
;  (- (x-point (rt rect)) (x-point (lb rect))))

; Селектор высоты прямоугольника обявленного через 2 точки
;(define (rect-height rect)
;  (- (y-point (rt rect)) (y-point (lb rect))))

; Селектор ширины прямоугольника обявленного через 1 точку, ширину и высоту
(define (rect-width rect)
  (car (cdr rect)))

; Селектор высоты прямоугольника обявленного через 1 точку, ширину и высоту
(define (rect-height rect)
  (cdr (cdr rect)))

(define (perimeter rect)
  (* (+ (rect-width rect) (rect-height rect)) 2))

(define (area rect)
  (* (rect-width rect) (rect-height rect)))

(newline)
(perimeter (make-rect 0 0 10 10))
(area (make-rect 0 0 10 10))
ы
