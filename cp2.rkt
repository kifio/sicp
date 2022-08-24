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
;(print-rat one-half)

(define one-third (make-rat 1 3))
;(print-rat one-third)

;(print-rat (add-rat one-half one-third))
;(print-rat (mul-rat one-half one-third))
;(print-rat (add-rat one-third one-third))

; Числитель - целое число, знаменатель - натуральное число.
; При создании рационального числа это надо учитывать и знаменатель всегда должен быть натуральным.

;(print-rat (make-rat -1 3)) ; -1/3
;(print-rat (make-rat 1 -3)) ; -1/3
;(print-rat (make-rat -1 -3)) ; 1/3
;(print-rat (make-rat 1 3)) ; 1/3

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

;(print-point (midpoint-segment (make-segment -7 3 2 4)))

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

;(newline)
;(perimeter (make-rect 0 0 10 10))
;(area (make-rect 0 0 10 10))

; Интервальная арифметика
(define (make-interval lower upper) (cons lower upper))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let (
         (p1 (* (lower-bound x) (lower-bound y)))
         (p2 (* (lower-bound x) (upper-bound y)))
         (p3 (* (upper-bound x) (lower-bound y)))
         (p4 (* (upper-bound x) (upper-bound y)))
         )
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
    (- (upper-bound x) (upper-bound y))))

;(add-interval (make-interval 3.0 5.0) (make-interval 4.0 5.0))
;(mul-interval (make-interval 3.0 5.0) (make-interval 4.0 5.0))
;(div-interval (make-interval 3.0 5.0) (make-interval -4.0 5.0))
;(sub-interval (make-interval 4.0 5.0) (make-interval 3.0 5.0))

; Списки
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
;(list-ref squares 3)

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
  (if (null? (cdr items))
    (car items)
    (last-pair (cdr items))))

(define (reverse items)
  (define (reverse-iter items out)
    (if (null? (cdr items))
      (cons (car items) out)
      (reverse-iter (cdr items) (cons (car items) out))))
  (reverse-iter items (list)))

(define (same-parity x . items)
  (define (is-same-parity x y)
    (and (= (remainder y 2) (remainder x 2))))

  (define (same-parity-iter x items out)
    (if (null? (cdr items))
      (if (is-same-parity x (car items))
	(cons (car items) out)
	out)
      (same-parity-iter x (cdr items) (if (is-same-parity x (car items))
					(cons (car items) out)
					out))))
  (same-parity-iter x items (list)))



;(last-pair (list 23 72 149 34))
;(reverse (list 23 72 149 34))
;(same-parity 2 3 4 5 6 7 8 9 10 11)

; map()

(define (map proc items)
  (if (null? items)
    null
    (cons (proc (car items)) (map proc (cdr items)))))

;(map abs (list -10 2.5 -11.6 17))

;(define (square-list items)
;  (if (null? items)
;    null
;    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x))  items))

;(square-list (list 2 3 4 5 6 7 8 9 10 11))

(define (for-each proc items)
  (proc (car items))
  (cond ((not (null? (cdr items)))
    (for-each proc (cdr items)))))

;(for-each (lambda (x) (newline) (display x)) (list 2 3 4 5 6 7 8 9 10 11))

;(define (extract-seven) (list 1 3 (list 5 7) 9))
;(car (cdr (car (cdr (cdr (extract-seven))))))

;(define (extract-seven) (list (list 7)))
;(car (car (extract-seven)))

;(define (extract-seven) (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (extract-seven)))))))))))))

;(define x (list 1 2 3))
;(define y (list 4 5 6))

;(append x y)
;(cons x y)
;(list x y)

; Деревья
(define tree (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))


(define (fringe l)
  (define (fringe-iter l out)
    ;(display l)
    ;(newline)
    (cond ((null? l) out)
	((not (pair? l))
         ;(display l)
         ;(display " не является списком")
         ;(newline)
         (append (list l) out))
	(else
         ;(display l)
         ;(display " является списком")
         ;(newline)
         (append (fringe-iter (car l) out)
		 (fringe-iter (cdr l) out)))))
  
  (fringe-iter l (list)))

(define x (list 1 (list 2 (list 3 4)) 5))

;(fringe (list x x))
;(fringe 101)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 2)

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (map-square-tree tree) (tree-map (lambda (x) (* x x)) tree))
;(map-square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; Операции над последовательностями
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;(filter odd? (list 1 2 3 4 5))
;(accumulate + 0 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree) (fringe tree))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))
  ))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons null (filter even? (map fib (enumerate-interval 0 n)))))

;(sum-odd-squares (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;(even-fibs 10)

(define (accumulate-map p sequence)
  (accumulate (lambda (x y) (cons (p (car sequence)) (accumulate-map p (cdr sequence)))) null sequence))

(define (accumulate-append seq1 seq2)
  (accumulate cons seq1 seq2)
  ;(cond
  ;  ((pair? seq1) (cons (car seq1) (accumulate-append (cdr seq1) seq2)))
  ;  ((pair? seq2) (cons (car seq2) (accumulate-append seq1 (cdr seq2))))
  ;  (else null))
  )

(define (accumulate-length sequence)
  (accumulate + 0 sequence))

;(accumulate-map (lambda (x) (* x x)) (list 2 3 4 5 6 7 8 9 10 11))
;(accumulate-append (list 2 3 4 5 6) (list 7 8 9 10 11))
;(accumulate-length (list 2 3 4 5 6 7 8 9 10 11))

(define (get-heads sequences)
  (if (null? sequences) null
  (cons (car (car sequences)) (get-heads (cdr sequences))))
)

(define (get-tails sequences)
  (if (null? sequences) null
  (cons (cdr (car sequences)) (get-tails (cdr sequences))))
)

;(get-heads (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;(get-tails (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; Аккумулирует соответствующие элементы из последовательностей входящей последовательности (типа операторов compact/flat)
; сижу с лицом лица осознавая, что get-heads - это (map car seq), а get-tails - это (map cdr seq). 
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (get-heads seqs))
            (accumulate-n op init (get-tails seqs)))))

;(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12) (list 13 14 15)))


; ALARMA. В книге в dot-product используется преобразование (map * v w), но map принимает только 2 аргумента, а не 3.
; Чтобы это порешить
(define (acc-with-transform proc seq1 seq2)
  (if (null? seq1)
      null
      (cons (proc (car seq1) (car seq2)) (acc-with-transform proc (cdr seq1) (cdr seq2)))
  ))

; Скалярное произведение двух векторов.
(define (dot-product v w)
  (accumulate + 0 (acc-with-transform * v w)))

; Умножение матрицы на вектор
(define(matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
;(matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 10 11 12))

; Транспонирование
(define (transpose mat)
  (accumulate-n cons null mat))
;(transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; Умножение матрицы на матрицу
(define(matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(matrix-*-matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))