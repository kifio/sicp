#lang racket

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square x)))

;(define (f a)
;  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (positive-odd x)
  (and (> x 0) (= 0 (remainder x 2))))

; if процедурой
(define (new-if predicate then-cl else-cl)
  (cond (predicate then-cl)
        (else else-cl)))

; квадратный корень
(define (sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess prev-guess)
    (< (abs (- guess prev-guess)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve  guess) guess)))
  (sqrt-iter 1.0 x))

;(sqrt 2048)

; кубический корень
(define (cqrt x)
  (define (cqrt guess x)
    (/ (+ (* 2 guess) (/ x (* guess guess))) 3))
  (define (good-enough? guess prev-guess)
    (< (abs (- guess prev-guess)) 0.0001))
  (define (cqrt-iter guess prev-guess x)
    (if (good-enough? guess prev-guess)
        guess
        (cqrt-iter (cqrt guess x) guess x))
    )
  (cqrt-iter 1.0 0.0 x))

;(cqrt 1331)

; f(n - 1) + f(n - 2) + f(n - 3)
(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))
))

; элемент треугольника Паскаля (рекурсивно)
(define (pascal n k)
  (if (or (= n 0) (= k 0) (= n k))
      1
      (+ (pascal (- n 1) k) (pascal (- n 1) (- k 1))))
)

;(pascal 6 3)

; рекурсивное возведение в степень
(define (fast-expt b n)
  (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1)))))
)

; итеративное возведение в степень
(define (pow base power)
  (fast-expt-iter 1 base power)
)

(define (fast-expt-iter a base n)
  (cond ((= n 0) a)
        ((= n 1) (* a base))
        (else (fast-expt-iter (* a base base) base (- n 2))))
)

;(pow 7 9)

(define (double n) (* n 2))

(define (halve n) (/ n 2))

(define (even? n) (= (remainder n 2) 0))

; рекурсивное умножение с помощью сложения (m * n)
(define (fast-mult-rec m n)
  (cond ((= n 0) 0) ;частный случай
        ((= n 1) m)
        ((even? n) (double (fast-mult-rec m (halve n))))
        (else (+ m (fast-mult-rec m (- n 1))))
   )
)

;(fast-mult-rec 11 11)


; итеративное умножение с помощью сложения (m * n)
(define (fast-mult-iter m n out)
  (cond ((= n 0) out) ;в случае нечетного второго множителя мы оказываемся здесь дважды - на первой и на последней итерации. в случае четного только на последней
        ((even? n) (fast-mult-iter (double m) (halve n) out))
        (else (fast-mult-iter m (- n 1) (+ m out)))
   )
)

;(fast-mult-iter 11 11 0)

; наименьший делитель
(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides n divisor)
  (= (remainder n divisor) 0))

(define (next test-divisor)
  (if (= test-divisor 2)
    3
    (+ test-divisor 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides n test-divisor) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

; поиск простых чисел в лоб
(define (prime? n)
  (= n (smallest-divisor n)))

; тест Ферма
(define (fermat-test n)
  (define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

;(fast-prime? 29 5)
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (when (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1999)
(timed-prime-test 10000000000037)
(timed-prime-test 10000000000051)
(timed-prime-test 10000000000099)
(timed-prime-test 100000000000031)
(timed-prime-test 100000000000067)
(timed-prime-test 100000000000097)

(define (search-for-primes n start-time founded-numbers-counter)
    (cond ((= founded-numbers-counter 0) 
	    (display "search finished\n"))  
	  ((prime? n) 
	    (display n)
	    (display " founded in: ")
	    (display (- (runtime) start-time))
  	    (newline)	    
 	    (search-for-primes (+ n 1) (runtime) (- founded-numbers-counter 1)))
	  (else (search-for-primes (+ n 1) start-time founded-numbers-counter))))

;(search-for-primes 1000000000000  (runtime) 3)
;(search-for-primes 10000000000000 (runtime) 3)
;(search-for-primes 100000000000000 (runtime) 3)

