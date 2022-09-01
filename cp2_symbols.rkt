#lang racket

(require "cp2.rkt")

(define (memq item x)
  (cond ((null? x)  false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;(memq 1 (list 1 2 3 4 5 6 7 8 9 10))
;(memq 12 (list 1 2 3 4 5 6 7 8 9 10))
;(memq '1 '(list 1 2 3 4 5 6 7 8 9 10))

; Дифферинцирование (за упражнения не брался)
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? ex num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond((number? exp) 0)
       ((variable? exp)
        (if (same-variable? exp var) 1 0))
       ((sum? exp)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))
       ((product? exp)
        (make-sum
         (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))
         (make-product (deriv (multiplier exp) var)
                       (multiplicand exp))))
        (else
         (error "Неизвестный тип выражения -- DERIV" exp))))

;(deriv '(+ x 3) 'x)

; Представление множеств списками
(define (element-of-set? x set)
    (cond ((null? set)  false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 4 2 3) (list 4 5 6 2))
(intersection-set (list 4 2 3) (list 4 2 8 9))

; Представление множеств деревьями
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right) (list entry left right))

(define (element-of-tree-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set))
	 (element-of-tree-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-tree-set? x (right-branch set)))))

(define (addjoin-tree-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (addjoin-tree-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (addjoin-tree-set x (right-branch set))))))

(define tree1 (make-tree 2
			 (make-tree 3
				    (make-tree 1 '() '())
				    (make-tree 5 '() '()))
			 (make-tree 9
				    '()
				    (make-tree 11 '() '()))))

(define tree2 (make-tree 3
			 (make-tree 1 '() '())
			 (make-tree 7
				    (make-tree 5 '() '())
				    (make-tree 9
					       '()
					       (make-tree 11 '() '())))))

(define (tree->list-iter tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

(tree->list-iter tree1)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (union-tree-set set1 set2)
  (list->tree (union-set (tree->list-iter set1) (tree->list-iter set2))))


(define (intersection-tree-set set1 set2)
  (list->tree (intersection-set (tree->list-iter set1) (tree->list-iter set2))))

(union-tree-set tree1 tree2)
(intersection-tree-set tree1 tree2)

; Поиск в словаре (множество ключ-значение организованных с помозью бинарного дерева)
(define (lookup-tree given-key map)
  (cond ((null? map) #f)
	((equal? given-key (key (entry map)))  (entry map))
	((< given-key (key (entry map)))
	 (lookup-tree given-key (left-branch map)))
	((> given-key (key (entry map)))
	 (lookup-tree given-key (right-branch map)))))
