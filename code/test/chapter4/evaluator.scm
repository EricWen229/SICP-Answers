;; this file contains tests for the scheme evaluator implemented by ourselves
;; it's not supposed to be run with other scheme interpreters

;; ===============
;; assertion utils
;; ===============

(define (assert-true e)
  (assert e))
(define (assert-false e)
  (assert (not e)))
(define (assert-equal a b)
  (assert-true (= a b)))
(define (assert-unequal a b)
  (assert-false (= a b)))
(define (assert-equal-list alst blst)
  (if (and (null? alst)
           (null? blst))
      'undefined
      (begin (assert-equal (car alst)
                           (car blst))
             (assert-equal-list (cdr alst)
                                (cdr blst)))))

;; =================
;; basic expressions
;; =================

;; self-evaluating
(assert-equal 233 233)
(assert-unequal 233 2333)
(assert-true (null? ()))

;; quote
(assert-equal 1 (car (quote (1 2 3))))
(assert-equal 2 (car (cdr (quote (1 2 3)))))
(assert-equal 3 (car (cdr (cdr (quote (1 2 3))))))

;; assignment
(define foo 233)
(assert-equal foo 233)
(set! foo 2333)
(assert-equal foo 2333)
(set! foo 23333)
(assert-equal foo 23333)

;; if
(assert-true (if (= (+ 1 1) 2) true false))
(assert-true (if (= (+ 1 1) 3) false true))
(assert-true (if (= (+ 1 1) 2) true (quotient 1 0)))
(assert-true (if (= (+ 1 1) 3) (quotient 1 0) true))

;; and
(assert-true (and true true true))
(assert-false (and true true false))
(assert-false (and true false false))
(assert-false (and false false false))
(assert-false (and false false true))
(assert-false (and false true true))

;; or
(assert-false (or false false false))
(assert-true (or false false true))
(assert-true (or false true true))
(assert-true (or true true true))
(assert-true (or true true false))
(assert-true (or true false false))

;; lambda
(assert-equal 2 ((lambda (x y) (+ x y)) 1 1))
(assert-equal 2 ((lambda (x) (* 2 x)) 1))

;; begin
(assert-true (begin false false true))
(assert-true (begin true true true))
(assert-true (not (begin true true false)))

;; ===============
;; list operations
;; ===============

(define (map op lst)
  (if (null? lst)
      ()
      (cons (op (car lst))
            (map op (cdr lst)))))

(define (filter op lst)
  (cond ((null? lst) ())
        ((op (car lst))
         (cons (car lst)
               (filter op (cdr lst))))
        (else (filter op (cdr lst)))))

;; named fold-left
;; because it combines current element
;; with result of combining all elements on the left
(define (foldl init op lst)
  (if (null? lst)
      init
      (foldl (op (car lst)
                 init)
             op
             (cdr lst))))

;; named fold-right
;; because it combines current element
;; with result of combining all elements on the right
(define (foldr init op lst)
  (if (null? lst)
      init
      (op (car lst)
          (foldr init
                 op
                 (cdr lst)))))

(define (reverse-list lst)
  (foldl ()
         (lambda (elem curr)
           (cons elem curr))
         lst))

(define foo (quote (1 2 3 4)))
(assert-equal-list (map (lambda (x) (+ x 1)) foo)
                   (quote (2 3 4 5)))
(assert-equal-list (filter (lambda (x) (> x 2)) foo)
                   (quote (3 4)))
(assert-equal (foldl 0 + foo) 10)
(assert-equal-list (reverse-list foo)
                   (quote (4 3 2 1)))

;; =================
;; stream operations
;; =================

(define (cons-stream start (lazy-memo rest))
  (lambda (f) (f start rest)))

(define (stream-car s)
  (s (lambda (start rest) start)))

(define (stream-cdr s)
  (s (lambda (start rest) rest)))

(define (make-stream init next transform)
  (define (iter-helper curr)
    (cons-stream (transform curr)
                 (iter-helper (next curr))))
  (iter-helper init))

(define (add-stream sa sb)
  (cons-stream (+ (stream-car sa)
                  (stream-car sb))
               (add-stream (stream-cdr sa)
                           (stream-cdr sb))))

(define (stream-to-list n stream)
  (if (= n 0)
      ()
      (cons (stream-car stream)
            (stream-to-list (- n 1)
                            (stream-cdr stream)))))

(define ones (cons-stream 1 ones))
(define integers-explicit
  (make-stream 1
               (lambda (x) (+ x 1))
               (lambda (x) x)))
(define integers-implicit
  (cons-stream 1
               (add-stream ones
                           integers-implicit)))

(assert-equal-list (stream-to-list 5 ones)
                   (quote (1 1 1 1 1)))
(assert-equal-list (stream-to-list 5 integers-implicit)
                   (quote (1 2 3 4 5)))
(assert-equal-list (stream-to-list 5 integers-explicit)
                   (quote (1 2 3 4 5)))

;; ==========
;; algorithms
;; ==========

(define (quicksort lst)
  (if (null? lst)
      ()
      (let* ((pivot (car lst))
             (smaller-portion
              (filter (lambda (x) (< x pivot)) lst))
             (equal-portion
              (filter (lambda (x) (= x pivot)) lst))
             (larger-portion
              (filter (lambda (x) (> x pivot)) lst)))
        (append (quicksort smaller-portion)
                equal-portion
                (quicksort larger-portion)))))

(assert-equal-list (quicksort (quote (3 2 1 5 4)))
                   (quote (1 2 3 4 5)))
