#lang racket

(provide the-empty-frame)
(provide the-fail-frame)

(provide make-binding)
(provide binding-variable)
(provide binding-value)

(provide extend-frame)
(provide binding-in-frame)

(define (the-empty-frame)
  '())

(define (the-fail-frame)
  'fail)

(define (make-binding var val)
  (cons var val))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (extend-frame var val frame)
  (cons (make-binding var val)
        frame))

(define (binding-in-frame var frame)
  (assoc var frame))