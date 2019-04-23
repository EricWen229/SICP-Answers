#lang racket

(require compatibility/mlist)

(provide get-assertions)

(define ASSERTIONS
  '((you good)
    (i good)
    (he bad)))

(define (get-assertions)
  ASSERTIONS)

