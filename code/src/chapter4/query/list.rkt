#lang racket

(provide flatmap)

(define (flatmap fn lst)
  (if (null? lst)
      '()
      (append (fn (car lst))
              (flatmap fn (cdr lst)))))
