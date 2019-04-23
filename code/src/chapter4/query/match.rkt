#lang racket

(require "format.rkt")
(require "frame.rkt")

(provide pattern-match)

(define (pattern-match pattern datum frame)
  (cond [(eq? frame (the-fail-frame)) (the-fail-frame)]
        [(equal? pattern datum) frame]
        [(var? pattern) (extend-if-consistent pattern datum frame)]
        [(and (pair? pattern)
              (pair? datum))
         (pattern-match (cdr pattern)
                        (cdr datum)
                        (pattern-match (car pattern)
                                       (car datum)
                                       frame))]
        [else (the-fail-frame)]))

(define (extend-if-consistent pattern datum frame)
  (let ([binding (binding-in-frame pattern frame)])
    (if binding
        (pattern-match (binding-value binding)
                       datum
                       frame)
        (extend-frame pattern datum frame))))
