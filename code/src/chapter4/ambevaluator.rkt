#lang racket

(define (repl)
  (ambeval (read)
           (lambda (val next)
             (display val)
             (newline)
             (next))
           (lambda ()
             (display "END")
             (newline)
             (repl))))

(define (ambeval exp succeed fail)
  ((analyze exp) succeed
                 fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((amb? exp)
         (analyze-amb exp))
        (else
         (error "Unknown expression type" exp))))

(define (self-evaluating? exp)
  (number? exp))
(define (analyze-self-evaluating exp)
  (lambda (succeed fail)
    (succeed exp fail)))

(define (amb? exp)
  (tagged-list? exp 'amb))
(define (amb-choices exp)
  (cdr exp))
(define (analyze-amb exp)
  (let ((cprocs (map analyze
                     (amb-choices exp))))
    (lambda (succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define (tagged-list? lst tag)
  (and (pair? lst)
       (eq? (car lst)
            tag)))
  