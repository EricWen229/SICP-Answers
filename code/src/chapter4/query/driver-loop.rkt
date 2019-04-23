#lang racket

(require "qeval.rkt")

(provide query-driver-loop)

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query output:")

(define (query-driver-loop)
  (let ([raw-query (read-query)])
    (if (eof-object? raw-query)
        'exit
        (begin (print-query-result (qeval raw-query))
               (query-driver-loop)))))

(define (read-query)
  (print-input-prompt)
  (read))

(define (print-query-result result)
  (print-output-prompt)
  (result)
  (newline))

(define (print-input-prompt)
  (display input-prompt)
  (newline))

(define (print-output-prompt)
  (newline)
  (display output-prompt)
  (newline))

