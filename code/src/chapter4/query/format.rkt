#lang racket

(provide process-raw-query)
(provide var?)

(define (process-raw-query raw-query)
  (map-over-symbols expand-question-mark raw-query))

(define (map-over-symbols fn exp)
  (cond [(pair? exp)
         (cons (map-over-symbols fn (car exp))
               (map-over-symbols fn (cdr exp)))]
        [(symbol? exp)
         (fn exp)]
        [else exp]))

(define (expand-question-mark sym)
  (let ([symstr (symbol->string sym)])
    (if (string=? (substring symstr 0 1)
                  "?")
        (list '?
              (string->symbol
               (substring symstr 1 (string-length symstr))))
        sym)))

(define (tagged? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (var? exp)
  (tagged? exp '?))