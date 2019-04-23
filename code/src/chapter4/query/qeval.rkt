#lang racket

(require "database.rkt")
(require "format.rkt")
(require "frame.rkt")
(require "list.rkt")
(require "match.rkt")

(provide qeval)

(define (qeval raw-query)
  (let* ([query (process-raw-query raw-query)]
         [query-result (qeval-with-frames query (list (the-empty-frame)))])
    (lambda ()
      (map (lambda (frame)
             (display (instantiate query frame))
             (newline))
           query-result))))

(define (instantiate pattern frame)
  (cond [(var? pattern)
         (let ([binding (binding-in-frame pattern frame)])
           (if binding
               (instantiate (binding-value binding) frame)
               pattern))]
        [(pair? pattern)
         (cons (instantiate (car pattern) frame)
               (instantiate (cdr pattern) frame))]
        [else pattern]))

(define (qeval-with-frames query-pattern frame-list)
  (flatmap
   (lambda (frame)
     (match-assertions query-pattern
                       frame))
   frame-list))

(define (match-assertions query-pattern frame)
  (filter (lambda (frame) (not (eq? frame 'fail)))
          (map (lambda (assertion)
                 (pattern-match query-pattern assertion frame))
               (get-assertions))))

