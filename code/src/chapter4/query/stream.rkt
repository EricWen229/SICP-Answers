#lang racket

(provide create-empty-stream)
(provide singleton-stream)
(provide stream-consume)
(provide stream-flatmap)

(define (create-empty-stream)
  empty-stream)

(define (singleton-stream elem)
  (stream-cons elem
               (create-empty-stream)))

(define (stream-consume fn stream)
  (if (stream-empty? stream)
      'done
      (begin (fn (stream-first stream))
             (stream-consume fn
                             (stream-rest stream)))))

(define (stream-flatmap fn stream)
  (stream-flatten (stream-map fn stream)))

(define (stream-flatten stream-of-stream)
  (if (stream-empty? stream-of-stream)
      stream-of-stream
      (stream-append-delayed (stream-first stream-of-stream)
                             (delay (stream-flatten (stream-rest stream-of-stream))))))

(define (stream-append-delayed first rest-delayed)
  (if (stream-empty? first)
      (force rest-delayed)
      (stream-cons (stream-first first)
                   (stream-append-delayed (stream-rest first)
                                          rest-delayed))))
