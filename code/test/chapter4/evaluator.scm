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
