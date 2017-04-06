#Chapter 1

##1.1

* 10
* 12
* 8
* 3
* 6
* a (with value 3)
* b (with value 4)
* 19
* #f (true)
* 4
* 16
* 6
* 16

##1.2

        (/ (+ 5
              4
              (- 2
                 (- 3
                    (+ 6
                       (/ 4 3)))))
           (\* 3
               (- 6 2)
               (- 2 7)))

##1.3

        (define (min2 a b)
                (if (< a b)
                    a
                    b))
        (define (min3 a b c)
                (min2 a
                      (min2 b c)))
        (define (square x)
                (\* x x))
        (define (f a b c)
                (- (+ (square a)
                      (square b)
                      (square c))
                   (square (min3 a b c))))

##1.4

If b is positive then return `(- a b)` (i.e.
 a - b), elsewise return `(+ a b)` (i.e. a + b).

##1.5

###Applicative-order Evaluation

        (test 0 (p))
        (test 0 (p))
        (test 0 (p))
        (test 0 (p))
        ......

In applicative-order evaluation, a function is not
 applied until all its parameters are evaluated. Thus the
 parameter `(p)` is constantly be expanded into itself
 over and over again while the function `test` can never
 get the chance to be applied. 

###Normal-order Evaluation

        (test 0 (p))
        (if (= 0 0) 0 (p))
        0

In normal-order evaluation, a function's parameters are
 not evaluated until it's needed, which means the function
 is always applied before its parameters are evaluated.
 Due to the special evaluation rule of `if` where the
 alternative expression is not evaluated if the
 predicate expression is evaluated to be true, the function
 `test` returns 0 before `(p)` gets the chance to be
 evaluated.

##1.6

The program will run infinitely until it runs out of memory.

`if` has a special evaluation rule where either the consequent or
 the alternative expression is evaluated depending on the value
 of the predicate expression. In the implementation of Newton's
 Method, the function should immediately return current value when
 the value is good enough, and here's where `if` comes in: if the
 requirement is met, the recursive call is not made.
 
 But the program using `new-if` always evaluates both the consequent
 and the alternative expression, causing the function to recursively
 call itself infinitely.

##1.7

For very small numbers, the threshold value 0.001 is too large to tell
 if current estimation is close enough to real value.

`(sqrt 0.00000001)` returns 0.03125, while 0.0001 is the real value.

For very large numbers, the precision is lost during calculation,
 hence the estimation can never get close enough ('close' defined by
 the threshold value 0.001) to real value.

`(sqrt (expt 10 150))` fails to return within a reasonable time.

Code after improvement:

        (define (sqrtIter guess lastGuess x)
                (if (goodEnough guess lastGuess)
                    guess
                    (sqrtIter (improve guess x)
                              guess
                              x)))
        (define (improve guess x)
                (average guess
                         (/ x guess)))
        (define (average x y)
                (/ (+ x y)
                   2))
        (define (goodEnough guess lastGuess)
                (< (/ (abs (- guess lastGuess))
                      lastGuess)
                   0.001))
        (define (sqrt x)
                (sqrtIter 1.0 x x))

After improvement, function calls mentioned above return
 1e-4 and 1e75 respectively.

##1.8

        (define (cbrtIter guess lastGuess x)
                (if (goodEnough guess lastGuess)
                    guess
                    (cbrtIter (improve guess x)
                              guess
                              x)))
        (define (improve guess x)
                (/ (+ (/ x
                         (* guess guess))
                      (* guess 2))
                   3))
        (define (goodEnough guess lastGuess)
                (< (/ (abs (- guess lastGuess))
                      lastGuess)
                   0.001))
        (define (cbrt x)
                (cbrtIter 1.0 x x))

##1.9

        (+ 4 5)
        (inc (+ 3 5))
        (inc (inc (+ 2 5)))
        (inc (inc (inc (+ 1 5))))
        (inc (inc (inc (inc (+ 0 5)))))
        (inc (inc (inc (inc 5))))
        (inc (inc (inc 6)))
        (inc (inc 7))
        (inc 8)
        9

The process is recursive.

        (+ 4 5)
        (+ 3 6)
        (+ 2 7)
        (+ 1 8)
        (+ 0 9)
        9

The process is iterative.

##1.10

        (A 1 10)
        (A 0 (A 1 9))
        (A 0 (A 0 (A 1 8)))
        ...
        (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
        (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
        (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 2)))))))))
        (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
        (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 4))))))))
        (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
        ...
        (A 0 512)
        (* 2 512)
        1024

        (A 2 4)
        (A 1 (A 2 3))
        (A 1 (A 1 (A 2 2)))
        (A 1 (A 1 (A 1 (A 2 1))))
        (A 1 (A 1 (A 1 2)))
        (A 1 (A 1 (A 0 (A 1 1))))
        (A 1 (A 1 (A 0 2)))
        (A 1 (A 1 (* 2 2)))
        (A 1 (A 1 4))
        (A 1 (A 0 (A 1 3)))
        (A 1 (A 0 (A 0 (A 1 2))))
        (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
        (A 1 (A 0 (A 0 (A 0 2))))
        (A 1 (A 0 (A 0 (* 2 2))))
        (A 1 (A 0 (A 0 4)))
        (A 1 (A 0 (* 2 4)))
        (A 1 (A 0 8))
        (A 1 (* 2 8))
        (A 1 16)
        ...
        65536

        (A 3 3)
        (A 2 (A 3 2))
        (A 2 (A 2 (A 3 1)))
        (A 2 (A 2 2))
        (A 2 (A 1 (A 2 1)))
        (A 2 (A 1 2))
        (A 2 (A 0 (A 1 1)))
        (A 2 (A 0 2))
        (A 2 (* 2 2))
        (A 2 4)
        ...
        65536

* `(f n)` equals `(A 0 n)` equals `(* 2 n)` which computes 2n for positive n.
* `(g n)` equals `(A 1 n)` equals `(A 0 (A 1 (- n 1)))` equals `(* 2 (A 1 (- n 1)))`
 (which, though, is not the way it's evaluated).
 In the basic case `(A 1 1)` returns 2, thus `(g n)` computes 2^n for positive n.
* `(h n)` equals `(A 2 n)` equals `(A 1 (A 2 (- n 1)))` equals `(expt 2 (A 2 (- n 1)))`
 (which, though, is not the way it's evaluated).
 In the basic case `(A 2 1)` returns 2. Informally, `(h n)` computes 2^(2^(2^...^(2^(2^2))...)) where the
 number of 2 is n. The concise definition can be given recursively as follow:
 `(h 1)` returns 2 for n=1, and returns `(expt 2 (h (- n 1)))` for n larger than 1.

##1.11

Recursive version:

        (define (f n)
                (if (< n 3)
                    n
                    (+ (f (- n 1))
                       (* 2 (f (- n 2)))
                       (* 3 (f (- n 3))))))

Iterative version:

        (define (f n)
                (define (fIter n curr a b c)
                        (if (= curr n)
                            a
                            (fIter n (+ 1 curr) (+ a (* 2 b) (* 3 c)) a b)))
                (if (< n 3)
                    n
                    (fIter n 2 2 1 0)))

##1.12

        (define (pascal line row)
                (if (or (= row 1)
                        (= row line))
                    1
                    (+ (pascal (- line 1) (- row 1))
                       (pascal (- line 1) row))))
