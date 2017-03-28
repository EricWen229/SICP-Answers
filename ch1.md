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
* #f
* 4
* 16
* 6
* 16

##1.2

        (/
		  (+
		    5
			4
			(-
			  2
			  (-
			    3
				(+
				  6
				  (/ 4 3)))))
		  (\*
		    3
			(- 6 2) (- 2 7)))

##1.3

        (define (min2 a b)
		  (if
			(< a b)
		    a
			b))
        (define (min3 a b c)
		  (min2
			a
		    (min2 b c)))
        (define (square x) (\* x x))
        (define (f a b c)
		  (-
		    (+
			  (square a)
			  (square b)
			  (square c))
			(square
		      (min3 a b c))))

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

In applicative-order evaluation, a function will not be
 applied until all its parameters are evaluated. Thus the
 parameter `(p)` will constantly be expanded into itself
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
 alternative expression will not be evaluated if the
 predicate expression is evaluated to be true, the function
 `test` returns 0 before `(p)` gets the chance to be
 evaluated.

