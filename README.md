# SICP-Solutions

Solutions to exercises in SICP

This is my personal collection of solutions to exercises in SICP. Basically NONE of these solutions are guaranteed to be 100% correct or the most effective (for better solutions please refer to websites like http://community.schemewiki.org/?SICP-Solutions, which are hosted and maintained by a bunch of more experienced guys), and they are NOT supposed to be, since they're simply my own thoughts and practices in the course of reading this book. This collection will keep getting updated till the end, but I'm really unsure of the schedule. Later (after I've finished reading this book and completed all the exercises) I might go through this book again for multiple times and refine these solutions based on my latest reflection.

## Update: 2018-9-11

I've been thinking about whether to thoroughly test each piece of code and I've added tests for some of the solutions. But in the end I decide to stop doing so. There're a couple of reasons for this. First of all the main purpose of reading this book and doing exercises is to understand the ideas, not to write industrial-level code. Also, it takes too much time and effort to write supporting code in order for the core part to run, and to be tested. Nevertheless, I will still include some test cases in the solutions.

## Update: 2018-12-27

Finally finished chapter 3 before 2019 comes. To me this is by far the most mind-bending part of the book, and I still haven't been able to find out an intuitive way of think about streams with my mind tangled up with closures, lazy evaluation and memoization.

## Update: 2018-12-30

Take the `integer` stream in section "Defining streams implicitly" for example:

```lisp
(define integers (cons-stream 1 (add-streams ones integers)))
```

Its correctness is not really that hard to figure out, but the time & space complexities behind this are really obscure to me. The first time I see it, I thought accessing an element of the stream would need a lot of streams, from which elements were added together and produce the element being accessed, just like this:

```
# 0 1 2 3 4
v 1 2 3 4 5 ...
  1 1 1 1 1
    + + + +
    1 1 1 1
      + + +
      1 1 1
        + +
        1 1
          +
          1
```

After some real dull, careful and annoying analysis, it turns out not so many elements are needed thanks to memoization.

Now we've known that memoization saves us a lot of space and time, yet it still remains mysterious how bad it is if memoization is not present. When we see a imperative stype program like this:

```c
int fib(int n) {
  if (n <= 1) return 0;
  else return fib(n - 1) + fib(n - 2);
}
```

We know it'll be slow, and we know how slow it's going to be. The time complexity $T(N)$ is super easy to analyze:

$$
T(N) = T(N - 1) + T(N - 2)
$$

Now, suppose memoization is not present, let take a look at this stream computing Fibonacci numbers:

```lisp
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                        fibs))))
```

We also know it's going to be slow, but it's way harder to figure out how slow it'll be, how much space it takes, etc.

Streams do not have to be so obscure, actually. We can define this Fibonacci stream in a more explicit way:

```lisp
(define (make-stream start next transform)
  (define (helper curr)
    (cons-stream (transform curr)
                 (helper (next curr))))
  (helper start))

(define fibs
  (make-stream (cons 0 1)
               (lambda (pair)
                 (cons (cdr pair)
                       (+ (car pair)
                          (cdr pair))))
               car))
```

Till I find a more intuitive way of understanding implicitly defined streams, I'll just stick to the explicit way for now.
