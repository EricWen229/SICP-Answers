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

## Update: 2018-1-5

Just finished implementing a new version of Scheme evaluator. This version utilizes the analysis technique introduced in section 4.1.7, and the backward compatible lazy evaluation is also implemented.

At first the idea is quite confusing. We know that `analyze` takes an expression and returns a procedure which takes an environment and returns the result of evaluating the expression specified earlier in the given environment. After introducing lazy evaluation, specifically thunks, things start to get trickier, as it seems not so clear where these thunks could appear. Could `analyze`'s argument be a thunk? Could `analyze` possibly return a thunk?

But after some careful thoughts, it turns out that I was mixing up the concept of expression and value.

Specifically, expression is more of a "static" concept. Expressions' semantics are specified by the language specification (implementation can also be seen as some sort of specification), not the runtime behaviour, thus we don't need to actually run the program when we `analyze` the expressions and generate procedures according to their semantics.

On the other hand, value is more of a "dynamic" concept. Except for some special cases like constants, there's no way we can deduce a variable's value by statically analyze the program without running it (some might say programmers can "analyze" a program and deduce the value of a certain variable at certain time but in such scenario the programmer actually acts as an evaluator), and it only exists at runtime.

What may seem confusing is that in some cases expressions and values can "interleave", where an entity seems both an expression and a value. For example, in the evaluator we just implemented, user program can include a string as an expression, which string is also the internal represenation of a string value. The same also applies to numbers and `null`. But, keep in mind that this sharing of representation is just for convenience, and that string expression and string value are conceptually different in spite of this.

Back to lazy evaluation. Thunk is a kind of value, which means it will never be created during analsis phase. It's created only when the program is evaluated, and can be passed around, bound to variables or used to compute other values at runtime. We can safely assume that `analyze` will neither need to handle thunks nor produce thunks as results.

Speaking of how thunk can be used, it's noteworthy that thunks are only forced in several cases: when used by an `if` expression as the predicate or when as arguments of primitive procedure application. At first I was wondering whether I should force thunks in some other situations, but at last this proved to be totally unnecessary. Only when calling primitive procedures or using primitive constructs such as `if` do we CARE about the actual value of a value.

This applies to not only thunks but also other types of values. Take integers for example. When not using them for basic operations like calculations and comparisons, all we use them for is just passing them around as arguments, results or assigning them to variables. And when we do so, all the program does is simply copying them from memory to registers, from registers to registers or from registers to memory. The actual values don't matter at all. Only when we, for example, add two integers, does the program actually use their value, where the adder in the CPU analyzes them bit by bit and computes theirs sum.

And, seems like I just missed an important case, and that's when thunks are printed to the console. After all we, humans, also care about the value.
