# Sai

Sai is my celebration to the spirit in the machine.

It's (yet another) toy lisp interpreter, in under 500 lines of code. Ended up
under 200 actually, but it is very rudimentary.  

The only thing different from it and the one in SICP, which is what I used as a
guide, is that it's built up with Racket's immutable lists. The only mutable
thing is the *vars* hash map, which is how you define functions.

# Running it

Open a Racket REPL and write (driver-loop).

# Things that work

* (eval '(lambda (x) x) '())
* (eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) '())
* ((lambda (x) (+ 1 x)) 3)
* (define bar (lambda (x) (+ 1 x))) (and bar is bound)

* Append!
```
(define (append x y)
    (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(append '(a b c) '(d e f))
```

# Things that don't work

Probably everything else.

# Future work

- Macro system!
- Fewer primitives (just cond and atom, for example)
- More semantics and example fns
- Clojure-ish data structures?
