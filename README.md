# Sai

My toy lisp interpreter, in under 500 lines of code.

# Running it

Open a Racket REPL and write (driver-loop).

# Things that work

* (eval '(lambda (x) x) '())
* (eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) '())
* ((lambda (x) (+ 1 x)) 3)
* (define bar (lambda (x) (+ 1 x))) (and bar is bound)


# Things that don't work

Probably the rest. Most notably, conditionals.
