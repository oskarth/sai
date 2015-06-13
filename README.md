# Sai

My toy lisp interpreter, in under 500 lines of code.

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
