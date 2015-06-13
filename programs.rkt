; this works! definitions, primitives, if-statements, lexical scope.

(define (append x y)
  (if (null? x)
    y
    (cons (car x)
          (append (cdr x) y))))

(append '(a b c) '(d e f))
