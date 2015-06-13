; Can't write this yet cause we don't have if! (and probably not null? either)
(define (append x y)
  (if (null? x)
    y
    (cons (car x)
          (append (cdr x) y))))
