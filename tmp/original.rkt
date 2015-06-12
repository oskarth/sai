#lang racket

(define evalquote
  (lambda (fn x)
    (apply fn x '())))

(define apply
  (lambda (fn x a)
    (cond ((symbol? fn)
           (cond ((eq? fn 'car) (caar x))
                 ((eq? fn 'cdr) (cdar x))
                 ((eq? fn 'cons) (cons (car x) (cadr x)))
                 ((eq? fn 'symbol?) (quote (car x))) ; XXX
                 ((eq? fn 'eq?) (eq? (car x) (cadr x)))
                 (else (apply (eval fn a) x a)))
           ((eq? (car fn) 'lambda)
            (eval (caddr fn) (pairlis (cadr fn) x a)))
           ((eq? (car fn) 'label)
            (apply (caddr fn) x (cons (cons (cadr fn) (caddr fn)) a)))))))

(define eval
  (lambda (e a)
    (cond ((symbol? e) (cdr (assoc e a)))
          ((symbol? (car e))
           (cond ((eq? (car e) 'quote) (cadr e))
                 ((eq? (car e) 'cond) (evcon (cdr e) a))
                 (else (apply (car e) (evlis (cdr e) a) a))))
          (else (apply (car e) (evlis (cdr e) a) a)))))

(define pairlis
  (lambda (x y a)
    (cond ((null x) a)
          (else (cons (cons (car x) (car y))
                      (pairlis (cdr x) (cdr y) a))))))

(define assoc
  (lambda (x a)
    (cond ((eq? (caar a) x) (car a))
          (else (assoc (x (cdr a)))))))

(define evcon
  (lambda (c a)
    (cond ((eval (caar c) a) (eval (cadar c) a))
          (else (evcon (cdr c) a)))))

(define evlis
  (lambda (m a)
    (cond ((null m) '())
          (else (cons (eval (car m) a) (evlis (cdr m) a))))))
