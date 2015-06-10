#lang racket ; SICP lecture try to eval ; (eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) '())

; (((lambda (x) (lambda (y) (+ x y))) 3) 4)

;
; works!
; (eval '(lambda (x) x) '())
; (eval ((lambda (x) x) '(lambda (y) y)) '())
; (eval '(lambda (x) (+ x 1)) '())

; No conditionals yet

; only import foo and bar?
(require (only-in racket/base
                  [apply apply-builtin]))

(define (print x) (display x))


(define initial-env
  (list (list 'car car)
        (list 'cdr cdr)
        (list '+ +)))

; what ddoes init-env look like? e0

(define assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (caar alist))
           (car alist))
          (else
            (assq sym (cdr alist))))))

(define (lookup sym env)
    (cond ((eq? env '()) (error "UBV"))
          (else
            ((lambda (vcell)
               (cond ((eq? vcell '())
                      (lookup sym
                              (cdr env)))
                     (else (cdr vcell)))))
            (assq sym (car env)))))

(define (evcond clauses env) '()) ; TODO

(define (evlist l env)
  (cond ((eq? l '()) '())
        (else
          (cons (eval (car l) env)
                (evlist (cdr l) env)))))

(define (eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup exp env))
        ((eq? (car exp) 'quote) (cadr env))
        ((eq? (car exp) 'lambda)
         (list 'closure (cdr exp) env))
        ((eq? (car exp) 'cond)
         (evcond (cdr exp) env))
        (else (apply (eval (car exp) env)
                        (evlist (cdr exp) env)))))

; eval '+? not in environment!

(define (apply-primop proc args)
  (apply-builtin
    (cadr proc) args))

(define pair-up
  (lambda (vars vals)
    (cond
      ((eq? vars '())
       (cond ((eq? vals '()) '())
             (else (error "TMA"))))
      ((eq? vals '()) (error "TFA"))
      (else
        (cons (cons (car vars)
                    (car vals))
              (pair-up (cdr vars)
                       (cdr vals)))))))

(define (bind vars vals env)
  (cons (pair-up vars vals)
        env))

;(define (tagged-list?
;(define (primitive? proc)
;  (tagged-list? proc 'primitive))

(define (apply proc args)
  (cond ((primitive? proc)
         (apply-primop proc args))
        ((eq? (car proc) 'closure)
         (eval (cadadr proc)
               (bind (caadr proc)
                     args
                     (cadar proc))))
         (else "error")))

; where is apply?
(define (rep x) (print (eval x '())))

; read-line just gives us strings
(define (repl)
  (display "> ")
  (rep (read))
  (repl))

;(repl)
