#lang racket

(require racket/match)

; eval matches on the type of expression:
(define (eval exp env)
  (match exp
         [`(,f ,e)          (apply (eval f env) (eval e env))]
         [`(lambda ,v . ,e) `(closure ,exp ,env)]
         [(? symbol?)       (cadr (assq exp env))]))

; apply destructures the function with a match too:
(define (apply f x)
  (match f
         [`(closure (lambda ,v . ,body) ,env)
           (eval body (cons `(,v ,x) env))]))

; read and parse stdin, then evaluate:
(display (eval (read) '()))
(newline)
