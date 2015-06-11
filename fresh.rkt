#lang racket

; works
; (lookup '+ global-env)

; next
; (eval '(+ 1 2) global-env)

(define global-env
  (list
    (list (list 'car car)
          (list 'cdr cdr)
          (list '+ +))))

(define (ev exp) (eval exp global-env))

(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          (else (error "IDK")))))

(define assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (caar alist)) (car alist))
          (else
            (assq sym (cdr alist))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error "UBV"))
          (else
            ((lambda (vcell)
               (cond ((eq? vcell '())           
                      (lookup sym
                              (cdr env)))
                     (else (cdr vcell))))
             (assq sym (car env)))))))
