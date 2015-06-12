#lang racket

; Programming is debugging. 100% REPL all the time.

(require scheme/mpair)

; eval

; apply

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))
(define (application? exp) (pair? exp)) ; later in cond-statement

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (quoted? exp)               (tagged-list? exp 'quote))
(define (assignment? exp)           (tagged-list? exp 'set!))
(define (definition? exp)           (tagged-list? exp 'define))
(define (lambda? exp)               (tagged-list? exp 'lambda))
(define (if? exp)                   (tagged-list? exp 'if))
(define (begin? exp)                (tagged-list? exp 'begin))
(define (cond? exp)                 (tagged-list? exp 'cond))
(define (primitive-procedure? proc) (tagged-list? proc 'proc))
(define (compund-procedure? proc)   (tagged-list? proc 'procedure))

; operations on environments
;
; this is an env, first-frame and enclosing environment
; (mcons (mcons (mcons 'a (mcons 'b '())) (mcons 1 (mcons 2 '()))) '())
; (define e0 (extend-environment (mlist 'a 'b) (mlist 1 2) the-empty-environment))
;
(define (enclosing-environment env)    (mcdr env))
(define (first-frame env)             (mcar env))
(define the-empty-environment         '())
(define (make-frame variables values) (mcons variables values))
(define (frame-variables frame)       (mcar frame))
(define (frame-values frame)          (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
    (mcons (make-frame vars vals) base-env)
    (if (< (mlength vars) (mlength vals))
      (error "Too many arguments supplied")
      (error "Too few arguments supplied"))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vals) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vals) (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars)) (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

;(define primitive-procedures
;  (list (list 'car car)
;        (list 'cdr cdr)
;        (list 'cons cons)
;        (list 'null? null?)
;        ;; more
;        ))

;(define (primitive-procedure-names) (map car primitive-procedures))

;text-of-quotation exp
;eval-assignment exp env
;eval-definition exp env
;eval-if exp env
;make-procedure foo bar baz
;lambda-parameters exp
;lambda-body exp
;eval-sequence foo bar
;begin-actions exp
;cond->if exp
;operator exp
;lis-of-values foo bar
;operands exp
