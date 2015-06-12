#lang racket

; Programming is debugging. 100% REPL all the time.
; Don't copy paste. it leads to stupid bugs.

(require scheme/mpair)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        (else (error "NYI"))))

; apply

;(define (self-evaluating? exp)
;  (cond ((number? exp) true)
;        ((string? exp) true)
;        (else false)))
;
;(define (tagged-list? exp tag)
;  (if (pair? exp)
;    (eq? (car exp) tag)
;    false))

(define (variable? exp) (symbol? exp))
(define (application? exp) (pair? exp)) ; later in cond-statement
(define (self-evaluating? exp) (or (number? exp) (string? exp)))
(define (tagged-list? exp tag) (and (pair? exp) (eq? (car exp) tag)))

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
            ((eq? var (mcar vars)) (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
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

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars)) (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (text-of-quotation exp) (cadr exp))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;(define (definition-variable exp)
;  (if (symbol? (cadr exp))
;    (cadr exp)
;    (caadr exp)))
;
;(define (definition-value exp)
;  (if (symbol? (cadr exp))
;    (caddr exp)
;    (make-lambda (cdadr exp)   ; formal parameters
;                 (cddr exp)))) ; body

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;(define (eval-definition exp env)
;  (define-variable! (definition-variable exp)
;                    (eval (definition-value exp) env)
;                    env))

; assume we have this:
; apply-primitive-procedure proc args

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


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
