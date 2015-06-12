#lang racket

; Programming is debugging. 100% REPL all the time.
; Don't copy paste. it leads to stupid bugs.

(require (only-in racket/base [apply apply-builtin]))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "NYI"))))

(define (apply proc args)
  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         (eval-sequence
           (procedure-body proc)
           (extend-environment (procedure-parameters proc)
                               args
                               (procedure-environment proc))))
        (else (error "Unknown procedure type -- APPLY" proc))))

(define (variable? exp)             (symbol? exp))
(define (application? exp)          (pair? exp)) ; later in cond-statement
(define (self-evaluating? exp)      (or (number? exp) (string? exp)))
(define (tagged-list? exp tag)      (and (pair? exp) (eq? (car exp) tag)))
(define (last-exp? seq)             (null? (cdr seq)))
(define (no-operands? ops)          (null? ops))
(define (quoted? exp)               (tagged-list? exp 'quote))
(define (assignment? exp)           (tagged-list? exp 'set!))
(define (definition? exp)           (tagged-list? exp 'define))
(define (lambda? exp)               (tagged-list? exp 'lambda))
(define (if? exp)                   (tagged-list? exp 'if))
(define (begin? exp)                (tagged-list? exp 'begin))
(define (cond? exp)                 (tagged-list? exp 'cond))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (compound-procedure? proc)  (tagged-list? proc 'procedure))

(define (first-exp seq)                 (car seq))
(define (rest-exps seq)                 (cdr seq))
(define (enclosing-environment env)     (cdr env))
(define (first-frame env)               (car env))
(define (frame-variables frame)         (car frame))
(define (frame-values frame)            (cdr frame))
(define (text-of-quotation exp)         (cadr exp))
(define (assignment-variable exp)       (cadr exp))
(define (assignment-value exp)          (caddr exp))
(define (lambda-parameters exp)         (cadr exp))
(define (lambda-body exp)               (cddr exp))
(define (operator exp)                  (car exp))
(define (operands exp)                  (cdr exp))
(define (first-operand ops)             (car ops))
(define (rest-operands ops)             (cdr ops))
(define (procedure-parameters p)        (cadr p))
(define (procedure-body p)              (caddr p))
(define (procedure-environment p)       (cadddr p))
(define (primitive-implementation proc) (cadr proc))

(define the-empty-environment                '())
(define (make-lambda parameters body)        (cons 'lambda (cons parameters body)))
(define (make-frame variables values)        (cons variables values))
(define (make-procedure parameters body env) (list 'procedure parameters body env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
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

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (apply-primitive-procedure proc args)
  (apply-builtin (primitive-implementation proc) args))

(define primitive-procedures
  (list (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'null? null?)
         (list '+ +)))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ";;; Eval input:")
(define output-prompt ";;; Eval input:")
(define (prompt-for-input string) (newline) (newline) (display string) (newline))
(define (announce-output string) (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))



; NYI

; in eval:
;        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))

; mutable cons cells etc, setup-env
;    (define-variable! 'true true initial-env)
;    (define-variable! 'false false initial-env)

;(define (add-binding-to-frame! var val frame)
;  (set-car! frame (cons var (car frame)))
;  (set-cdr! frame (cons val (cdr frame))))

;(define (eval-assignment exp env)
;  (set-variable-value! (assignment-variable exp)
;                       (eval (assignment-value exp) env)
;                       env)
;  'ok)

;(define (eval-definition exp env)
;  (define-variable! (definition-variable exp)
;                    (eval (definition-value exp) env)
;                    env))

;(define (set-variable-value! var val env)
;  (define (env-loop env)
;    (define (scan vars vals)
;      (cond ((null? vals) (env-loop (enclosing-environment env)))
;            ((eq? var (car vars)) (set-car! vals val))
;            (else (scan (cdr vars) (cdr vals)))))
;    (if (eq? env the-empty-environment)
;      (error "Unbound variable -- SET!" var)
;      (let ((frame (first-frame env)))
;        (scan (frame-variables frame) (frame-values frame)))))
;  (env-loop env))

;(define (define-variable! var val env)
;  (let ((frame (first-frame env)))
;    (define (scan vars vals)
;      (cond ((null? vars) (add-binding-to-frame! var val frame))
;            ((eq? var (car vars)) (set-car! vals val))
;            (else (scan (cdr vars) (cdr vals)))))
;    (scan (frame-variables frame) (frame-values frame))))
;

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
;

;eval-assignment exp env
;eval-definition exp env
;eval-if exp env
;eval-sequence foo bar
;begin-actions exp
;cond->if exp

