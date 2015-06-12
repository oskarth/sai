#lang racket

; Programming is debugging. 100% REPL all the time.
; Don't copy paste. it leads to stupid bugs.

(require scheme/mpair)

; Alternatively, call (define apply-builtin apply) before defining our apply.
(require (only-in racket/base [apply apply-builtin]))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "NYI"))))

(define (apply proc args)
  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
        (else (error "Unknown procedure type -- APPLY" proc))))

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
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (compound-procedure? proc)  (tagged-list? proc 'procedure))

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

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-builtin (primitive-implementation proc) args))

; do we not need +?
(define primitive-procedures
  (mlist (mlist 'car car)
         (mlist 'cdr cdr)
         (mlist 'cons cons)
         (mlist 'null? null?)
         (mlist '+ +)
         ;; more
         ))

; this is gettting seriously annoying, surel we can coerce at just a few points?
; not everything is mutable, just...the environment :<
(define (mcadr x) (mcar (mcdr x)))

(define (primitive-procedure-names) (mmap mcar primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
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






;eval-assignment exp env
;eval-definition exp env
;eval-if exp env
;make-procedure foo bar baz
;lambda-parameters exp
;lambda-body exp
;eval-sequence foo bar
;begin-actions exp
;cond->if exp

