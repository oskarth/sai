#lang racket

(require (only-in racket/base [apply apply-builtin]))

; this is our only mutable piece of code. It holds a mapping between vars and
; values, such as function definitions. There's only one namespace.
(define *ht* (make-hash))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (cadr exp))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (cadr exp) (cddr exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (car exp) env)
                (list-of-values (cdr exp) env)))
        (else (error "NYI"))))

(define (apply proc args)
  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         (eval-sequence
           (caddr proc)
           (extend-environment (cadr proc) args (cadddr proc))))
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
(define (true? x)                   (not (eq? x false)))
(define (false? x)                  (eq? x false))

(define (cond-else-clause? clause) (eq? (car clause) 'else))

(define (make-lambda parameters body)        (cons 'lambda (cons parameters body)))
(define (make-frame variables values)        (cons variables values))
(define (make-procedure parameters body env) (list 'procedure parameters body env))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

; '('define foo 1)) => 'foo and 1 respectivel here
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
 (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) (cddr exp))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent))

(define (eval-if exp env)
  (if (true? (eval (cadr exp) env))
    (eval (caddr exp) env)
    (eval (if-alternative exp) env)))

(define (make-begin seq) (cons 'begin seq))

(define (begin-actions exp) (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (car seq))
        (else (make-begin seq))))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses)) (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cdr first))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (car first)
                 (sequence->exp (cdr first))
                    (expand-clauses rest))))))

(define (cond->if exp) (expand-clauses (cdr exp)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied")
      (error "Too few arguments supplied"))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vals) (env-loop (cdr env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env '())
      ; XXX: this is a problem! cause it could also be that it's a definition
      ; we can't know in advance, can we?
      (if (hash-has-key? *ht* var) ;; global def exists
        (hash-ref *ht* var)
        (error "Unbound variable" var))

      (let ((frame (car env)))
        (scan (car frame) (cdr frame)))))
  (env-loop env))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (car exps) env))
        (else (eval (first exps) env)
              (eval-sequence (cdr exps) env))))

; What's the env supposed to do here?
; What am I missing out on with define-variable?
; is var quoted? val lambda? 
(define (define-variable! var val env)
    (hash-set! *ht* var val))

; Adds it to the global namespace.
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (car exps) env)
          (list-of-values (cdr exps) env))))

(define (apply-primitive-procedure proc args)
  (apply-builtin (cadr proc) args))

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

; this is a good start example! instead of define var and that
(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              '())))
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ";;; Eval input:")
(define output-prompt ";;; Eval input:")
(define (prompt-for-input string) (newline) (newline) (display string) (newline))
(define (announce-output string) (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (cadr object)
                   (caddr object)
                   '<procedure-env>))
    (display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
