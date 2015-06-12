#lang racket

; Based on SICP 4.1

(define (self-evaluating? exp) #t)
(define (variable? exp) #t)
(define (quoted? exp) #t)
(define (assignment? exp) #t)
(define (definition? exp) #t)
(define (lookup-variable-value exp env) #t)
(define (text-of-quotation exp) #t)
(define (eval-assignment exp env) #t)
(define (eval-definition exp env) #t)
(define (if? exp) #t)
(define (lambda? exp) #t)
(define (make-procedure foo bar env) #t)
(define (lambda-parameters exp) #t)
(define (lambda-body exp) #t)
(define (begin? exp) #t)
(define (begin-actions exp) #t)
(define (cond? exp) #t)
(define (application? exp) #t)
(define (operator exp) #t)
(define (operands exp) #t)
(define (cond->if foo) #t)
(define (primitive-procedure? procedure) #t)
(define (apply-primitive-procedure procedure arguments) #t)
(define (compound-procedure? procedure) #t)
(define (procedure-body procedure) #t)
(define (extend-environment foo bar quux) #t)
(define (procedure-parameters procedure) #t)
(define (procedure-environment procedure) #t)
(define (no-operands? exps) #t)
(define (first-operand exps) #t)
(define (rest-operands exps) #t)
(define (true? exp) exp)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (if-predicate exp) #t)
(define (if-consequent exp) #t)
(define (if-consequent exp) #t)

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence foo env) #t)