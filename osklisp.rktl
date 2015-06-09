#lang racket

(define (read x) x)
(define (eval x) x)
(define (print x) (display x))

(define (rep x)
  (print (eval (read x))))

(define (repl)
  (display "> ")
  (rep (read-line))
  (repl))

(repl)
