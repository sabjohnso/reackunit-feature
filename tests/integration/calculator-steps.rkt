#lang racket/base
(require rackunit rackunit-feature/steps)
(provide calculator-steps feature-steps)

(define-steps calculator-steps
  (given "a calculator"
    (lambda (ctx) (hash-set ctx 'calc 'ready)))
  (when "I add {a} and {b}"
    (lambda (ctx a b)
      (hash-set ctx 'result (+ (string->number a) (string->number b)))))
  (then "the result is {n}"
    (lambda (ctx n)
      (check-equal? (hash-ref ctx 'result) (string->number n))
      ctx)))
