#lang racket/base
(require rackunit rackunit/feature)
(provide calculator-steps)

(define-steps calculator-steps
  (given "a calculator"
    (lambda (ctx) (hash-set ctx 'calc 'ready)))
  (when "I add {a} and {b}"
    (lambda (ctx a b)
      (hash-set ctx 'result (+ (string->number a) (string->number b)))))
  (when "I subtract {a} from {b}"
    (lambda (ctx a b)
      (hash-set ctx 'result (- (string->number b) (string->number a)))))
  (then "the result is {n}"
    (lambda (ctx n)
      (check-equal? (hash-ref ctx 'result) (string->number n))
      ctx)))
