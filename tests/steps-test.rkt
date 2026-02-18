#lang racket/base
(require rackunit rackunit/text-ui racket/list)
(require rackunit-feature/runtime)
(require rackunit-feature/steps)

(define-steps my-steps
  (given "a calculator"
    (lambda (ctx) (hash-set ctx 'calc 'ready)))
  (when "I add {a} and {b}"
    (lambda (ctx a b)
      (hash-set ctx 'result (+ (string->number a) (string->number b)))))
  (then "the result is {n}"
    (lambda (ctx n)
      (check-equal? (hash-ref ctx 'result) (string->number n))
      ctx)))

(define steps-tests
  (test-suite
   "Step definition API"

   (test-case "define-steps produces a list of step-defs"
     (check-pred list? my-steps)
     (check-equal? (length my-steps) 3))

   (test-case "each element is a step-def"
     (for ([sd (in-list my-steps)])
       (check-pred step-def? sd)))

   (test-case "step types are correct"
     (check-equal? (map step-def-type my-steps)
                   '(given when then)))

   (test-case "step patterns are correct"
     (check-equal? (map step-def-pattern my-steps)
                   '("a calculator" "I add {a} and {b}" "the result is {n}")))

   (test-case "steps work with run-step"
     (define ctx0 (hash))
     (define ctx1 (run-step my-steps 'given "a calculator" ctx0))
     (define ctx2 (run-step my-steps 'when "I add 2 and 3" ctx1))
     (define ctx3 (run-step my-steps 'then "the result is 5" ctx2))
     (check-equal? (hash-ref ctx3 'result) 5))

   (test-case "define-steps also binds feature-steps"
     (check-equal? feature-steps my-steps))))

(run-tests steps-tests)
