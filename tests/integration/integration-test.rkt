#lang racket/base
(require rackunit rackunit/text-ui)
(require rackunit-feature/runtime)
(require "calculator-steps.rkt")

(define integration-tests
  (test-suite
   "Integration: step definitions work with runtime"

   (test-case "calculator steps run end-to-end"
     (define ctx0 (hash))
     (define ctx1 (run-step calculator-steps 'given "a calculator" ctx0))
     (check-equal? (hash-ref ctx1 'calc) 'ready)
     (define ctx2 (run-step calculator-steps 'when "I add 2 and 3" ctx1))
     (check-equal? (hash-ref ctx2 'result) 5)
     (define ctx3 (run-step calculator-steps 'then "the result is 5" ctx2))
     (check-equal? (hash-ref ctx3 'result) 5))

   (test-case "wrong result triggers rackunit failure"
     (define ctx (hash 'result 42))
     (check-exn exn:test:check?
                (lambda ()
                  (run-step calculator-steps 'then "the result is 5" ctx))))))

(run-tests integration-tests)
