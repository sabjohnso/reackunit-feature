#lang racket/base
(require rackunit rackunit/text-ui rackunit/feature)
(require "calculator-steps.rkt")
(require "tags.feature")

(define tags-tests
  (test-suite
   "integration: tags"

   (test-case "feature has tags from .feature file"
     (check-equal? (gherkin-feature-tags (car features)) '("@smoke")))

   (test-case "scenario has tags from .feature file"
     (define sc (car (gherkin-feature-scenarios (car features))))
     (check-equal? (gherkin-scenario-tags sc) '("@fast")))

   (test-case "tagged feature runs successfully"
     (check-not-exn
      (lambda ()
        (run-features features #:steps calculator-steps))))))

(run-tests tags-tests)
