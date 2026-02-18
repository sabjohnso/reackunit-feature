#lang racket/base
(require rackunit rackunit/text-ui racket/list)
(require rackunit/feature/private/ast)
(require "sample.feature")

(define reader-tests
  (test-suite
   "#lang feature reader"

   (test-case "features is a list"
     (check-pred list? features))

   (test-case "features contains one gherkin-feature"
     (check-equal? (length features) 1)
     (check-pred gherkin-feature? (car features)))

   (test-case "feature name is parsed"
     (check-equal? (gherkin-feature-name (car features)) "Calculator"))

   (test-case "scenario is parsed"
     (define scenarios (gherkin-feature-scenarios (car features)))
     (check-equal? (length scenarios) 1)
     (check-equal? (gherkin-scenario-name (car scenarios)) "Addition"))

   (test-case "feature without Background has empty background list"
     (check-equal? (gherkin-feature-background (car features)) '()))

   (test-case "steps are parsed with correct types"
     (define steps (gherkin-scenario-steps
                    (car (gherkin-feature-scenarios (car features)))))
     (check-equal? (length steps) 3)
     (check-equal? (map gherkin-step-type steps) '(given when then))
     (check-equal? (map gherkin-step-text steps)
                   '("a calculator" "I add 2 and 3" "the result is 5")))))

(run-tests reader-tests)
