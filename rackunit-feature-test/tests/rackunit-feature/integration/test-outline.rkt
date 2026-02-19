#lang racket/base
(require rackunit rackunit/text-ui racket/string rackunit/feature)
(require "calculator-steps.rkt")
(require "outline.feature")

(define scenario-count (box 0))
(define results (box '()))

(run-features features
  #:steps calculator-steps
  #:after-scenario (lambda (ctx sc)
                     (set-box! scenario-count (add1 (unbox scenario-count)))
                     (set-box! results
                               (cons (hash-ref ctx 'result #f)
                                     (unbox results)))
                     ctx))

(define outline-tests
  (test-suite
   "integration: scenario outlines"

   (test-case "outline expands into 2 scenarios"
     (check-equal? (length (gherkin-feature-scenarios (car features))) 2))

   (test-case "expanded scenarios have parameter descriptions in names"
     (define names
       (map gherkin-scenario-name (gherkin-feature-scenarios (car features))))
     (check-true (andmap (lambda (n) (string-contains? n "(")) names)))

   (test-case "both scenarios ran"
     (check-equal? (unbox scenario-count) 2))

   (test-case "step values were substituted correctly"
     (check-equal? (sort (unbox results) <) '(3 9)))))

(run-tests outline-tests)
