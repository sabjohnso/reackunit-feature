#lang racket/base
(require rackunit rackunit/text-ui racket/list)
(require rackunit/feature/private/ast)
(require rackunit/feature/private/lexer)
(require rackunit/feature/private/parser)

(define (parse-string str)
  (parse (tokenize (open-input-string str) "test")))

(define parser-tests
  (test-suite
   "Parser"

   (test-suite
    "minimal feature"
    (test-case "single scenario with one step"
      (define doc (parse-string
                   (string-append "Feature: Calculator\n"
                                  "  Scenario: Add\n"
                                  "    Given a calculator\n")))
      (check-pred gherkin-document? doc)
      (define features (gherkin-document-features doc))
      (check-equal? (length features) 1)
      (check-equal? (gherkin-feature-name (car features)) "Calculator")
      (define scenarios (gherkin-feature-scenarios (car features)))
      (check-equal? (length scenarios) 1)
      (check-equal? (gherkin-scenario-name (car scenarios)) "Add")
      (define steps (gherkin-scenario-steps (car scenarios)))
      (check-equal? (length steps) 1)
      (check-equal? (gherkin-step-type (car steps)) 'given)
      (check-equal? (gherkin-step-text (car steps)) "a calculator")))

   (test-suite
    "And/But resolution"
    (test-case "And inherits type from preceding step"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given a calculator\n"
                                  "    And a display\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (length steps) 2)
      (check-equal? (gherkin-step-type (second steps)) 'given)
      (check-equal? (gherkin-step-text (second steps)) "a display"))

    (test-case "But inherits type from preceding step"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Then the result is 5\n"
                                  "    But the display is not empty\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (gherkin-step-type (second steps)) 'then))

    (test-case "And after When becomes When"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    When I press add\n"
                                  "    And I press equals\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (gherkin-step-type (second steps)) 'when)))

   (test-suite
    "multiple scenarios"
    (test-case "two scenarios in one feature"
      (define doc (parse-string
                   (string-append "Feature: Calculator\n"
                                  "  Scenario: Add\n"
                                  "    Given a calculator\n"
                                  "  Scenario: Subtract\n"
                                  "    Given a calculator\n")))
      (define scenarios (gherkin-feature-scenarios
                         (car (gherkin-document-features doc))))
      (check-equal? (length scenarios) 2)
      (check-equal? (gherkin-scenario-name (first scenarios)) "Add")
      (check-equal? (gherkin-scenario-name (second scenarios)) "Subtract")))

   (test-suite
    "source locations"
    (test-case "feature carries source location"
      (define doc (parse-string "Feature: F\n  Scenario: S\n    Given x\n"))
      (define f (car (gherkin-document-features doc)))
      (define loc (gherkin-feature-srcloc f))
      (check-equal? (list-ref loc 0) "test")
      (check-equal? (list-ref loc 1) 1)))

   (test-suite
    "full feature"
    (test-case "complete feature with given/when/then"
      (define doc (parse-string
                   (string-append "Feature: Calculator\n"
                                  "  Scenario: Addition\n"
                                  "    Given a calculator\n"
                                  "    When I add 2 and 3\n"
                                  "    Then the result is 5\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (map gherkin-step-type steps) '(given when then))
      (check-equal? (map gherkin-step-text steps)
                    '("a calculator" "I add 2 and 3" "the result is 5"))))))

(run-tests parser-tests)
