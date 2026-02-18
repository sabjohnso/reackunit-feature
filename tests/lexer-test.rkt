#lang racket/base
(require rackunit rackunit/text-ui racket/list)
(require rackunit-feature/private/lexer)

(define (tokenize-string str)
  (tokenize (open-input-string str) "test.feature.rkt"))

(define lexer-tests
  (test-suite
   "Lexer"

   (test-suite
    "keyword recognition"
    (test-case "Feature keyword"
      (define tokens (tokenize-string "Feature: Calculator"))
      (check-equal? (length tokens) 1)
      (check-equal? (token-type (car tokens)) 'feature)
      (check-equal? (token-value (car tokens)) "Calculator"))

    (test-case "Scenario keyword"
      (define tokens (tokenize-string "  Scenario: Addition"))
      (check-equal? (token-type (car tokens)) 'scenario)
      (check-equal? (token-value (car tokens)) "Addition"))

    (test-case "Given keyword"
      (define tokens (tokenize-string "    Given a calculator"))
      (check-equal? (token-type (car tokens)) 'given)
      (check-equal? (token-value (car tokens)) "a calculator"))

    (test-case "When keyword"
      (define tokens (tokenize-string "    When I add 2 and 3"))
      (check-equal? (token-type (car tokens)) 'when)
      (check-equal? (token-value (car tokens)) "I add 2 and 3"))

    (test-case "Then keyword"
      (define tokens (tokenize-string "    Then the result is 5"))
      (check-equal? (token-type (car tokens)) 'then)
      (check-equal? (token-value (car tokens)) "the result is 5"))

    (test-case "And keyword"
      (define tokens (tokenize-string "    And another thing"))
      (check-equal? (token-type (car tokens)) 'and)
      (check-equal? (token-value (car tokens)) "another thing"))

    (test-case "But keyword"
      (define tokens (tokenize-string "    But not this"))
      (check-equal? (token-type (car tokens)) 'but)
      (check-equal? (token-value (car tokens)) "not this")))

   (test-suite
    "Steps directive"
    (test-case "Steps directive with quoted path"
      (define tokens (tokenize-string "Steps: \"calculator-steps.rkt\""))
      (check-equal? (token-type (car tokens)) 'steps)
      (check-equal? (token-value (car tokens)) "calculator-steps.rkt"))

    (test-case "multiple Steps directives"
      (define tokens (tokenize-string
                      (string-append "Steps: \"a.rkt\"\n"
                                     "Steps: \"b.rkt\"\n")))
      (check-equal? (length tokens) 2)
      (check-equal? (token-value (first tokens)) "a.rkt")
      (check-equal? (token-value (second tokens)) "b.rkt")))

   (test-suite
    "blanks and comments"
    (test-case "blank lines are skipped"
      (define tokens (tokenize-string "\n\n  \n"))
      (check-equal? tokens '()))

    (test-case "comment lines are skipped"
      (define tokens (tokenize-string "# this is a comment"))
      (check-equal? tokens '())))

   (test-suite
    "source locations"
    (test-case "tokens carry line and column info"
      (define tokens (tokenize-string
                      (string-append "Feature: F\n"
                                     "  Scenario: S\n"
                                     "    Given x\n")))
      (check-equal? (token-line (first tokens)) 1)
      (check-equal? (token-line (second tokens)) 2)
      (check-equal? (token-line (third tokens)) 3))

    (test-case "tokens carry source name"
      (define tokens (tokenize-string "Feature: F"))
      (check-equal? (token-source (car tokens)) "test.feature.rkt")))

   (test-suite
    "multi-line document"
    (test-case "full feature tokenizes correctly"
      (define tokens (tokenize-string
                      (string-append "Steps: \"steps.rkt\"\n"
                                     "\n"
                                     "Feature: Calculator\n"
                                     "  Scenario: Addition\n"
                                     "    Given a calculator\n"
                                     "    When I add 2 and 3\n"
                                     "    Then the result is 5\n")))
      (check-equal? (length tokens) 6)
      (check-equal? (map token-type tokens)
                    '(steps feature scenario given when then))))))

(run-tests lexer-tests)
