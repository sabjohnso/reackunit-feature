#lang racket/base
(require rackunit rackunit/text-ui racket/list)
(require rackunit/feature/private/lexer)

(define (tokenize-string str)
  (tokenize (open-input-string str) "test.feature"))

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
      (check-equal? (token-value (car tokens)) "not this"))

    (test-case "Background keyword"
      (define tokens (tokenize-string "  Background:"))
      (check-equal? (length tokens) 1)
      (check-equal? (token-type (car tokens)) 'background)
      (check-equal? (token-value (car tokens)) ""))

    (test-case "Background keyword with trailing text"
      (define tokens (tokenize-string "  Background: setup"))
      (check-equal? (token-type (car tokens)) 'background)
      (check-equal? (token-value (car tokens)) "setup")))

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
      (check-equal? (token-column (first tokens)) 0)
      (check-equal? (token-line (second tokens)) 2)
      (check-equal? (token-column (second tokens)) 2)
      (check-equal? (token-line (third tokens)) 3)
      (check-equal? (token-column (third tokens)) 4))

    (test-case "tokens carry source name"
      (define tokens (tokenize-string "Feature: F"))
      (check-equal? (token-source (car tokens)) "test.feature")))

   (test-suite
    "table rows"
    (test-case "pipe-delimited line produces table-row token"
      (define tokens (tokenize-string "      | name | age |"))
      (check-equal? (length tokens) 1)
      (check-equal? (token-type (car tokens)) 'table-row)
      (check-equal? (token-value (car tokens)) '("name" "age")))

    (test-case "table row cells are trimmed"
      (define tokens (tokenize-string "      |  Alice  |  30  |"))
      (check-equal? (token-value (car tokens)) '("Alice" "30")))

    (test-case "table row with single cell"
      (define tokens (tokenize-string "      | solo |"))
      (check-equal? (token-value (car tokens)) '("solo")))

    (test-case "multiple table rows produce multiple tokens"
      (define tokens (tokenize-string
                      (string-append "      | a | b |\n"
                                     "      | 1 | 2 |\n")))
      (check-equal? (length tokens) 2)
      (check-equal? (map token-type tokens) '(table-row table-row))
      (check-equal? (token-value (second tokens)) '("1" "2"))))

   (test-suite
    "scenario outline keywords"
    (test-case "Scenario Outline keyword"
      (define tokens (tokenize-string "  Scenario Outline: Adding numbers"))
      (check-equal? (length tokens) 1)
      (check-equal? (token-type (car tokens)) 'scenario-outline)
      (check-equal? (token-value (car tokens)) "Adding numbers"))

    (test-case "Examples keyword"
      (define tokens (tokenize-string "    Examples:"))
      (check-equal? (length tokens) 1)
      (check-equal? (token-type (car tokens)) 'examples)
      (check-equal? (token-value (car tokens)) ""))

    (test-case "Scenario Outline before Scenario in matching"
      (define tokens (tokenize-string
                      (string-append "  Scenario Outline: outline\n"
                                     "  Scenario: regular\n")))
      (check-equal? (map token-type tokens)
                    '(scenario-outline scenario))))

   (test-suite
    "tags"
    (test-case "single tag produces tags token"
      (define tokens (tokenize-string "  @smoke"))
      (check-equal? (length tokens) 1)
      (check-equal? (token-type (car tokens)) 'tags)
      (check-equal? (token-value (car tokens)) '("@smoke")))

    (test-case "multiple tags on one line"
      (define tokens (tokenize-string "  @smoke @fast @wip"))
      (check-equal? (token-value (car tokens)) '("@smoke" "@fast" "@wip")))

    (test-case "tags line before feature keyword"
      (define tokens (tokenize-string
                      (string-append "@smoke\n"
                                     "Feature: Calculator\n")))
      (check-equal? (length tokens) 2)
      (check-equal? (token-type (first tokens)) 'tags)
      (check-equal? (token-type (second tokens)) 'feature)))

   (test-suite
    "doc strings"
    (test-case "triple-quoted block produces doc-string token"
      (define tokens (tokenize-string
                      (string-append "    Given a document:\n"
                                     "      \"\"\"\n"
                                     "      Hello world\n"
                                     "      \"\"\"\n")))
      (check-equal? (length tokens) 2)
      (check-equal? (token-type (second tokens)) 'doc-string)
      (check-equal? (token-value (second tokens)) "Hello world"))

    (test-case "multi-line doc string preserves lines"
      (define tokens (tokenize-string
                      (string-append "    Given some text:\n"
                                     "      \"\"\"\n"
                                     "      line one\n"
                                     "      line two\n"
                                     "      line three\n"
                                     "      \"\"\"\n")))
      (check-equal? (token-value (second tokens))
                    "line one\nline two\nline three"))

    (test-case "doc string strips leading indent based on delimiter"
      (define tokens (tokenize-string
                      (string-append "    Given indented:\n"
                                     "    \"\"\"\n"
                                     "    first\n"
                                     "      indented\n"
                                     "    \"\"\"\n")))
      (check-equal? (token-value (second tokens))
                    "first\n  indented"))

    (test-case "doc string with empty lines"
      (define tokens (tokenize-string
                      (string-append "    Given text:\n"
                                     "      \"\"\"\n"
                                     "      hello\n"
                                     "\n"
                                     "      world\n"
                                     "      \"\"\"\n")))
      (check-equal? (token-value (second tokens))
                    "hello\n\nworld")))

   (test-suite
    "multi-line document"
    (test-case "full feature tokenizes correctly (no Steps directive)"
      (define tokens (tokenize-string
                      (string-append "Feature: Calculator\n"
                                     "  Scenario: Addition\n"
                                     "    Given a calculator\n"
                                     "    When I add 2 and 3\n"
                                     "    Then the result is 5\n")))
      (check-equal? (length tokens) 5)
      (check-equal? (map token-type tokens)
                    '(feature scenario given when then))))))

(run-tests lexer-tests)
