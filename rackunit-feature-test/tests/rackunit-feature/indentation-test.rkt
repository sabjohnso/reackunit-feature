#lang racket/base
(require rackunit rackunit/text-ui racket/vector)
(require feature/lang/indentation)

;; Helper: build a get-line function from a vector of strings.
;; (make-get-line lines) returns a procedure (n → string?) where
;; n is a 0-based line number.
(define (make-get-line lines)
  (lambda (n)
    (if (< n (vector-length lines))
        (vector-ref lines n)
        #f)))

(define indentation-tests
  (test-suite
   "Gherkin Indentation"

   (test-suite
    "keyword-based indentation"

    (test-case "Feature: indents to 0"
      (define get-line (make-get-line #("Feature: Calculator")))
      (check-equal? (gherkin-indent-line get-line 0) 0))

    (test-case "Background: indents to 2"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Background:")))
      (check-equal? (gherkin-indent-line get-line 1) 2))

    (test-case "Scenario: indents to 2"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Addition")))
      (check-equal? (gherkin-indent-line get-line 1) 2))

    (test-case "Scenario Outline: indents to 2"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario Outline: Arithmetic")))
      (check-equal? (gherkin-indent-line get-line 1) 2))

    (test-case "Given indents to 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Addition"
                         "    Given a calculator")))
      (check-equal? (gherkin-indent-line get-line 2) 4))

    (test-case "When indents to 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Addition"
                         "    When I add 2 and 3")))
      (check-equal? (gherkin-indent-line get-line 2) 4))

    (test-case "Then indents to 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Addition"
                         "    Then the result is 5")))
      (check-equal? (gherkin-indent-line get-line 2) 4))

    (test-case "And indents to 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Addition"
                         "    Given a calculator"
                         "    And another thing")))
      (check-equal? (gherkin-indent-line get-line 3) 4))

    (test-case "But indents to 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Addition"
                         "    Given a calculator"
                         "    But not this")))
      (check-equal? (gherkin-indent-line get-line 3) 4))

    (test-case "Examples: indents to 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario Outline: Arithmetic"
                         "    Given a calculator"
                         "    Examples:")))
      (check-equal? (gherkin-indent-line get-line 3) 4))

    (test-case "table pipe indents to 6"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario Outline: Arithmetic"
                         "    Examples:"
                         "      | a | b |")))
      (check-equal? (gherkin-indent-line get-line 3) 6))

    (test-case "doc string delimiter indents to 6"
      (define get-line
        (make-get-line #("Feature: Doc"
                         "  Scenario: Test"
                         "    Given a document:"
                         "      \"\"\"")))
      (check-equal? (gherkin-indent-line get-line 3) 6)))

   (test-suite
    "tag indentation"

    (test-case "tag before Feature: indents to 0"
      (define get-line
        (make-get-line #("@smoke"
                         "Feature: Calculator")))
      (check-equal? (gherkin-indent-line get-line 0) 0))

    (test-case "tag after Feature: indents to 2"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  @fast"
                         "  Scenario: Addition")))
      (check-equal? (gherkin-indent-line get-line 1) 2))

    (test-case "multiple tags before Feature: indent to 0"
      (define get-line
        (make-get-line #("@smoke"
                         "@wip"
                         "Feature: Calculator")))
      (check-equal? (gherkin-indent-line get-line 0) 0)
      (check-equal? (gherkin-indent-line get-line 1) 0)))

   (test-suite
    "context-based indentation (blank lines and comments)"

    (test-case "blank line after Feature: indents to 2"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "")))
      (check-equal? (gherkin-indent-line get-line 1) 2))

    (test-case "blank line after Scenario: indents to 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Addition"
                         "")))
      (check-equal? (gherkin-indent-line get-line 2) 4))

    (test-case "blank line after step indents to 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Addition"
                         "    Given a calculator"
                         "")))
      (check-equal? (gherkin-indent-line get-line 3) 4))

    (test-case "blank line after Examples: indents to 6"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario Outline: Arithmetic"
                         "    Examples:"
                         "")))
      (check-equal? (gherkin-indent-line get-line 3) 6))

    (test-case "blank line after table row indents to 6"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario Outline: Arithmetic"
                         "    Examples:"
                         "      | a | b |"
                         "")))
      (check-equal? (gherkin-indent-line get-line 4) 6))

    (test-case "blank line after closing doc string indents to 4"
      (define get-line
        (make-get-line #("Feature: Doc"
                         "  Scenario: Test"
                         "    Given a document:"
                         "      \"\"\""
                         "      content"
                         "      \"\"\""
                         "")))
      (check-equal? (gherkin-indent-line get-line 6) 4))

    (test-case "blank line after tag inherits tag indent"
      (define get-line
        (make-get-line #("@smoke"
                         ""
                         "Feature: Calculator")))
      (check-equal? (gherkin-indent-line get-line 1) 0))

    (test-case "comment inherits context from previous line"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  # This is a comment")))
      (check-equal? (gherkin-indent-line get-line 1) 2))

    (test-case "comment after Scenario: inherits 4"
      (define get-line
        (make-get-line #("Feature: Calculator"
                         "  Scenario: Test"
                         "    # step comment")))
      (check-equal? (gherkin-indent-line get-line 2) 4)))

   (test-suite
    "doc string content indentation"

    (test-case "text inside doc string indents to 6"
      (define get-line
        (make-get-line #("Feature: Doc"
                         "  Scenario: Test"
                         "    Given a document:"
                         "      \"\"\""
                         "      some content")))
      (check-equal? (gherkin-indent-line get-line 4) 6))

    (test-case "blank line inside doc string indents to 6"
      (define get-line
        (make-get-line #("Feature: Doc"
                         "  Scenario: Test"
                         "    Given a document:"
                         "      \"\"\""
                         ""
                         "      more content"
                         "      \"\"\"")))
      (check-equal? (gherkin-indent-line get-line 4) 6))

    (test-case "closing doc string indents to 6"
      (define get-line
        (make-get-line #("Feature: Doc"
                         "  Scenario: Test"
                         "    Given a document:"
                         "      \"\"\""
                         "      content"
                         "      \"\"\"")))
      (check-equal? (gherkin-indent-line get-line 5) 6)))

   (test-suite
    "end-to-end feature file"

    (test-case "full feature file indentation"
      (define lines
        #("@smoke"
          "Feature: Tagged Calculator"
          "  @fast"
          "  Background:"
          "    Given a calculator"
          ""
          "  Scenario: Addition"
          "    When I add 2 and 3"
          "    Then the result is 5"
          ""
          "  Scenario Outline: Arithmetic"
          "    Given a calculator"
          "    When I add <a> and <b>"
          "    Then the result is <result>"
          "    Examples:"
          "      | a | b | result |"
          "      | 1 | 2 | 3      |"
          ""
          "  Scenario: Doc string"
          "    Given a document:"
          "      \"\"\""
          "      Hello world"
          "      \"\"\""
          "    Then the document should contain Hello world"))
      (define get-line (make-get-line lines))
      (define expected
        #(0    ; @smoke (before Feature)
          0    ; Feature:
          2    ; @fast (inside Feature)
          2    ; Background:
          4    ; Given
          4    ; blank after step
          2    ; Scenario:
          4    ; When
          4    ; Then
          4    ; blank after step (context: step → 4)
          2    ; Scenario Outline:
          4    ; Given
          4    ; When
          4    ; Then
          4    ; Examples:
          6    ; | table row
          6    ; | table row
          6    ; blank after table row
          2    ; Scenario:
          4    ; Given
          6    ; """
          6    ; doc string content
          6    ; """
          4    ; Then (after doc string block)
          ))
      (for ([i (in-range (vector-length lines))])
        (check-equal? (gherkin-indent-line get-line i)
                      (vector-ref expected i)
                      (format "line ~a: ~s" i (vector-ref lines i)))))

    (test-case "#lang feature line returns #f"
      (define get-line (make-get-line #("#lang feature"
                                        "Feature: X")))
      (check-equal? (gherkin-indent-line get-line 0) #f))

    (test-case "line 0 with no keyword returns #f"
      (define get-line (make-get-line #("")))
      (check-equal? (gherkin-indent-line get-line 0) #f)))))

(run-tests indentation-tests)
