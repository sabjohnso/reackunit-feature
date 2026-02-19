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
    "background section"
    (test-case "feature with background stores steps in background field"
      (define doc (parse-string
                   (string-append "Feature: Calculator\n"
                                  "  Background:\n"
                                  "    Given a calculator\n"
                                  "    And the display is cleared\n"
                                  "  Scenario: Add\n"
                                  "    When I add 2 and 3\n"
                                  "    Then the result is 5\n")))
      (define f (car (gherkin-document-features doc)))
      (check-equal? (length (gherkin-feature-background f)) 2)
      (check-equal? (gherkin-step-type (first (gherkin-feature-background f))) 'given)
      (check-equal? (gherkin-step-text (first (gherkin-feature-background f))) "a calculator")
      (check-equal? (gherkin-step-type (second (gherkin-feature-background f))) 'given)
      (check-equal? (gherkin-step-text (second (gherkin-feature-background f))) "the display is cleared"))

    (test-case "feature without background has empty background list"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given x\n")))
      (define f (car (gherkin-document-features doc)))
      (check-equal? (gherkin-feature-background f) '()))

    (test-case "background steps are not duplicated into scenario steps"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Background:\n"
                                  "    Given shared setup\n"
                                  "  Scenario: S\n"
                                  "    When I do something\n")))
      (define f (car (gherkin-document-features doc)))
      (define sc-steps (gherkin-scenario-steps (car (gherkin-feature-scenarios f))))
      (check-equal? (length sc-steps) 1)
      (check-equal? (gherkin-step-text (car sc-steps)) "I do something"))

    (test-case "And/But resolution works in background steps"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Background:\n"
                                  "    Given a calculator\n"
                                  "    And the display is cleared\n"
                                  "    But not the memory\n"
                                  "  Scenario: S\n"
                                  "    When I press add\n")))
      (define bg (gherkin-feature-background
                  (car (gherkin-document-features doc))))
      (check-equal? (map gherkin-step-type bg) '(given given given))))

   (test-suite
    "data tables"
    (test-case "table rows attach to preceding step as argument"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given the following users:\n"
                                  "      | name  | age |\n"
                                  "      | Alice | 30  |\n"
                                  "      | Bob   | 25  |\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (length steps) 1)
      (check-equal? (gherkin-step-argument (car steps))
                    '(("name" "age") ("Alice" "30") ("Bob" "25"))))

    (test-case "step without table has #f argument"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given a calculator\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-false (gherkin-step-argument (car steps))))

    (test-case "table attaches to correct step when multiple steps exist"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given a calculator\n"
                                  "    When I enter values:\n"
                                  "      | a | b |\n"
                                  "      | 2 | 3 |\n"
                                  "    Then the result is 5\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (length steps) 3)
      (check-false (gherkin-step-argument (first steps)))
      (check-equal? (gherkin-step-argument (second steps))
                    '(("a" "b") ("2" "3")))
      (check-false (gherkin-step-argument (third steps)))))

   (test-suite
    "doc strings"
    (test-case "doc string attaches to preceding step"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given a document:\n"
                                  "      \"\"\"\n"
                                  "      Hello world\n"
                                  "      \"\"\"\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (length steps) 1)
      (check-equal? (gherkin-step-argument (car steps)) "Hello world"))

    (test-case "multi-line doc string preserved"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given text:\n"
                                  "      \"\"\"\n"
                                  "      line one\n"
                                  "      line two\n"
                                  "      \"\"\"\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (gherkin-step-argument (car steps))
                    "line one\nline two")))

   (test-suite
    "source locations"
    (test-case "feature carries source location"
      (define doc (parse-string "Feature: F\n  Scenario: S\n    Given x\n"))
      (define f (car (gherkin-document-features doc)))
      (define loc (gherkin-feature-srcloc f))
      (check-equal? (list-ref loc 0) "test")
      (check-equal? (list-ref loc 1) 1)))

   (test-suite
    "scenario outline"
    (test-case "outline expands into multiple scenarios"
      (define doc (parse-string
                   (string-append "Feature: Calculator\n"
                                  "  Scenario Outline: Adding\n"
                                  "    Given a calculator\n"
                                  "    When I add <a> and <b>\n"
                                  "    Then the result is <result>\n"
                                  "    Examples:\n"
                                  "      | a | b | result |\n"
                                  "      | 1 | 2 | 3      |\n"
                                  "      | 4 | 5 | 9      |\n")))
      (define scenarios (gherkin-feature-scenarios
                         (car (gherkin-document-features doc))))
      (check-equal? (length scenarios) 2))

    (test-case "expanded scenario names include parameter values"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario Outline: Adding\n"
                                  "    Given a calculator\n"
                                  "    When I add <a> and <b>\n"
                                  "    Then the result is <result>\n"
                                  "    Examples:\n"
                                  "      | a | b | result |\n"
                                  "      | 1 | 2 | 3      |\n"
                                  "      | 4 | 5 | 9      |\n")))
      (define scenarios (gherkin-feature-scenarios
                         (car (gherkin-document-features doc))))
      (check-equal? (gherkin-scenario-name (first scenarios))
                    "Adding (a=1, b=2, result=3)")
      (check-equal? (gherkin-scenario-name (second scenarios))
                    "Adding (a=4, b=5, result=9)"))

    (test-case "placeholders are substituted in step text"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario Outline: Adding\n"
                                  "    When I add <a> and <b>\n"
                                  "    Examples:\n"
                                  "      | a | b |\n"
                                  "      | 1 | 2 |\n")))
      (define steps (gherkin-scenario-steps
                     (car (gherkin-feature-scenarios
                           (car (gherkin-document-features doc))))))
      (check-equal? (gherkin-step-text (car steps)) "I add 1 and 2"))

    (test-case "outline with tags on the outline"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  @outline-tag\n"
                                  "  Scenario Outline: Adding\n"
                                  "    When I add <a> and <b>\n"
                                  "    Examples:\n"
                                  "      | a | b |\n"
                                  "      | 1 | 2 |\n")))
      (define sc (car (gherkin-feature-scenarios
                       (car (gherkin-document-features doc)))))
      (check-equal? (gherkin-scenario-tags sc) '("@outline-tag")))

    (test-case "outline mixed with regular scenario"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: Regular\n"
                                  "    Given x\n"
                                  "  Scenario Outline: Template\n"
                                  "    When I do <action>\n"
                                  "    Examples:\n"
                                  "      | action |\n"
                                  "      | run    |\n"
                                  "      | jump   |\n")))
      (define scenarios (gherkin-feature-scenarios
                         (car (gherkin-document-features doc))))
      (check-equal? (length scenarios) 3)
      (check-equal? (gherkin-scenario-name (first scenarios)) "Regular")
      (check-equal? (gherkin-scenario-name (second scenarios))
                    "Template (action=run)")
      (check-equal? (gherkin-scenario-name (third scenarios))
                    "Template (action=jump)")))

   (test-suite
    "tags"
    (test-case "tags before feature are stored on feature"
      (define doc (parse-string
                   (string-append "@smoke @wip\n"
                                  "Feature: Calculator\n"
                                  "  Scenario: Add\n"
                                  "    Given a calculator\n")))
      (define f (car (gherkin-document-features doc)))
      (check-equal? (gherkin-feature-tags f) '("@smoke" "@wip")))

    (test-case "tags before scenario are stored on scenario"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  @fast\n"
                                  "  Scenario: S\n"
                                  "    Given x\n")))
      (define sc (car (gherkin-feature-scenarios
                       (car (gherkin-document-features doc)))))
      (check-equal? (gherkin-scenario-tags sc) '("@fast")))

    (test-case "feature and scenario both have tags"
      (define doc (parse-string
                   (string-append "@feature-tag\n"
                                  "Feature: F\n"
                                  "  @scenario-tag\n"
                                  "  Scenario: S\n"
                                  "    Given x\n")))
      (define f (car (gherkin-document-features doc)))
      (check-equal? (gherkin-feature-tags f) '("@feature-tag"))
      (define sc (car (gherkin-feature-scenarios f)))
      (check-equal? (gherkin-scenario-tags sc) '("@scenario-tag")))

    (test-case "feature without tags has empty tags"
      (define doc (parse-string
                   (string-append "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given x\n")))
      (define f (car (gherkin-document-features doc)))
      (check-equal? (gherkin-feature-tags f) '()))

    (test-case "multiple tag lines are merged"
      (define doc (parse-string
                   (string-append "@smoke\n"
                                  "@fast\n"
                                  "Feature: F\n"
                                  "  Scenario: S\n"
                                  "    Given x\n")))
      (define f (car (gherkin-document-features doc)))
      (check-equal? (gherkin-feature-tags f) '("@smoke" "@fast"))))

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
