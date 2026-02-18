#lang racket/base
(require rackunit rackunit/text-ui racket/list racket/string)
(require rackunit-feature/private/ast)
(require rackunit-feature/private/compiler)

(define compiler-tests
  (test-suite
   "Compiler"

   (test-suite
    "compile-document"
    (test-case "produces a syntax object"
      (define doc
        (gherkin-document
         #f '("steps.rkt")
         (list (gherkin-feature
                #f "Calculator"
                (list (gherkin-scenario
                       #f "Addition"
                       (list (gherkin-step #f 'given "a calculator"))))))))
      (define result (compile-document doc))
      (check-pred syntax? result))

    (test-case "compiled form is a begin with require and run-tests"
      (define doc
        (gherkin-document
         #f '("steps.rkt")
         (list (gherkin-feature
                #f "Calculator"
                (list (gherkin-scenario
                       #f "Addition"
                       (list (gherkin-step #f 'given "a calculator"))))))))
      (define result (compile-document doc))
      (define as-datum (syntax->datum result))
      ;; Top level is a begin
      (check-equal? (car as-datum) 'begin)
      ;; Contains require forms and run-tests
      (define forms (cdr as-datum))
      (check-true (pair? forms)))

    (test-case "feature name appears in test-suite"
      (define doc
        (gherkin-document
         #f '("steps.rkt")
         (list (gherkin-feature
                #f "Calculator"
                (list (gherkin-scenario
                       #f "Addition"
                       (list (gherkin-step #f 'given "a calculator"))))))))
      (define as-datum (syntax->datum (compile-document doc)))
      (define as-str (format "~s" as-datum))
      (check-true (string-contains? as-str "Feature: Calculator")))

    (test-case "scenario name appears in test-case"
      (define doc
        (gherkin-document
         #f '("steps.rkt")
         (list (gherkin-feature
                #f "Calculator"
                (list (gherkin-scenario
                       #f "Addition"
                       (list (gherkin-step #f 'given "a calculator"))))))))
      (define as-datum (syntax->datum (compile-document doc)))
      (define as-str (format "~s" as-datum))
      (check-true (string-contains? as-str "Scenario: Addition")))

    (test-case "step types and texts appear in run-step calls"
      (define doc
        (gherkin-document
         #f '("steps.rkt")
         (list (gherkin-feature
                #f "F"
                (list (gherkin-scenario
                       #f "S"
                       (list (gherkin-step #f 'given "a calculator")
                             (gherkin-step #f 'when "I add 2 and 3")
                             (gherkin-step #f 'then "the result is 5"))))))))
      (define as-datum (syntax->datum (compile-document doc)))
      (define as-str (format "~s" as-datum))
      (check-true (string-contains? as-str "a calculator"))
      (check-true (string-contains? as-str "I add 2 and 3"))
      (check-true (string-contains? as-str "the result is 5"))
      (check-true (string-contains? as-str "(quote given)"))
      (check-true (string-contains? as-str "(quote when)"))
      (check-true (string-contains? as-str "(quote then)")))

    (test-case "multiple step files produce multiple prefix-in requires"
      (define doc
        (gherkin-document
         #f '("a.rkt" "b.rkt")
         (list (gherkin-feature
                #f "F"
                (list (gherkin-scenario
                       #f "S"
                       (list (gherkin-step #f 'given "x"))))))))
      (define as-datum (syntax->datum (compile-document doc)))
      (define as-str (format "~s" as-datum))
      (check-true (string-contains? as-str "a.rkt"))
      (check-true (string-contains? as-str "b.rkt"))))))

(run-tests compiler-tests)
