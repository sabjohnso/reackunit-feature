#lang racket/base
(require rackunit rackunit/text-ui)
(require rackunit/feature/private/ast)

(define ast-tests
  (test-suite
   "AST structs"

   (test-suite
    "gherkin-step"
    (test-case "constructs with srcloc, type, and text"
      (define s (gherkin-step '(test 1 0 1 10) 'given "a calculator"))
      (check-equal? (gherkin-step-srcloc s) '(test 1 0 1 10))
      (check-equal? (gherkin-step-type s) 'given)
      (check-equal? (gherkin-step-text s) "a calculator"))

    (test-case "equal? works for prefab structs"
      (check-equal? (gherkin-step #f 'when "I add 2 and 3")
                    (gherkin-step #f 'when "I add 2 and 3")))

    (test-case "different steps are not equal"
      (check-not-equal? (gherkin-step #f 'given "a calculator")
                        (gherkin-step #f 'then "the result is 5"))))

   (test-suite
    "gherkin-scenario"
    (test-case "constructs with srcloc, name, and steps"
      (define steps (list (gherkin-step #f 'given "a calculator")))
      (define sc (gherkin-scenario '(test 3 0 3 20) "Addition" steps))
      (check-equal? (gherkin-scenario-srcloc sc) '(test 3 0 3 20))
      (check-equal? (gherkin-scenario-name sc) "Addition")
      (check-equal? (gherkin-scenario-steps sc) steps))

    (test-case "equal? works for nested prefab structs"
      (define s1 (gherkin-scenario #f "S" (list (gherkin-step #f 'given "x"))))
      (define s2 (gherkin-scenario #f "S" (list (gherkin-step #f 'given "x"))))
      (check-equal? s1 s2)))

   (test-suite
    "gherkin-feature"
    (test-case "constructs with srcloc, name, and scenarios"
      (define sc (gherkin-scenario #f "S" '()))
      (define f (gherkin-feature '(test 1 0 1 30) "Calculator" (list sc)))
      (check-equal? (gherkin-feature-srcloc f) '(test 1 0 1 30))
      (check-equal? (gherkin-feature-name f) "Calculator")
      (check-equal? (gherkin-feature-scenarios f) (list sc))))

   (test-suite
    "gherkin-document"
    (test-case "constructs with srcloc and features (no step-files)"
      (define f (gherkin-feature #f "Calculator" '()))
      (define doc (gherkin-document '(test 1 0 1 0) (list f)))
      (check-equal? (gherkin-document-srcloc doc) '(test 1 0 1 0))
      (check-equal? (gherkin-document-features doc) (list f)))

    (test-case "equal? works for full document tree"
      (define doc1
        (gherkin-document
         #f
         (list (gherkin-feature
                #f "F"
                (list (gherkin-scenario
                       #f "S"
                       (list (gherkin-step #f 'given "x"))))))))
      (define doc2
        (gherkin-document
         #f
         (list (gherkin-feature
                #f "F"
                (list (gherkin-scenario
                       #f "S"
                       (list (gherkin-step #f 'given "x"))))))))
      (check-equal? doc1 doc2)))))

(run-tests ast-tests)
