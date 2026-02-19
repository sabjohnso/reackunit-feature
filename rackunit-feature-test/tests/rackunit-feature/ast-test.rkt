#lang racket/base
(require rackunit rackunit/text-ui)
(require rackunit/feature/private/ast)

(define ast-tests
  (test-suite
   "AST structs"

   (test-suite
    "gherkin-step"
    (test-case "constructs with srcloc, type, text, and argument"
      (define s (gherkin-step '(test 1 0 1 10) 'given "a calculator" #f))
      (check-equal? (gherkin-step-srcloc s) '(test 1 0 1 10))
      (check-equal? (gherkin-step-type s) 'given)
      (check-equal? (gherkin-step-text s) "a calculator"))

    (test-case "equal? works for prefab structs"
      (check-equal? (gherkin-step #f 'when "I add 2 and 3" #f)
                    (gherkin-step #f 'when "I add 2 and 3" #f)))

    (test-case "different steps are not equal"
      (check-not-equal? (gherkin-step #f 'given "a calculator" #f)
                        (gherkin-step #f 'then "the result is 5" #f)))

    (test-case "argument field defaults to #f"
      (define s (gherkin-step #f 'given "x" #f))
      (check-false (gherkin-step-argument s)))

    (test-case "argument field stores data table"
      (define table '(("name" "age") ("Alice" "30")))
      (define s (gherkin-step #f 'given "a table" table))
      (check-equal? (gherkin-step-argument s) table))

    (test-case "argument field stores doc string"
      (define s (gherkin-step #f 'given "a doc" "some text"))
      (check-equal? (gherkin-step-argument s) "some text")))

   (test-suite
    "gherkin-scenario"
    (test-case "constructs with srcloc, name, tags, and steps"
      (define steps (list (gherkin-step #f 'given "a calculator" #f)))
      (define sc (gherkin-scenario '(test 3 0 3 20) "Addition" '() steps))
      (check-equal? (gherkin-scenario-srcloc sc) '(test 3 0 3 20))
      (check-equal? (gherkin-scenario-name sc) "Addition")
      (check-equal? (gherkin-scenario-steps sc) steps))

    (test-case "equal? works for nested prefab structs"
      (define s1 (gherkin-scenario #f "S" '() (list (gherkin-step #f 'given "x" #f))))
      (define s2 (gherkin-scenario #f "S" '() (list (gherkin-step #f 'given "x" #f))))
      (check-equal? s1 s2))

    (test-case "tags field stores tag list"
      (define sc (gherkin-scenario #f "S" '("@smoke" "@fast") '()))
      (check-equal? (gherkin-scenario-tags sc) '("@smoke" "@fast")))

    (test-case "tags field defaults to empty"
      (define sc (gherkin-scenario #f "S" '() '()))
      (check-equal? (gherkin-scenario-tags sc) '())))

   (test-suite
    "gherkin-feature"
    (test-case "constructs with srcloc, name, tags, background, and scenarios"
      (define bg (list (gherkin-step #f 'given "a calculator" #f)))
      (define sc (gherkin-scenario #f "S" '() '()))
      (define f (gherkin-feature '(test 1 0 1 30) "Calculator" '() bg (list sc)))
      (check-equal? (gherkin-feature-srcloc f) '(test 1 0 1 30))
      (check-equal? (gherkin-feature-name f) "Calculator")
      (check-equal? (gherkin-feature-background f) bg)
      (check-equal? (gherkin-feature-scenarios f) (list sc)))

    (test-case "empty background when no Background section"
      (define f (gherkin-feature #f "F" '() '() '()))
      (check-equal? (gherkin-feature-background f) '()))

    (test-case "tags field on feature"
      (define f (gherkin-feature #f "F" '("@wip") '() '()))
      (check-equal? (gherkin-feature-tags f) '("@wip")))

    (test-case "tags field defaults to empty on feature"
      (define f (gherkin-feature #f "F" '() '() '()))
      (check-equal? (gherkin-feature-tags f) '())))

   (test-suite
    "gherkin-document"
    (test-case "constructs with srcloc and features (no step-files)"
      (define f (gherkin-feature #f "Calculator" '() '() '()))
      (define doc (gherkin-document '(test 1 0 1 0) (list f)))
      (check-equal? (gherkin-document-srcloc doc) '(test 1 0 1 0))
      (check-equal? (gherkin-document-features doc) (list f)))

    (test-case "equal? works for full document tree"
      (define doc1
        (gherkin-document
         #f
         (list (gherkin-feature
                #f "F" '() '()
                (list (gherkin-scenario
                       #f "S" '()
                       (list (gherkin-step #f 'given "x" #f))))))))
      (define doc2
        (gherkin-document
         #f
         (list (gherkin-feature
                #f "F" '() '()
                (list (gherkin-scenario
                       #f "S" '()
                       (list (gherkin-step #f 'given "x" #f))))))))
      (check-equal? doc1 doc2)))))

(run-tests ast-tests)
