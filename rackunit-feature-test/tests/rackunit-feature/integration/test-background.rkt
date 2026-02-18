#lang racket/base
(require rackunit rackunit/text-ui rackunit/feature)
(require "calculator-steps.rkt")
(require (prefix-in bg: "background.feature"))

(define display-step
  (step-def 'given "the display is cleared"
            (lambda (ctx) (hash-set ctx 'display 'cleared))))

(define all-steps (append calculator-steps (list display-step)))

(define background-tests
  (test-suite
   "integration: background"

   (test-case "background steps execute before each scenario"
     (define step-log (box '()))
     (run-features bg:features
       #:steps all-steps
       #:before-step
       (lambda (ctx step)
         (set-box! step-log (cons (gherkin-step-text step) (unbox step-log)))
         ctx))
     ;; Two scenarios, each gets 2 background steps + their own steps
     ;; Addition:    "a calculator" "the display is cleared" "I add 2 and 3" "the result is 5"
     ;; Subtraction: "a calculator" "the display is cleared" "I subtract 3 from 10" "the result is 7"
     (check-equal? (reverse (unbox step-log))
                   '("a calculator" "the display is cleared"
                     "I add 2 and 3" "the result is 5"
                     "a calculator" "the display is cleared"
                     "I subtract 3 from 10" "the result is 7")))

   (test-case "background context propagates to scenario steps"
     (define saw-display (box '()))
     (run-features bg:features
       #:steps all-steps
       #:before-step
       (lambda (ctx step)
         (when (equal? (gherkin-step-text step) "I add 2 and 3")
           (set-box! saw-display (hash-ref ctx 'display #f)))
         ctx))
     (check-equal? (unbox saw-display) 'cleared))

   (test-case "reader produces empty background for feature without Background section"
     (define f (car bg:features))
     (check-equal? (length (gherkin-feature-background f)) 2)
     (check-equal? (gherkin-step-type (car (gherkin-feature-background f))) 'given))))

(run-tests background-tests)
