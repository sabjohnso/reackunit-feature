#lang racket/base
(require rackunit rackunit/text-ui rackunit/feature)
(require "calculator-steps.rkt")
(require "calculator.feature")

(define hooks-tests
  (test-suite
   "integration: hooks"

   (test-case "before-scenario receives scenario AST node"
     (define names (box '()))
     (run-features features
       #:steps calculator-steps
       #:before-scenario
       (lambda (ctx sc)
         (set-box! names (cons (gherkin-scenario-name sc) (unbox names)))
         ctx))
     (check-equal? (sort (unbox names) string<?)
                   '("Addition" "Subtraction")))

   (test-case "after-scenario sees accumulated context"
     (define results (box '()))
     (run-features features
       #:steps calculator-steps
       #:after-scenario
       (lambda (ctx sc)
         (set-box! results (cons (hash-ref ctx 'result) (unbox results)))
         ctx))
     (check-equal? (sort (unbox results) <) '(5 7)))

   (test-case "before-all initializes shared context"
     (define saw-setup (box #f))
     (run-features
      (list (gherkin-feature
             #f "F" '() '()
             (list (gherkin-scenario
                    #f "S" '()
                    (list (gherkin-step #f 'given "a calculator" #f))))))
      #:steps (list (step-def 'given "a calculator"
                              (lambda (ctx)
                                (set-box! saw-setup (hash-ref ctx 'setup #f))
                                ctx)))
      #:before-all (lambda (ctx) (hash-set ctx 'setup 'initialized)))
     (check-equal? (unbox saw-setup) 'initialized))

   (test-case "scenarios are isolated from each other"
     (define s2-has-result (box 'not-checked))
     (run-features features
       #:steps (list (step-def 'given "a calculator"
                               (lambda (ctx)
                                 (set-box! s2-has-result
                                           (hash-has-key? ctx 'result))
                                 (hash-set ctx 'calc 'ready)))
                     (step-def 'when "I add {a} and {b}"
                               (lambda (ctx a b)
                                 (hash-set ctx 'result
                                           (+ (string->number a)
                                              (string->number b)))))
                     (step-def 'when "I subtract {a} from {b}"
                               (lambda (ctx a b)
                                 (hash-set ctx 'result
                                           (- (string->number b)
                                              (string->number a)))))
                     (step-def 'then "the result is {n}"
                               (lambda (ctx n)
                                 (check-equal? (hash-ref ctx 'result)
                                               (string->number n))
                                 ctx))))
     ;; The second call to "given a calculator" (in Subtraction scenario)
     ;; should NOT see 'result from the Addition scenario.
     (check-false (unbox s2-has-result)))))

(run-tests hooks-tests)
