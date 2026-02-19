#lang racket/base
(require rackunit rackunit/text-ui racket/list)
(require rackunit/feature/private/ast)
(require rackunit/feature/runtime)
(require rackunit/feature/main)

(define test-steps
  (list (step-def 'given "a calculator"
                  (lambda (ctx) (hash-set ctx 'calc 'ready)))
        (step-def 'when "I add {a} and {b}"
                  (lambda (ctx a b)
                    (hash-set ctx 'result
                              (+ (string->number a)
                                 (string->number b)))))
        (step-def 'then "the result is {n}"
                  (lambda (ctx n)
                    (check-equal? (hash-ref ctx 'result)
                                  (string->number n))
                    ctx))))

(define sample-features
  (list (gherkin-feature
         #f "Calculator" '() '()
         (list (gherkin-scenario
                #f "Addition" '()
                (list (gherkin-step #f 'given "a calculator" #f)
                      (gherkin-step #f 'when "I add 2 and 3" #f)
                      (gherkin-step #f 'then "the result is 5" #f)))))))

(define run-features-tests
  (test-suite
   "run-features"

   (test-case "runs a passing feature without error"
     (check-not-exn
      (lambda ()
        (run-features sample-features
                      #:steps test-steps))))

   ;; Failing step and missing step behavior are tested in integration/
   ;; because run-features delegates to run-tests, whose inner results
   ;; propagate to raco test's failure count.

   (test-case "before-scenario hook is called"
     (define hook-log (box '()))
     (run-features sample-features
                   #:steps test-steps
                   #:before-scenario
                   (lambda (ctx sc)
                     (set-box! hook-log
                               (cons (gherkin-scenario-name sc)
                                     (unbox hook-log)))
                     ctx))
     (check-equal? (unbox hook-log) '("Addition")))

   (test-case "after-scenario hook receives accumulated context"
     (define final-ctx (box #f))
     (run-features sample-features
                   #:steps test-steps
                   #:after-scenario
                   (lambda (ctx sc)
                     (set-box! final-ctx ctx)
                     ctx))
     (check-equal? (hash-ref (unbox final-ctx) 'result) 5))

   (test-case "before-all hook initializes context"
     (define saw-key (box #f))
     (run-features
      (list (gherkin-feature
             #f "F" '() '()
             (list (gherkin-scenario
                    #f "S" '()
                    (list (gherkin-step #f 'given "a calculator" #f))))))
      #:steps (list (step-def 'given "a calculator"
                              (lambda (ctx)
                                (set-box! saw-key (hash-ref ctx 'setup #f))
                                ctx)))
      #:before-all (lambda (ctx) (hash-set ctx 'setup 'done)))
     (check-equal? (unbox saw-key) 'done))

   (test-case "background steps fire before scenario steps"
     (define step-log (box '()))
     (define bg-features
       (list (gherkin-feature
              #f "F" '()
              (list (gherkin-step #f 'given "a calculator" #f))
              (list (gherkin-scenario
                     #f "S" '()
                     (list (gherkin-step #f 'when "I add 2 and 3" #f)
                           (gherkin-step #f 'then "the result is 5" #f)))))))
     (run-features bg-features
       #:steps (list (step-def 'given "a calculator"
                               (lambda (ctx)
                                 (set-box! step-log (cons 'bg-given (unbox step-log)))
                                 (hash-set ctx 'calc 'ready)))
                     (step-def 'when "I add {a} and {b}"
                               (lambda (ctx a b)
                                 (set-box! step-log (cons 'when-add (unbox step-log)))
                                 (hash-set ctx 'result
                                           (+ (string->number a) (string->number b)))))
                     (step-def 'then "the result is {n}"
                               (lambda (ctx n)
                                 (set-box! step-log (cons 'then-result (unbox step-log)))
                                 (check-equal? (hash-ref ctx 'result) (string->number n))
                                 ctx))))
     (check-equal? (reverse (unbox step-log))
                   '(bg-given when-add then-result)))

   (test-case "background runs for every scenario"
     (define bg-count (box 0))
     (define bg-features
       (list (gherkin-feature
              #f "F" '()
              (list (gherkin-step #f 'given "setup" #f))
              (list (gherkin-scenario #f "S1" '()
                     (list (gherkin-step #f 'given "do something" #f)))
                    (gherkin-scenario #f "S2" '()
                     (list (gherkin-step #f 'given "do something" #f)))))))
     (run-features bg-features
       #:steps (list (step-def 'given "setup"
                               (lambda (ctx)
                                 (set-box! bg-count (add1 (unbox bg-count)))
                                 ctx))
                     (step-def 'given "do something"
                               (lambda (ctx) ctx))))
     (check-equal? (unbox bg-count) 2))

   (test-case "hooks fire for background steps"
     (define step-hook-log (box '()))
     (define bg-features
       (list (gherkin-feature
              #f "F" '()
              (list (gherkin-step #f 'given "setup" #f))
              (list (gherkin-scenario #f "S" '()
                     (list (gherkin-step #f 'when "act" #f)))))))
     (run-features bg-features
       #:steps (list (step-def 'given "setup" (lambda (ctx) ctx))
                     (step-def 'when "act" (lambda (ctx) ctx)))
       #:before-step (lambda (ctx step)
                       (set-box! step-hook-log
                                 (cons (gherkin-step-text step)
                                       (unbox step-hook-log)))
                       ctx))
     (check-equal? (reverse (unbox step-hook-log)) '("setup" "act")))

   (test-case "scenarios are isolated from each other"
     (define multi-scenario-features
       (list (gherkin-feature
              #f "F" '() '()
              (list (gherkin-scenario
                     #f "S1" '()
                     (list (gherkin-step #f 'given "a calculator" #f)
                           (gherkin-step #f 'when "I add 2 and 3" #f)))
                    (gherkin-scenario
                     #f "S2" '()
                     (list (gherkin-step #f 'given "a calculator" #f)))))))
     (define s2-has-result (box #f))
     (run-features multi-scenario-features
                   #:steps (list (step-def 'given "a calculator"
                                           (lambda (ctx)
                                             (set-box! s2-has-result
                                                       (hash-has-key? ctx 'result))
                                             (hash-set ctx 'calc 'ready)))
                                 (step-def 'when "I add {a} and {b}"
                                           (lambda (ctx a b)
                                             (hash-set ctx 'result
                                                       (+ (string->number a)
                                                          (string->number b)))))))
     ;; S2 should NOT see S1's 'result key
     (check-false (unbox s2-has-result)))

   (test-case "data table argument is threaded to step handler"
     (define received-table (box #f))
     (define table-features
       (list (gherkin-feature
              #f "F" '() '()
              (list (gherkin-scenario
                     #f "S" '()
                     (list (gherkin-step #f 'given "a table"
                                         '(("name" "age") ("Alice" "30")))))))))
     (run-features table-features
       #:steps (list (step-def 'given "a table"
                               (lambda (ctx table)
                                 (set-box! received-table table)
                                 ctx))))
     (check-equal? (unbox received-table)
                   '(("name" "age") ("Alice" "30"))))))

(run-tests run-features-tests)
