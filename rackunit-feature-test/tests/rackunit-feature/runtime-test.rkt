#lang racket/base
(require rackunit rackunit/text-ui racket/list)
(require rackunit/feature/runtime)

(define runtime-tests
  (test-suite
   "Runtime"

   (test-suite
    "compile-step-pattern"
    (test-case "literal pattern matches exactly"
      (define rx (compile-step-pattern "a calculator"))
      (check-true (regexp-match? rx "a calculator"))
      (check-false (regexp-match? rx "a toaster")))

    (test-case "placeholder becomes capture group"
      (define rx (compile-step-pattern "I add {a} and {b}"))
      (define m (regexp-match rx "I add 2 and 3"))
      (check-not-false m)
      (check-equal? (cadr m) "2")
      (check-equal? (caddr m) "3"))

    (test-case "pattern with no placeholders has no captures"
      (define rx (compile-step-pattern "a calculator"))
      (define m (regexp-match rx "a calculator"))
      (check-equal? (length m) 1)))

   (test-suite
    "step-def construction"
    (test-case "step-def has type, pattern, and handler"
      (define sd (step-def 'given "a calculator" (lambda (ctx) ctx)))
      (check-equal? (step-def-type sd) 'given)
      (check-equal? (step-def-pattern sd) "a calculator")
      (check-pred procedure? (step-def-handler sd))))

   (test-suite
    "run-step"
    (test-case "matches and runs a step"
      (define steps
        (list (step-def 'given "a calculator"
                        (lambda (ctx) (hash-set ctx 'calc 'ready)))))
      (define ctx (run-step steps 'given "a calculator" (hash)))
      (check-equal? (hash-ref ctx 'calc) 'ready))

    (test-case "captures placeholders and passes to handler"
      (define steps
        (list (step-def 'when "I add {a} and {b}"
                        (lambda (ctx a b)
                          (hash-set ctx 'result
                                    (+ (string->number a)
                                       (string->number b)))))))
      (define ctx (run-step steps 'when "I add 2 and 3" (hash)))
      (check-equal? (hash-ref ctx 'result) 5))

    (test-case "context threads through multiple steps"
      (define steps
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
      (define ctx0 (hash))
      (define ctx1 (run-step steps 'given "a calculator" ctx0))
      (define ctx2 (run-step steps 'when "I add 2 and 3" ctx1))
      (define ctx3 (run-step steps 'then "the result is 5" ctx2))
      (check-equal? (hash-ref ctx3 'result) 5))

    (test-case "raises error on unmatched step"
      (check-exn exn:fail?
                 (lambda ()
                   (run-step '() 'given "nonexistent step" (hash)))))

    (test-case "matches correct type even with same text"
      (define steps
        (list (step-def 'given "x"
                        (lambda (ctx) (hash-set ctx 'from 'given)))
              (step-def 'when "x"
                        (lambda (ctx) (hash-set ctx 'from 'when)))))
      (define ctx (run-step steps 'when "x" (hash)))
      (check-equal? (hash-ref ctx 'from) 'when))

    (test-case "argument is passed as last parameter when non-#f"
      (define steps
        (list (step-def 'given "a table"
                        (lambda (ctx table)
                          (hash-set ctx 'table table)))))
      (define table '(("a" "b") ("1" "2")))
      (define ctx (run-step steps 'given "a table" (hash)
                            #:argument table))
      (check-equal? (hash-ref ctx 'table) table))

    (test-case "argument with captures: captures come before argument"
      (define steps
        (list (step-def 'when "I enter {x}"
                        (lambda (ctx x table)
                          (hash-set (hash-set ctx 'x x) 'table table)))))
      (define table '(("a") ("1")))
      (define ctx (run-step steps 'when "I enter foo" (hash)
                            #:argument table))
      (check-equal? (hash-ref ctx 'x) "foo")
      (check-equal? (hash-ref ctx 'table) table))

    (test-case "argument #f does not add extra parameter (backward compatible)"
      (define steps
        (list (step-def 'given "a calculator"
                        (lambda (ctx) (hash-set ctx 'calc 'ready)))))
      (define ctx (run-step steps 'given "a calculator" (hash)
                            #:argument #f))
      (check-equal? (hash-ref ctx 'calc) 'ready)))))

(run-tests runtime-tests)
