#lang racket/base
(require racket/list
         rackunit
         rackunit/text-ui
         rackunit/feature/private/ast
         rackunit/feature/runtime
         (for-syntax racket/base))
(provide run-features
         define-steps
         (all-from-out rackunit/feature/runtime)
         (all-from-out rackunit/feature/private/ast))

;; define-steps : syntax
;; Matches given/when/then by datum so racket/base's `when` is not shadowed.
(define-syntax (define-steps stx)
  (define (step-type-symbol s)
    (case (syntax->datum s)
      [(given) #''given]
      [(when)  #''when]
      [(then)  #''then]
      [else (raise-syntax-error 'define-steps
              "expected given, when, or then" s)]))
  (syntax-case stx ()
    [(_ name clause ...)
     (with-syntax ([(step-expr ...)
                    (map (lambda (c)
                           (syntax-case c ()
                             [(kw pattern handler)
                              (memq (syntax->datum #'kw) '(given when then))
                              (with-syntax ([type-sym (step-type-symbol #'kw)])
                                #'(step-def type-sym pattern handler))]))
                         (syntax->list #'(clause ...)))])
       #'(define name (list step-expr ...)))]))

;; run-features : (listof gherkin-feature?) [#:steps ...] [#:hooks ...] -> void
;; Builds rackunit test suites from feature data and runs them.
(define (run-features features
          #:steps [steps '()]
          #:before-all [before-all (lambda (ctx) ctx)]
          #:after-all [after-all (lambda (ctx) ctx)]
          #:before-feature [before-feature (lambda (ctx f) ctx)]
          #:after-feature [after-feature (lambda (ctx f) ctx)]
          #:before-scenario [before-scenario (lambda (ctx sc) ctx)]
          #:after-scenario [after-scenario (lambda (ctx sc) ctx)]
          #:before-step [before-step (lambda (ctx st) ctx)]
          #:after-step [after-step (lambda (ctx st) ctx)])

  (define base-ctx (before-all (hash)))

  (define suites
    (for/list ([feat (in-list features)])
      (define feat-ctx (before-feature base-ctx feat))
      (make-test-suite
       (format "Feature: ~a" (gherkin-feature-name feat))
       (for/list ([sc (in-list (gherkin-feature-scenarios feat))])
         (make-test-case
          (format "Scenario: ~a" (gherkin-scenario-name sc))
          (lambda ()
            (define sc-ctx (before-scenario feat-ctx sc))
            (define final-ctx
              (for/fold ([ctx sc-ctx]) ([step (in-list (gherkin-scenario-steps sc))])
                (define pre-ctx (before-step ctx step))
                (define post-ctx
                  (run-step steps
                            (gherkin-step-type step)
                            (gherkin-step-text step)
                            pre-ctx))
                (after-step post-ctx step)))
            (after-scenario final-ctx sc)
            (void)))))))

  (for ([suite (in-list suites)])
    (run-tests suite))
  (after-all base-ctx)
  (void))
