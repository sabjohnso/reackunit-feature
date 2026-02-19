#lang racket/base
(require racket/list
         racket/path
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

;; unquoted : string -> unquoted
;; Wrapper so rackunit prints the value without surrounding double quotes.
;; Enables Emacs compilation-mode to recognize "file:line:col" as a jump target.
(struct unquoted (s)
  #:property prop:custom-write
  (lambda (self port mode)
    (write-string (unquoted-s self) port)))

;; relativize-source : any -> string
;; Converts a source (path or string) to a relative path string.
(define (relativize-source source)
  (define p (cond
              [(path? source) source]
              [(string? source) (string->path source)]
              [else #f]))
  (if p
      (path->string (find-relative-path (current-directory) p))
      "unknown"))

;; step-check-info : gherkin-step? -> (listof check-info?)
;; Builds diagnostic context from a step's srcloc and text for check failures.
(define (step-check-info step)
  (define srcloc (gherkin-step-srcloc step))
  (define type (gherkin-step-type step))
  (define text (gherkin-step-text step))
  (list*
   (make-check-info 'step (format "~a ~a" type text))
   (if (and (list? srcloc) (>= (length srcloc) 2))
       (let ([source (car srcloc)]
             [line (cadr srcloc)]
             [col (if (>= (length srcloc) 3) (caddr srcloc) 0)])
         (list (make-check-info 'feature-file
                 (unquoted (format "~a:~a:~a"
                                   (relativize-source source) line col)))))
       '())))

;; run-features : (listof gherkin-feature?) [#:steps ...] [#:hooks ...] -> natural?
;; Builds rackunit test suites from feature data and runs them.
;; Returns the total number of test failures.
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
            (parameterize ([current-check-around (lambda (thunk) (thunk))])
              (define sc-ctx (before-scenario feat-ctx sc))
              (define all-steps
                (append (gherkin-feature-background feat)
                        (gherkin-scenario-steps sc)))
              (define final-ctx
                (for/fold ([ctx sc-ctx]) ([step (in-list all-steps)])
                  (define pre-ctx (before-step ctx step))
                  (with-check-info*
                    (step-check-info step)
                    (lambda ()
                      (define post-ctx
                        (run-step steps
                                  (gherkin-step-type step)
                                  (gherkin-step-text step)
                                  pre-ctx
                                  #:argument (gherkin-step-argument step)))
                      (after-step post-ctx step)))))
              (after-scenario final-ctx sc)
              (void))))))))

  (define total-failures
    (for/fold ([total 0]) ([suite (in-list suites)])
      (+ total (run-tests suite))))
  (after-all base-ctx)
  total-failures)
