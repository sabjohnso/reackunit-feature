#lang racket/base
(require racket/list "ast.rkt")
(provide compile-document)

;; compile-document : gherkin-document -> syntax
;; Transforms a Gherkin AST into IR syntax objects:
;;   (begin
;;     (require rackunit rackunit/text-ui rackunit-feature/runtime)
;;     (require (prefix-in steps-0: "file0.rkt")) ...
;;     (define all-step-defs (append steps-0:feature-steps ...))
;;     (run-tests (test-suite "Feature: ..." ...)))
(define (compile-document doc)
  (define step-files (gherkin-document-step-files doc))
  (define features (gherkin-document-features doc))

  (define step-requires (compile-step-requires step-files))
  (define all-defs-form (compile-all-defs step-files))
  (define test-forms (map compile-feature features))

  (datum->syntax
   #f
   `(begin
      (require rackunit rackunit/text-ui rackunit-feature/runtime)
      ,@step-requires
      ,all-defs-form
      ,@(map (lambda (tf) `(run-tests ,tf)) test-forms))))

(define (compile-step-requires step-files)
  (for/list ([file (in-list step-files)]
             [i (in-naturals)])
    (define prefix (string->symbol (format "steps-~a:" i)))
    `(require (prefix-in ,prefix ,file))))

(define (compile-all-defs step-files)
  (define step-refs
    (for/list ([file (in-list step-files)]
               [i (in-naturals)])
      (string->symbol (format "steps-~a:feature-steps" i))))
  `(define all-step-defs (append ,@step-refs)))

(define (compile-feature feat)
  (define name (format "Feature: ~a" (gherkin-feature-name feat)))
  (define scenario-forms (map compile-scenario (gherkin-feature-scenarios feat)))
  `(test-suite ,name ,@scenario-forms))

(define (compile-scenario sc)
  (define name (format "Scenario: ~a" (gherkin-scenario-name sc)))
  (define steps (gherkin-scenario-steps sc))
  (define bindings (compile-step-bindings steps))
  `(test-case ,name
     (let* ,bindings (void))))

(define (compile-step-bindings steps)
  (if (null? steps)
      '([ctx (hash)])
      (for/list ([step (in-list steps)]
                 [i (in-naturals)])
        (define type (gherkin-step-type step))
        (define text (gherkin-step-text step))
        (define prev (if (= i 0) '(hash) 'ctx))
        `[ctx (run-step all-step-defs ',type ,text ,prev)])))
