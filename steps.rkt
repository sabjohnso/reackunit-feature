#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require rackunit-feature/runtime)
(provide define-steps
         (all-from-out rackunit-feature/runtime))

;; define-steps : identifier (type pattern handler) ...
;; Expands to a definition of `name` as a list of step-defs,
;; and also binds `feature-steps` to the same list.
(define-syntax (define-steps stx)
  (syntax-case stx (given when then)
    [(_ name clause ...)
     (with-syntax ([feature-steps-id (datum->syntax stx 'feature-steps)])
       #'(begin
           (define name
             (list (make-step-clause clause) ...))
           (define feature-steps-id name)))]))

(define-syntax (make-step-clause stx)
  (syntax-case stx (given when then)
    [(_ (given pattern handler))
     #'(step-def 'given pattern handler)]
    [(_ (when pattern handler))
     #'(step-def 'when pattern handler)]
    [(_ (then pattern handler))
     #'(step-def 'then pattern handler)]))
