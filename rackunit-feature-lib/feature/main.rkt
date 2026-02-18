#lang racket/base
(require (for-syntax racket/base racket/syntax))
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [feature-module-begin #%module-begin]))

(define-syntax (feature-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (with-syntax ([features-id (datum->syntax stx 'features)])
       #'(#%plain-module-begin
           (provide features-id)
           form ...))]))
