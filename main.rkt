#lang racket/base
(require (for-syntax racket/base))
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [feature-module-begin #%module-begin]))

(define-syntax (feature-module-begin stx)
  (syntax-case stx (begin)
    [(_ (begin form ...))
     #'(#%plain-module-begin form ...)]
    [(_ form ...)
     #'(#%plain-module-begin form ...)]))
