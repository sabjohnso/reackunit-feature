#lang racket/base
(require scribble/manual
         (for-label racket/base
                    racket/contract
                    rackunit
                    rackunit/feature
                    rackunit/feature/runtime
                    rackunit/feature/private/ast))

(provide (for-label (all-from-out racket/base
                                  racket/contract
                                  rackunit
                                  rackunit/feature
                                  rackunit/feature/runtime
                                  rackunit/feature/private/ast))
         (all-from-out scribble/manual)
         reftech)

(define (reftech . content)
  (apply tech content #:doc '(lib "scribblings/reference/reference.scrbl")))
