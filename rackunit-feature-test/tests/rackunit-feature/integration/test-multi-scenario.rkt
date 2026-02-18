#lang racket/base
(require rackunit rackunit/feature)
(require "calculator-steps.rkt")
(require (prefix-in ms: "multi-scenario.feature"))

;; Multi-scenario feature uses And steps and multiple scenarios.
;; We need an additional step for "the display is cleared".
(define extra-steps
  (list (step-def 'given "the display is cleared"
                  (lambda (ctx) (hash-set ctx 'display 'cleared)))))

(run-features ms:features
  #:steps (append calculator-steps extra-steps))
