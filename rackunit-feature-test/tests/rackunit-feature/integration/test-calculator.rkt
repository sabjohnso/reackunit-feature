#lang racket/base
(require rackunit/feature)
(require "calculator-steps.rkt")
(require "calculator.feature")

(run-features features
  #:steps calculator-steps)
