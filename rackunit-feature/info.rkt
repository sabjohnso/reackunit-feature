#lang info
(define collection 'multi)
(define deps '("rackunit-feature-lib"
               "rackunit-feature-test"
               "rackunit-feature-doc"))
(define implies '("rackunit-feature-lib"
                  "rackunit-feature-doc"))
(define pkg-desc "Gherkin BDD feature tests for Racket (meta-package)")
(define version "0.2.0")
