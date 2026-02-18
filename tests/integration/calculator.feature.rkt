#lang rackunit-feature
Steps: "calculator-steps.rkt"

Feature: Calculator
  Scenario: Addition
    Given a calculator
    When I add 2 and 3
    Then the result is 5
