#lang feature

Feature: Calculator
  Scenario: Addition
    Given a calculator
    When I add 2 and 3
    Then the result is 5

  Scenario: Subtraction
    Given a calculator
    When I subtract 3 from 10
    Then the result is 7
