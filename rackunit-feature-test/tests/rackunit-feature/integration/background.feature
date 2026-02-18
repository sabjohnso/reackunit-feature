#lang feature

Feature: Calculator with Background
  Background:
    Given a calculator
    And the display is cleared

  Scenario: Addition
    When I add 2 and 3
    Then the result is 5

  Scenario: Subtraction
    When I subtract 3 from 10
    Then the result is 7
