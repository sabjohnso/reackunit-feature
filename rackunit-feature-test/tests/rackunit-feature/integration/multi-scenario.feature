#lang feature

Feature: Multi-step Calculator
  Scenario: Addition with setup
    Given a calculator
    And the display is cleared
    When I add 10 and 20
    Then the result is 30

  Scenario: Just setup
    Given a calculator
    And the display is cleared
