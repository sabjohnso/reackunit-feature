#lang feature

Feature: Calculator with Outline
  Scenario Outline: Arithmetic
    Given a calculator
    When I add <a> and <b>
    Then the result is <result>
    Examples:
      | a | b | result |
      | 1 | 2 | 3      |
      | 4 | 5 | 9      |
