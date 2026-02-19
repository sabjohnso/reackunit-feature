#lang feature

Feature: Data Table Support
  Scenario: Step receives data table
    Given the following users:
      | name  | age |
      | Alice | 30  |
      | Bob   | 25  |
    Then there should be 2 users
