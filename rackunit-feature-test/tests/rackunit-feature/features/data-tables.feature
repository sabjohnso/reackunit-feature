#lang feature

Feature: Data Tables
  Scenario: Step handler receives data table
    Given a step definition for given "user list" that receives a table
    And a feature with a given step "user list" with a data table
    When the feature is run
    Then the step handler should have received the table

  Scenario: Data table rows are accessible
    Given a step definition for given "user list" that counts table rows
    And a feature with a given step "user list" with a data table
    When the feature is run
    Then the table row count should be 2
