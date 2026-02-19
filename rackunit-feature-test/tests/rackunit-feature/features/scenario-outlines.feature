#lang feature

Feature: Scenario Outlines
  Scenario: Outline expands into multiple scenarios
    Given a feature with a scenario outline and 2 example rows
    And a step definition for given "noop"
    When the feature is run
    Then the step handler should have been called 2 times
