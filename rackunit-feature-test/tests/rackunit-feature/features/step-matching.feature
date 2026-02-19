#lang feature

Feature: Step Matching
  Scenario: Literal text matches a step definition
    Given a step definition for given "a calculator"
    And a feature with a given step "a calculator"
    When the feature is run
    Then the step handler should have been called

  Scenario: Placeholders capture values from step text
    Given a step definition for when "I add {a} and {b}"
    And a feature with a when step "I add 3 and 5"
    When the feature is run
    Then the captured values should be "3" and "5"

  Scenario: Step type must match the definition type
    Given a step definition for when "I add {a} and {b}"
    And a feature with a given step "I add 3 and 5"
    When the feature is run
    Then the execution should have failed

  Scenario: And keyword inherits the preceding step type
    Given a step definition for given "a calculator"
    And a step definition for given "the display is cleared"
    And a feature with given "a calculator" followed by and "the display is cleared"
    When the feature is run
    Then both step handlers should have been called
