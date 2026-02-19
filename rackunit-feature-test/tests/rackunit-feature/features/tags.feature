#lang feature

Feature: Tags
  Scenario: Feature has tags accessible via accessor
    Given a feature tagged with "@smoke" containing a scenario
    And a step definition for given "noop"
    When the feature is run
    Then the feature tag should be "@smoke"

  Scenario: Scenario has tags accessible via accessor
    Given a feature containing a scenario tagged with "@fast"
    And a step definition for given "noop"
    When the feature is run
    Then the scenario tag should be "@fast"
