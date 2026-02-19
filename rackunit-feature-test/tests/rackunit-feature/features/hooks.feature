#lang feature

Feature: Lifecycle Hooks
  Scenario: The before-scenario hook receives the scenario name
    Given a feature with a scenario named "Login"
    And a before-scenario hook that logs the scenario name
    When the feature is run
    Then the logged scenario name should be "Login"

  Scenario: The after-scenario hook sees accumulated context
    Given a step definition for given "set color" that sets color to red
    And a feature with a given step "set color"
    And an after-scenario hook that captures the color key
    When the feature is run
    Then the captured result should be red

  Scenario: Step hooks fire for every step
    Given a feature with 3 steps
    And before-step and after-step hooks that count invocations
    When the feature is run
    Then before-step should have fired 3 times
    And after-step should have fired 3 times
