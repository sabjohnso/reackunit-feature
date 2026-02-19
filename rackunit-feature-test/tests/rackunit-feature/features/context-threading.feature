#lang feature

Feature: Context Threading
  Scenario: Context flows from one step to the next
    Given a step definition for given "set color" that sets color to red
    And a step definition for then "check color" that reads color
    And a feature with given "set color" and then "check color"
    When the feature is run
    Then the read value should be red

  Scenario: Each scenario starts with a fresh context
    Given a feature where the first scenario sets a key and the second checks it
    When the feature is run
    Then the second scenario should not have seen the key

  Scenario: The before-all hook initializes shared context
    Given a step definition for given "noop" that reads shared
    And a feature with a given step "noop"
    And a before-all hook that sets shared to initialized
    When the feature is run
    Then the read value should be initialized
