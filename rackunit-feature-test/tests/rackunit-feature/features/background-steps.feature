#lang feature

Feature: Background Steps
  Scenario: Background runs before each scenario
    Given a feature with a background step "setup env" and two scenarios
    When the feature is run
    Then the log should show "setup env" before each scenario

  Scenario: Background context is available to scenario steps
    Given a feature where the background sets env and the scenario reads it
    When the feature is run
    Then the read value should be production
