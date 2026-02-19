#lang feature

Feature: Doc Strings
  Scenario: Step handler receives doc string
    Given a step definition for given "a document" that receives a doc string
    And a feature with a given step "a document" with a doc string
    When the feature is run
    Then the step handler should have received the doc string

  Scenario: Doc string content is preserved
    Given a step definition for given "a document" that stores the doc string
    And a feature with a given step "a document" with doc string content "hello world"
    When the feature is run
    Then the stored doc string should be "hello world"
