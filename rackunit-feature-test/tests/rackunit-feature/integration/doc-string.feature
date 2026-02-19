#lang feature

Feature: Doc String Support
  Scenario: Step receives doc string
    Given a document:
      """
      Hello world
      from a doc string
      """
    Then the document should contain Hello world
