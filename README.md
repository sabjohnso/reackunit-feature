# rackunit-feature

Gherkin BDD feature tests for Racket.

## Quick Start

A complete BDD workflow requires three files:

**1. A feature file** (`calculator.feature`):

```gherkin
#lang feature

Feature: Calculator
  Scenario: Addition
    Given a calculator
    When I add 2 and 3
    Then the result is 5
```

**2. Step definitions** (`calculator-steps.rkt`):

```racket
#lang racket/base
(require rackunit rackunit/feature)
(provide calculator-steps)

(define-steps calculator-steps
  (given "a calculator"
    (lambda (ctx) (hash-set ctx 'calc 'ready)))
  (when "I add {a} and {b}"
    (lambda (ctx a b)
      (hash-set ctx 'result (+ (string->number a) (string->number b)))))
  (then "the result is {n}"
    (lambda (ctx n)
      (check-equal? (hash-ref ctx 'result) (string->number n))
      ctx)))
```

**3. A test runner** (`test-calculator.rkt`):

```racket
#lang racket/base
(require rackunit/feature)
(require "calculator-steps.rkt")
(require "calculator.feature")

(run-features features
  #:steps calculator-steps)
```

Run with `raco test test-calculator.rkt`. The `features` identifier is
automatically provided by the `#lang feature` module.

## Installation

```
raco pkg install rackunit-feature
```

## Supported Gherkin Syntax

```gherkin
#lang feature

@smoke
Feature: Shopping Cart
  Background:
    Given an empty cart

  @fast
  Scenario: Add item
    When I add "Milk" to the cart
    Then the cart has 1 item

  Scenario Outline: Pricing
    When I add <item> at <price>
    Then the total is <price>
    Examples:
      | item  | price |
      | Bread | 2     |
      | Eggs  | 3     |
```

- `Feature:` -- groups related scenarios under a name
- `Background:` -- steps that run before every scenario in the feature
- `Scenario:` -- an individual test case with its own isolated context
- `Scenario Outline:` -- a parameterized template, expanded once per `Examples:` row
- `Given`, `When`, `Then` -- step keywords for preconditions, actions, and assertions
- `And`, `But` -- continuation keywords that inherit the type of the preceding step
- Tags -- `@`-prefixed labels on the line before a feature or scenario keyword
- Data Tables -- pipe-delimited tables following a step
- Doc Strings -- triple-quoted blocks following a step

## Step Definitions

Use `define-steps` to create a named list of step definitions. Each clause
matches a step keyword (`given`, `when`, or `then`) and a pattern string:

```racket
(define-steps my-steps
  (given "a user named {name}"
    (lambda (ctx name)
      (hash-set ctx 'user name)))
  (then "the greeting is {msg}"
    (lambda (ctx msg)
      (check-equal? (hash-ref ctx 'greeting) msg)
      ctx)))
```

**Placeholders:** Text inside `{braces}` becomes a capture group. Captured
values are passed to the handler as strings, in order, after the context
argument. Convert with `string->number` or similar as needed.

**Handler contract:** Every handler receives the context hash as its first
argument, followed by captured placeholder values. If the step has a data
table or doc string, that value is passed as the final argument. The handler
must return a hash -- this becomes the context for the next step.

**Data table example:**

```racket
(given "the following users:"
  (lambda (ctx table)
    (define header (car table))
    (define rows (cdr table))
    (hash-set ctx 'users rows)))
```

**Doc string example:**

```racket
(given "a document:"
  (lambda (ctx doc)
    (hash-set ctx 'document doc)))
```

## Lifecycle Hooks

Hooks run setup and teardown logic at various points. Each hook receives the
current context hash (and the relevant AST node where applicable) and must
return a hash:

```racket
(run-features features
  #:steps my-steps
  #:before-all
  (lambda (ctx) (hash-set ctx 'db (connect-db)))
  #:after-all
  (lambda (ctx) (disconnect (hash-ref ctx 'db)) ctx)
  #:before-scenario
  (lambda (ctx sc) (hash-set ctx 'tx (begin-tx ctx)))
  #:after-scenario
  (lambda (ctx sc) (rollback (hash-ref ctx 'tx)) ctx))
```

**Execution order** for a feature with two scenarios:

```
before-all           (ctx)        -> ctx'
  before-feature     (ctx', feat) -> ctx''
    before-scenario  (ctx'', sc1) -> ctx-a
      before-step    (ctx-a, step)  -> ...
      step handler
      after-step     (ctx, step)    -> ...
    after-scenario   (ctx, sc1)
    before-scenario  (ctx'', sc2) -> ctx-b  (fresh from before-feature)
      ...
    after-scenario   (ctx, sc2)
  after-feature      (ctx'', feat)
after-all            (ctx')
```

Each scenario starts from the context returned by `#:before-feature`,
providing isolation between scenarios.

## Multiple Feature Files

Use `prefix-in` to avoid name collisions on the `features` binding when
requiring multiple feature files:

```racket
(require "calculator.feature")
(require (prefix-in s: "shopping.feature"))

(run-features (append features
                      s:features)
  #:steps (append calc-steps shop-steps))
```

## Project Structure

| Package | Description |
|---------|-------------|
| `rackunit-feature` | Meta-package (depends on lib, test, doc) |
| `rackunit-feature-lib` | Core library: parser, runtime, `#lang feature` |
| `rackunit-feature-test` | Test suite |
| `rackunit-feature-doc` | Scribble documentation |

## Documentation

```
raco docs rackunit-feature
```

## Running Tests

```
raco test rackunit-feature-test
```
