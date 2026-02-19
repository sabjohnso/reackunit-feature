#lang scribble/manual
@(require "util.rkt")

@title[#:tag "guide"]{Guide}

This guide walks through using @racketmodname[rackunit/feature] for
Behavior-Driven Development in Racket. Each section builds on the
previous one.

@; -------------------------------------------------------------------
@section[#:tag "quick-start"]{Quick Start}

A complete BDD workflow requires three files:

@bold{1. A feature file} (@filepath{calculator.feature}):

@codeblock[#:keep-lang-line? #t]{
#lang feature

Feature: Calculator
  Scenario: Addition
    Given a calculator
    When I add 2 and 3
    Then the result is 5
}

@bold{2. Step definitions} (@filepath{calculator-steps.rkt}):

@racketblock[
(require rackunit rackunit/feature)
(provide calculator-steps)

(define-steps calculator-steps
  (given "a calculator"
    (lambda (ctx) (hash-set ctx 'calc 'ready)))
  (when "I add {a} and {b}"
    (lambda (ctx a b)
      (hash-set ctx 'result (+ (string->number a)
                                (string->number b)))))
  (then "the result is {n}"
    (lambda (ctx n)
      (check-equal? (hash-ref ctx 'result) (string->number n))
      ctx)))
]

@bold{3. A test runner} (@filepath{test-calculator.rkt}):

@racketblock[
(require rackunit/feature)
(require "calculator-steps.rkt")
(require "calculator.feature")

(run-features features
  #:steps calculator-steps)
]

Run with @exec{raco test test-calculator.rkt}. The
@racket[features] identifier is automatically provided by the
@tt{#lang feature} module.

@; -------------------------------------------------------------------
@section[#:tag "feature-syntax"]{Feature File Syntax}

Feature files use @tt{#lang feature} and follow a
subset of the Gherkin specification:

@codeblock[#:keep-lang-line? #t]|{
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
}|

@subsection{Keywords}

@itemlist[
  @item{@tt{Feature:} --- groups related scenarios under a name.}
  @item{@tt{Background:} --- steps that run before @emph{every}
        scenario in the feature.}
  @item{@tt{Scenario:} --- an individual test case with its own
        isolated context.}
  @item{@tt{Scenario Outline:} --- a parameterized scenario template,
        expanded once per row in the @tt{Examples:} table.}
  @item{@tt{Given}, @tt{When}, @tt{Then} --- step keywords that
        express preconditions, actions, and assertions.}
  @item{@tt{And}, @tt{But} --- continuation keywords that inherit the
        type of the preceding step.}
]

@; -------------------------------------------------------------------
@section[#:tag "step-definitions"]{Step Definitions}

Use @racket[define-steps] to create a named list of step definitions.
Each clause matches a step keyword (@racket[given], @racket[when], or
@racket[then]) and a pattern string:

@racketblock[
(define-steps my-steps
  (given "a user named {name}"
    (lambda (ctx name)
      (hash-set ctx 'user name)))
  (then "the greeting is {msg}"
    (lambda (ctx msg)
      (check-equal? (hash-ref ctx 'greeting) msg)
      ctx)))
]

@subsection{Placeholders}

Text inside @tt{{braces}} in a pattern becomes a capture group. Captured
values are passed to the handler as strings, in order, after the context
argument. All captures are strings --- convert with
@racket[string->number] or similar as needed.

@subsection{Handler Contract}

Every step handler receives the context @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{hash table} as its first
argument, followed by any captured placeholder values. If the step has a
@seclink["data-tables"]{data table} or @seclink["doc-strings"]{doc string},
that value is passed as the final argument.

The handler @bold{must return} a hash --- this becomes the context for
the next step.

@; -------------------------------------------------------------------
@section[#:tag "running"]{Running Features}

The @racket[run-features] function takes a list of
@racket[gherkin-feature] structures (as provided by
@tt{#lang feature} files) and runs them as @racketmodname[rackunit]
test suites:

@racketblock[
(require "calculator.feature")
(require (prefix-in s: "shopping.feature"))

(run-features (append features
                      s:features)
  #:steps (append calc-steps shop-steps))
]

To require multiple feature files, use @racket[prefix-in] to avoid
name collisions on the @racket[features] binding:

@racketblock[
(require "calculator.feature")
(require (prefix-in s: "shopping.feature"))
]

@; -------------------------------------------------------------------
@section[#:tag "hooks"]{Lifecycle Hooks}

Hooks let you run setup and teardown logic at various points. Each hook
receives the current context hash (and the relevant AST node where
applicable) and must return a hash:

@racketblock[
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
]

@subsection{Hook Execution Order}

For a feature with two scenarios, hooks execute in this order:

@verbatim|{
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
}|

Note: each scenario starts from the context returned by
@racket[#:before-feature], providing isolation between scenarios.

@; -------------------------------------------------------------------
@section[#:tag "data-tables"]{Data Tables}

A step can have a pipe-delimited data table immediately following it:

@verbatim|{
    Given the following users:
      | name  | age |
      | Alice | 30  |
      | Bob   | 25  |
}|

The table is passed to the step handler as the final argument ---
a list of lists of strings (rows of cells). The first row is
typically a header:

@racketblock[
(given "the following users:"
  (lambda (ctx table)
    (define header (car table))
    (define rows (cdr table))
    (hash-set ctx 'users rows)))
]

@; -------------------------------------------------------------------
@section[#:tag "doc-strings"]{Doc Strings}

A step can have a triple-quoted doc string following it:

@verbatim|{
    Given a document:
      """
      Hello world
      from a doc string
      """
}|

The doc string is passed to the handler as a single string with
leading indentation stripped:

@racketblock[
(given "a document:"
  (lambda (ctx doc)
    (hash-set ctx 'document doc)))
]

@; -------------------------------------------------------------------
@section[#:tag "tags"]{Tags}

Tags annotate features and scenarios with @"@"-prefixed labels.
Place them on the line before the feature or scenario keyword:

@verbatim|{
@smoke
Feature: Login
  @fast @critical
  Scenario: Valid credentials
    Given a registered user
    ...
}|

Tags are stored in the @racket[gherkin-feature-tags] and
@racket[gherkin-scenario-tags] fields as lists of strings
(e.g., @racket['("@smoke")]). You can use them in hooks to
implement tag-based filtering or conditional logic.

@; -------------------------------------------------------------------
@section[#:tag "scenario-outlines"]{Scenario Outlines}

A @tt{Scenario Outline} is a template expanded at parse time into
one concrete scenario per row in the @tt{Examples:} table. Use
angle-bracket placeholders (@tt{<name>}) in steps:

@codeblock[#:keep-lang-line? #t]{
#lang feature

Feature: Arithmetic
  Scenario Outline: Operations
    Given a calculator
    When I add <a> and <b>
    Then the result is <result>
    Examples:
      | a | b | result |
      | 1 | 2 | 3      |
      | 4 | 5 | 9      |
}

This produces two scenarios: @tt{Operations (a=1, b=2, result=3)} and
@tt{Operations (a=4, b=5, result=9)}. The step text has the
placeholders replaced with values, so existing step definitions match
without any changes.
