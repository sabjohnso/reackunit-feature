#lang scribble/manual
@(require "util.rkt")

@title[#:tag "reference"]{API Reference}

@; ===================================================================
@section[#:tag "core-api"]{Core API}

The @racketmodname[rackunit/feature] module provides the two primary
entry points for defining and running BDD tests.

@defform[(define-steps name clause ...)
         #:grammar ([clause (given pattern handler)
                            (when pattern handler)
                            (then pattern handler)]
                    [pattern string?]
                    [handler (-> hash? any/c ... hash?)])]{
Defines @racket[name] as a list of @racket[step-def] structures.

Each @racket[clause] pairs a step keyword with a pattern and handler
function. The keywords @racket[given], @racket[when], and @racket[then]
are matched by datum --- they do not shadow @racketmodname[racket/base]'s
@racket[when].

The @racket[pattern] is a string that may contain @tt{{placeholder}}
markers. Each placeholder becomes a capture group. Captured values are
passed to the @racket[handler] as strings after the context argument.

@racketblock[
(define-steps my-steps
  (given "a user named {name}"
    (lambda (ctx name)
      (hash-set ctx 'user name)))
  (when "the user logs in"
    (lambda (ctx)
      (hash-set ctx 'logged-in? #t)))
  (then "the welcome message is {msg}"
    (lambda (ctx msg)
      (check-equal? (hash-ref ctx 'welcome) msg)
      ctx)))
]}

@defproc[(run-features
          [features (listof gherkin-feature?)]
          [#:steps steps (listof step-def?) '()]
          [#:before-all before-all (-> hash? hash?) (lambda (ctx) ctx)]
          [#:after-all after-all (-> hash? hash?) (lambda (ctx) ctx)]
          [#:before-feature before-feature (-> hash? gherkin-feature? hash?)
                           (lambda (ctx f) ctx)]
          [#:after-feature after-feature (-> hash? gherkin-feature? hash?)
                          (lambda (ctx f) ctx)]
          [#:before-scenario before-scenario (-> hash? gherkin-scenario? hash?)
                            (lambda (ctx sc) ctx)]
          [#:after-scenario after-scenario (-> hash? gherkin-scenario? hash?)
                           (lambda (ctx sc) ctx)]
          [#:before-step before-step (-> hash? gherkin-step? hash?)
                        (lambda (ctx st) ctx)]
          [#:after-step after-step (-> hash? gherkin-step? hash?)
                       (lambda (ctx st) ctx)])
         void?]{
Builds @racketmodname[rackunit] test suites from @racket[features] and
runs them.

An empty @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{hash table} is created, passed through @racket[before-all],
then threaded through features, scenarios, and steps. Each scenario
receives a fresh copy of the context from @racket[before-feature],
isolating scenarios from one another. Background steps (if any) run
before each scenario's own steps.

See the @seclink["hooks"]{Lifecycle Hooks} section of the guide for the
full execution order.}

@; ===================================================================
@section[#:tag "ast-types"]{AST Types}

@defmodule[rackunit/feature/private/ast]

These @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{prefab} structures represent parsed Gherkin documents.
They are produced by the @tt{#lang feature} reader and
consumed by @racket[run-features].

@defstruct*[gherkin-document ([srcloc any/c]
                              [features (listof gherkin-feature?)])
            #:prefab]{
The top-level container returned by the parser. Contains the list of
@racket[gherkin-feature] structures found in the source file. Most users
interact with @racket[features] directly (as provided by
@tt{#lang feature}) rather than the document wrapper.}

@defstruct*[gherkin-feature ([srcloc any/c]
                             [name string?]
                             [tags (listof string?)]
                             [background (listof gherkin-step?)]
                             [scenarios (listof gherkin-scenario?)])
            #:prefab]{
A single @tt{Feature:} block. The @racket[tags] field contains tag
strings (e.g., @racket["@smoke"]). The @racket[background] is a
(possibly empty) list of steps that run before every scenario.}

@defstruct*[gherkin-scenario ([srcloc any/c]
                              [name string?]
                              [tags (listof string?)]
                              [steps (listof gherkin-step?)])
            #:prefab]{
A @tt{Scenario:} or expanded @tt{Scenario Outline:} instance. When
expanded from an outline, the @racket[name] includes parameter values
(e.g., @racket["Addition (a=1, b=2, result=3)"]).}

@defstruct*[gherkin-step ([srcloc any/c]
                          [type symbol?]
                          [text string?]
                          [argument any/c])
            #:prefab]{
A single step. The @racket[type] is one of @racket['given],
@racket['when], @racket['then], @racket['and], or @racket['but].
The @racket[argument] is @racket[#f] when absent, a
@racket[(listof (listof string?))] for a data table, or a
@racket[string?] for a doc string.}

@; ===================================================================
@section[#:tag "runtime"]{Runtime}

@defmodule[rackunit/feature/runtime]

Lower-level utilities for step definition matching and execution.

@defstruct*[step-def ([type symbol?]
                      [pattern string?]
                      [handler procedure?])
            #:transparent]{
A step definition binding a @racket[type] (@racket['given],
@racket['when], or @racket['then]), a @racket[pattern] string (possibly
containing @tt{{placeholder}} markers), and a @racket[handler]
procedure.}

@defproc[(compile-step-pattern [pattern string?]) regexp?]{
Converts a step pattern to a compiled @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{regular expression}. Each
@tt{{placeholder}} becomes a @tt{(.+?)} non-greedy capture group, and
the result is anchored with @tt{^} and @tt{$}.

@racketblock[
(compile-step-pattern "I add {a} and {b}")
(code:comment "=> #px\"^I add (.+?) and (.+?)$\"")
]}

@defproc[(run-step [step-defs (listof step-def?)]
                   [type symbol?]
                   [text string?]
                   [ctx hash?]
                   [#:argument argument any/c #f])
         hash?]{
Finds the first @racket[step-def] in @racket[step-defs] whose
@racket[type] matches and whose @racket[pattern] matches @racket[text],
then calls its handler with @racket[ctx], captured values, and
(when non-@racket[#f]) the @racket[argument].

Raises @racket[exn:fail?] if no matching step definition is found.}
