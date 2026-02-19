#lang scribble/manual
@(require "rackunit-feature/util.rkt")

@title{rackunit-feature: Gherkin BDD Testing}
@author{sbj}
@defmodule[rackunit/feature]

The @racketmodname[rackunit/feature] library brings
@hyperlink["https://cucumber.io/docs/gherkin/"]{Gherkin}-style
Behavior-Driven Development to Racket. Write feature specifications in
plain-language @tt{.feature} files using @tt{#lang feature},
define step implementations in Racket, and run everything through
@racketmodname[rackunit].

@table-of-contents[]

@include-section["rackunit-feature/guide.scrbl"]
@include-section["rackunit-feature/reference.scrbl"]
