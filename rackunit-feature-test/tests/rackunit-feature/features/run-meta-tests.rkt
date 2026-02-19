#lang racket/base
(require rackunit/feature)
(require "meta-steps.rkt")
(require (prefix-in sm: "step-matching.feature"))
(require (prefix-in ct: "context-threading.feature"))
(require (prefix-in bg: "background-steps.feature"))
(require (prefix-in hk: "hooks.feature"))
(require (prefix-in dt: "data-tables.feature"))
(require (prefix-in ds: "doc-strings.feature"))
(require (prefix-in tg: "tags.feature"))
(require (prefix-in so: "scenario-outlines.feature"))

(run-features
 (append sm:features ct:features bg:features hk:features
         dt:features ds:features tg:features so:features)
 #:steps meta-steps)
