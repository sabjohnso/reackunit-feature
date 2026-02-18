#lang racket/base
;; Shim: re-export from the subcollection main.rkt.
;; Needed because rackunit-spec's named collection link
;; prevents subcollection resolution for rackunit/feature.
(require rackunit/feature/main)
(provide (all-from-out rackunit/feature/main))
