#lang racket/base
(require rackunit rackunit/text-ui racket/list rackunit/feature)

(require "data-table.feature")

(define stored-table (box #f))

(define-steps table-steps
  (given "the following users:"
    (lambda (ctx table)
      (set-box! stored-table table)
      (hash-set ctx 'users (cdr table))))   ;; skip header row
  (then "there should be {n} users"
    (lambda (ctx n)
      (check-equal? (length (hash-ref ctx 'users))
                    (string->number n))
      ctx)))

(run-features features #:steps table-steps)

(define data-table-tests
  (test-suite
   "integration: data tables"

   (test-case "header row is accessible"
     (define table (unbox stored-table))
     (check-equal? (car table) '("name" "age")))

   (test-case "data rows contain expected values"
     (define table (unbox stored-table))
     (define data-rows (cdr table))
     (check-equal? (length data-rows) 2)
     (check-equal? (caar data-rows) "Alice")
     (check-equal? (caadr data-rows) "Bob"))))

(run-tests data-table-tests)
