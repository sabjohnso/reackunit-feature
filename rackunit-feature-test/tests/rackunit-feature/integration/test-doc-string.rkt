#lang racket/base
(require rackunit rackunit/text-ui racket/string rackunit/feature)

(require "doc-string.feature")

(define stored-doc (box #f))

(define-steps doc-steps
  (given "a document:"
    (lambda (ctx doc)
      (set-box! stored-doc doc)
      (hash-set ctx 'document doc)))
  (then "the document should contain {text}"
    (lambda (ctx text)
      (define doc (hash-ref ctx 'document))
      (check-true (string-contains? doc text)
                  (format "Expected document to contain ~s" text))
      ctx)))

(run-features features #:steps doc-steps)

(define doc-string-tests
  (test-suite
   "integration: doc strings"

   (test-case "doc string is multi-line"
     (check-true (string-contains? (unbox stored-doc) "\n")
                 "Doc string should be multi-line"))

   (test-case "doc string has correct content"
     (check-equal? (unbox stored-doc) "Hello world\nfrom a doc string"))))

(run-tests doc-string-tests)
