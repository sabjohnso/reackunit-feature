#lang racket/base
(require rackunit rackunit/text-ui racket/list racket/port racket/string)
(require feature/lang/color-lexer)

;; Helper: collect all tokens from a string by calling the arity-3 lexer
;; repeatedly until EOF. Returns a list of (list text type paren start end mode).
(define (lex-all str)
  (define port (open-input-string str))
  (port-count-lines! port)
  (let loop ([mode 'sol] [acc '()])
    (define-values (txt type paren start end backup new-mode)
      (feature-color-lexer port 0 mode))
    (if (equal? type 'eof)
        (reverse acc)
        (loop new-mode
              (cons (list txt type paren start end new-mode) acc)))))

;; Helper: extract just the types from a full lex
(define (lex-types str)
  (map second (lex-all str)))

;; Helper: extract (text type) pairs
(define (lex-text+type str)
  (map (lambda (t) (list (first t) (second t))) (lex-all str)))

(define color-lexer-tests
  (test-suite
   "Color Lexer"

   (test-suite
    "keywords at start of line"

    (test-case "Feature: is a keyword"
      (define tokens (lex-all "Feature: Calculator\n"))
      (check-equal? (second (first tokens)) 'keyword)
      (check-equal? (first (first tokens)) "Feature:"))

    (test-case "Scenario: is a keyword"
      (define tokens (lex-all "  Scenario: Addition\n"))
      ;; whitespace first, then keyword
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "Scenario:"))

    (test-case "Background: is a keyword"
      (define tokens (lex-all "  Background:\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "Background:"))

    (test-case "Scenario Outline: is a keyword"
      (define tokens (lex-all "  Scenario Outline: Pricing\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "Scenario Outline:"))

    (test-case "Examples: is a keyword"
      (define tokens (lex-all "    Examples:\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "Examples:"))

    (test-case "Given is a keyword"
      (define tokens (lex-all "    Given a calculator\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "Given"))

    (test-case "When is a keyword"
      (define tokens (lex-all "    When I add 2 and 3\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "When"))

    (test-case "Then is a keyword"
      (define tokens (lex-all "    Then the result is 5\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "Then"))

    (test-case "And is a keyword"
      (define tokens (lex-all "    And another thing\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "And"))

    (test-case "But is a keyword"
      (define tokens (lex-all "    But not this\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (length kw-tokens) 1)
      (check-equal? (first (car kw-tokens)) "But")))

   (test-suite
    "Scenario Outline: before Scenario: in matching"

    (test-case "Scenario Outline is not misclassified as Scenario"
      (define tokens (lex-all "  Scenario Outline: Template\n"))
      (define kw-tokens (filter (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (first (car kw-tokens)) "Scenario Outline:")))

   (test-suite
    "step text after keywords"

    (test-case "text after step keyword is 'other"
      (define tokens (lex-all "    Given a calculator\n"))
      (define other-tokens (filter (lambda (t) (equal? (second t) 'other)) tokens))
      (check-true (> (length other-tokens) 0))
      ;; The step text portion should contain "a calculator"
      (check-true
       (ormap (lambda (t) (string-contains? (first t) "a calculator"))
              other-tokens)))

    (test-case "text after Feature: is 'other"
      (define tokens (lex-all "Feature: Calculator\n"))
      (define other-tokens (filter (lambda (t) (equal? (second t) 'other)) tokens))
      (check-true (> (length other-tokens) 0))))

   (test-suite
    "tags"

    (test-case "single tag produces hash-colon-keyword"
      (define tokens (lex-all "@smoke\n"))
      (define tag-tokens (filter (lambda (t) (equal? (second t) 'hash-colon-keyword)) tokens))
      (check-equal? (length tag-tokens) 1)
      (check-equal? (first (car tag-tokens)) "@smoke"))

    (test-case "multiple tags on one line"
      (define tokens (lex-all "@smoke @fast @wip\n"))
      (define tag-tokens (filter (lambda (t) (equal? (second t) 'hash-colon-keyword)) tokens))
      (check-equal? (length tag-tokens) 3)))

   (test-suite
    "comments"

    (test-case "comment line produces 'comment token"
      (define tokens (lex-all "# this is a comment\n"))
      (define comment-tokens (filter (lambda (t) (equal? (second t) 'comment)) tokens))
      (check-equal? (length comment-tokens) 1)
      (check-true (string-contains? (first (car comment-tokens)) "# this is a comment")))

    (test-case "indented comment"
      (define tokens (lex-all "  # indented comment\n"))
      (define comment-tokens (filter (lambda (t) (equal? (second t) 'comment)) tokens))
      (check-equal? (length comment-tokens) 1)))

   (test-suite
    "doc strings"

    (test-case "doc string delimiters and body are 'string tokens"
      (define tokens (lex-all "      \"\"\"\n      Hello world\n      \"\"\"\n"))
      (define string-tokens (filter (lambda (t) (equal? (second t) 'string)) tokens))
      (check-true (>= (length string-tokens) 1)))

    (test-case "doc string mode transitions"
      ;; After opening \"\"\" we enter 'doc mode
      (define tokens (lex-all "      \"\"\"\n      content\n      \"\"\"\n"))
      ;; The content inside doc string should be 'string
      (define string-tokens (filter (lambda (t) (equal? (second t) 'string)) tokens))
      (check-true (>= (length string-tokens) 2)))

    (test-case "doc string with multiple lines"
      (define tokens (lex-all "    \"\"\"\n    line one\n    line two\n    \"\"\"\n"))
      (define string-tokens (filter (lambda (t) (equal? (second t) 'string)) tokens))
      ;; Opening delimiter, two content lines, closing delimiter
      (check-true (>= (length string-tokens) 3))))

   (test-suite
    "table pipes"

    (test-case "pipe characters produce 'parenthesis tokens"
      (define tokens (lex-all "      | name | age |\n"))
      (define paren-tokens (filter (lambda (t) (equal? (second t) 'parenthesis)) tokens))
      (check-true (>= (length paren-tokens) 1)))

    (test-case "all pipes in a table row are individually colored"
      (define tokens (lex-all "      | name | age |\n"))
      (define paren-tokens (filter (lambda (t) (equal? (second t) 'parenthesis)) tokens))
      (check-equal? (length paren-tokens) 3))

    (test-case "table cell text is 'other"
      (define tokens (lex-all "      | Alice | 30 |\n"))
      (define other-tokens (filter (lambda (t) (equal? (second t) 'other)) tokens))
      (check-true (>= (length other-tokens) 1))))

   (test-suite
    "whitespace"

    (test-case "leading whitespace produces 'white-space"
      (define tokens (lex-all "    Given x\n"))
      (define ws-tokens (filter (lambda (t) (equal? (second t) 'white-space)) tokens))
      (check-true (>= (length ws-tokens) 1)))

    (test-case "blank line produces 'white-space"
      (define tokens (lex-all "\n"))
      (define ws-tokens (filter (lambda (t) (equal? (second t) 'white-space)) tokens))
      (check-true (>= (length ws-tokens) 1)))

    (test-case "empty line with spaces"
      (define tokens (lex-all "   \n"))
      (check-true (andmap (lambda (t) (equal? (second t) 'white-space)) tokens))))

   (test-suite
    "mode transitions"

    (test-case "start-of-line mode transitions to mid-line after keyword"
      (define tokens (lex-all "Feature: X\n"))
      ;; After "Feature:" keyword, mode should be 'mid for the rest text
      (define kw-tok (findf (lambda (t) (equal? (second t) 'keyword)) tokens))
      (check-equal? (sixth kw-tok) 'mid))

    (test-case "newline resets mode to sol"
      (define tokens (lex-all "Feature: X\n"))
      (define last-tok (last tokens))
      (check-equal? (sixth last-tok) 'sol))

    (test-case "doc string opening transitions to doc mode"
      (define tokens (lex-all "    \"\"\"\n"))
      (define string-tok (findf (lambda (t) (equal? (second t) 'string)) tokens))
      (check-equal? (sixth string-tok) 'doc))

    (test-case "doc string closing transitions to sol mode"
      (define tokens (lex-all "    \"\"\"\n    content\n    \"\"\"\n"))
      (define last-tok (last tokens))
      (check-equal? (sixth last-tok) 'sol)))

   (test-suite
    "position tracking"

    (test-case "token positions are positive integers"
      (define tokens (lex-all "Feature: Calculator\n"))
      (for ([tok (in-list tokens)])
        (check-true (exact-positive-integer? (fourth tok))
                    (format "start position of ~a" tok))
        (check-true (exact-positive-integer? (fifth tok))
                    (format "end position of ~a" tok))))

    (test-case "positions are monotonically increasing"
      (define tokens (lex-all "Feature: Calculator\n  Scenario: Addition\n"))
      (for ([tok (in-list tokens)]
            [next (in-list (cdr tokens))])
        (check-true (<= (fifth tok) (fourth next))
                    (format "~a end <= ~a start" tok next))))

    (test-case "positions cover entire input"
      (define input "Given x\n")
      (define tokens (lex-all input))
      (when (pair? tokens)
        (check-equal? (fourth (first tokens)) 1)
        (check-equal? (fifth (last tokens)) (+ 1 (string-length input))))))

   (test-suite
    "EOF handling"

    (test-case "empty input produces EOF immediately"
      (define port (open-input-string ""))
      (port-count-lines! port)
      (define-values (txt type paren start end backup new-mode)
        (feature-color-lexer port 0 'sol))
      (check-equal? type 'eof))

    (test-case "lexer returns EOF after all tokens consumed"
      (define port (open-input-string "Feature: X\n"))
      (port-count-lines! port)
      ;; Consume all tokens
      (let loop ([mode 'sol])
        (define-values (txt type paren start end backup new-mode)
          (feature-color-lexer port 0 mode))
        (unless (equal? type 'eof)
          (loop new-mode)))
      ;; Next call should also be EOF
      (define-values (txt type paren start end backup new-mode)
        (feature-color-lexer port 0 'sol))
      (check-equal? type 'eof)))

   (test-suite
    "complete feature file"

    (test-case "multi-line feature tokenizes with correct types"
      (define input
        (string-append "@smoke\n"
                       "Feature: Calculator\n"
                       "  Background:\n"
                       "    Given a calculator\n"
                       "\n"
                       "  Scenario: Addition\n"
                       "    When I add 2 and 3\n"
                       "    Then the result is 5\n"))
      (define tokens (lex-all input))
      (define types (map second tokens))
      (check-not-false (member 'keyword types))
      (check-not-false (member 'hash-colon-keyword types))
      (check-not-false (member 'other types))
      (check-not-false (member 'white-space types))
      (check-false (member 'comment types))
      (check-false (member 'string types)))

    (test-case "feature with doc string and table"
      (define input
        (string-append "Feature: Mixed\n"
                       "  Scenario: Doc and table\n"
                       "    Given a document:\n"
                       "      \"\"\"\n"
                       "      Hello world\n"
                       "      \"\"\"\n"
                       "    And the following users:\n"
                       "      | name  | age |\n"
                       "      | Alice | 30  |\n"))
      (define tokens (lex-all input))
      (define types (map second tokens))
      (check-not-false (member 'keyword types))
      (check-not-false (member 'string types))
      (check-not-false (member 'parenthesis types))
      (check-not-false (member 'other types)))

    (test-case "feature with comments"
      (define input
        (string-append "# This is a comment\n"
                       "Feature: Commented\n"
                       "  # Another comment\n"
                       "  Scenario: Test\n"
                       "    Given something\n"))
      (define tokens (lex-all input))
      (define types (map second tokens))
      (check-not-false (member 'comment types))
      (check-not-false (member 'keyword types))))))

(run-tests color-lexer-tests)
