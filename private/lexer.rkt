#lang racket/base
(require racket/string)
(provide tokenize
         token token-type token-value token-line token-source)

(struct token (type value line source) #:transparent)

(define keyword-patterns
  `((steps     ,#px"^\\s*Steps:\\s*\"([^\"]+)\"")
    (feature   ,#px"^\\s*Feature:\\s*(.*?)\\s*$")
    (scenario  ,#px"^\\s*Scenario:\\s*(.*?)\\s*$")
    (given     ,#px"^\\s*Given\\s+(.*?)\\s*$")
    (when      ,#px"^\\s*When\\s+(.*?)\\s*$")
    (then      ,#px"^\\s*Then\\s+(.*?)\\s*$")
    (and       ,#px"^\\s*And\\s+(.*?)\\s*$")
    (but       ,#px"^\\s*But\\s+(.*?)\\s*$")))

(define (blank-or-comment? line)
  (or (regexp-match? #px"^\\s*$" line)
      (regexp-match? #px"^\\s*#" line)))

(define (tokenize-line line line-number source)
  (if (blank-or-comment? line)
      #f
      (let loop ([patterns keyword-patterns])
        (cond
          [(null? patterns)
           (error 'tokenize "unrecognized line at ~a:~a: ~a"
                  source line-number line)]
          [else
           (define pat (car patterns))
           (define m (regexp-match (cadr pat) line))
           (if m
               (token (car pat) (cadr m) line-number source)
               (loop (cdr patterns)))]))))

(define (tokenize port source)
  (let loop ([line-number 1] [tokens '()])
    (define line (read-line port 'any))
    (if (eof-object? line)
        (reverse tokens)
        (let ([tok (tokenize-line line line-number source)])
          (loop (add1 line-number)
                (if tok (cons tok tokens) tokens))))))
