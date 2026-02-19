#lang racket/base
(require racket/string)
(provide tokenize
         token token-type token-value token-line token-source)

(struct token (type value line source) #:transparent)

(define keyword-patterns
  `((feature          ,#px"^\\s*Feature:\\s*(.*?)\\s*$")
    (background       ,#px"^\\s*Background:\\s*(.*?)\\s*$")
    (scenario-outline ,#px"^\\s*Scenario Outline:\\s*(.*?)\\s*$")
    (scenario         ,#px"^\\s*Scenario:\\s*(.*?)\\s*$")
    (examples         ,#px"^\\s*Examples:\\s*(.*?)\\s*$")
    (given            ,#px"^\\s*Given\\s+(.*?)\\s*$")
    (when             ,#px"^\\s*When\\s+(.*?)\\s*$")
    (then             ,#px"^\\s*Then\\s+(.*?)\\s*$")
    (and              ,#px"^\\s*And\\s+(.*?)\\s*$")
    (but              ,#px"^\\s*But\\s+(.*?)\\s*$")))

(define (blank-or-comment? line)
  (or (regexp-match? #px"^[\\s\xA0]*$" line)
      (regexp-match? #px"^[\\s\xA0]*#" line)))

(define (table-row-line? line)
  (regexp-match? #px"^\\s*\\|" line))

(define (parse-table-row line)
  (define stripped (string-trim line))
  ;; Remove leading and trailing pipes, split by |, trim each cell
  (define inner
    (let ([s stripped])
      (define s1 (if (string-prefix? s "|") (substring s 1) s))
      (define s2 (if (string-suffix? s1 "|")
                     (substring s1 0 (sub1 (string-length s1)))
                     s1))
      s2))
  (map string-trim (string-split inner "|")))

(define (tags-line? line)
  (regexp-match? #px"^\\s*@" line))

(define (parse-tags line)
  (map bytes->string/utf-8
       (regexp-match* #px"@[\\w-]+" (string->bytes/utf-8 line))))

(define (tokenize-line line line-number source)
  (cond
    [(blank-or-comment? line) #f]
    [(table-row-line? line)
     (token 'table-row (parse-table-row line) line-number source)]
    [(tags-line? line)
     (token 'tags (parse-tags line) line-number source)]
    [else
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
              (loop (cdr patterns)))]))]))

(define (doc-string-delimiter? line)
  (regexp-match? #px"^\\s*\"\"\"\\s*$" line))

(define (line-indent line)
  (let ([m (regexp-match #px"^(\\s*)" line)])
    (if m (string-length (cadr m)) 0)))

(define (strip-indent line n)
  (cond
    [(= (string-length line) 0) line]
    [(<= (string-length line) n) ""]
    [else (substring line n)]))

(define (tokenize port source)
  (let loop ([line-number 1]
             [tokens '()]
             ;; Doc-string accumulation state
             [in-doc-string #f]     ;; line number of opening """
             [doc-indent 0]         ;; indent of opening """
             [doc-lines '()])       ;; accumulated content lines (reversed)
    (define line (read-line port 'any))
    (cond
      [(eof-object? line)
       (when in-doc-string
         (error 'tokenize "unterminated doc string starting at ~a:~a"
                source in-doc-string))
       (reverse tokens)]
      [in-doc-string
       (cond
         [(doc-string-delimiter? line)
          ;; Close doc string: strip indent and join
          (define content
            (string-join
             (map (lambda (l) (strip-indent l doc-indent))
                  (reverse doc-lines))
             "\n"))
          (loop (add1 line-number)
                (cons (token 'doc-string content in-doc-string source)
                      tokens)
                #f 0 '())]
         [else
          (loop (add1 line-number) tokens
                in-doc-string doc-indent (cons line doc-lines))])]
      [(doc-string-delimiter? line)
       ;; Opening doc string delimiter
       (loop (add1 line-number) tokens
             line-number (line-indent line) '())]
      [else
       (let ([tok (tokenize-line line line-number source)])
         (loop (add1 line-number)
               (if tok (cons tok tokens) tokens)
               #f 0 '()))])))
