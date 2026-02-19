#lang racket/base
(provide feature-color-lexer)

;; Color lexer for #lang feature (Gherkin syntax).
;; Conforms to syntax-color/lexer-contract lexer/c arity-3:
;;   (port offset mode) -> (values text type paren start end backup new-mode)
;;
;; Modes:
;;   'sol — start of line (can match keywords, tags, comments, doc-string delimiters)
;;   'mid — mid-line (consuming rest of line after a keyword)
;;   'doc — inside a doc string block

;; Section keywords that end with ":"
(define section-keywords
  '("Feature:" "Scenario Outline:" "Scenario:" "Background:" "Examples:"))

;; Step keywords followed by a space (no colon)
(define step-keywords '("Given" "When" "Then" "And" "But"))

(define (feature-color-lexer port offset mode)
  (define-values (line col pos) (port-next-location port))
  (define ch (peek-char port))
  (cond
    [(eof-object? ch)
     (values eof 'eof #f #f #f 0 mode)]

    ;; --- DOC STRING MODE ---
    [(eq? mode 'doc)
     (lex-doc-mode port pos)]

    ;; --- SOL/MID MODES ---
    ;; Newline always resets to sol
    [(char=? ch #\newline)
     (read-char port)
     (define-values (_l _c end-pos) (port-next-location port))
     (values "\n" 'white-space #f pos end-pos 0 'sol)]

    ;; Whitespace (not newline) — consume consecutive whitespace
    [(and (char-whitespace? ch) (not (char=? ch #\newline)))
     (lex-whitespace port pos)]

    ;; SOL-only matches (keywords, tags, comments, doc strings, table pipes)
    [(eq? mode 'sol)
     (cond
       ;; Comment line
       [(char=? ch #\#)
        (lex-comment port pos)]

       ;; Tag
       [(char=? ch #\@)
        (lex-tag port pos)]

       ;; Doc string delimiter
       [(and (char=? ch #\") (doc-string-start? port))
        (lex-doc-string-delimiter port pos 'doc)]

       ;; Table pipe
       [(char=? ch #\|)
        (read-char port)
        (define-values (_l _c end-pos) (port-next-location port))
        (values "|" 'parenthesis #f pos end-pos 0 'sol)]

       ;; Try section keywords, then step keywords
       [(try-section-keyword port pos)
        => (lambda (result) (apply values result))]

       [(try-step-keyword port pos)
        => (lambda (result) (apply values result))]

       ;; Anything else on sol: consume to next pipe or end of line
       [else
        (lex-to-pipe-or-eol port pos)])]

    ;; MID mode — consume rest of line as 'other
    [else
     (lex-rest-of-line port pos 'other 'sol)]))

;; --- Lexer helpers ---

(define (lex-whitespace port start-pos)
  (let loop ()
    (define ch (peek-char port))
    (when (and (char? ch) (char-whitespace? ch) (not (char=? ch #\newline)))
      (read-char port)
      (loop)))
  (define-values (_l _c end-pos) (port-next-location port))
  (values " " 'white-space #f start-pos end-pos 0 'sol))

(define (lex-comment port start-pos)
  (define text (lex-to-eol port))
  (define-values (_l _c end-pos) (port-next-location port))
  (values text 'comment #f start-pos end-pos 0 'sol))

(define (lex-tag port start-pos)
  ;; Read the @word
  (define text
    (let loop ([chars '()])
      (define ch (peek-char port))
      (cond
        [(and (char? ch) (not (char-whitespace? ch)))
         (read-char port)
         (loop (cons ch chars))]
        [else
         (list->string (reverse chars))])))
  (define-values (_l _c end-pos) (port-next-location port))
  (values text 'hash-colon-keyword #f start-pos end-pos 0 'sol))

(define (doc-string-start? port)
  (define s (peek-string 3 0 port))
  (and (string? s) (string=? s "\"\"\"")))

(define (lex-doc-string-delimiter port start-pos new-mode)
  ;; Consume the three quote characters
  (read-char port) (read-char port) (read-char port)
  (define-values (_l _c end-pos) (port-next-location port))
  (values "\"\"\"" 'string #f start-pos end-pos 0 new-mode))

(define (lex-doc-mode port start-pos)
  (define ch (peek-char port))
  (cond
    [(eof-object? ch)
     (values eof 'eof #f #f #f 0 'doc)]
    [(char=? ch #\newline)
     (read-char port)
     (define-values (_l _c end-pos) (port-next-location port))
     (values "\n" 'white-space #f start-pos end-pos 0 'doc)]
    ;; Check for closing """ (after optional whitespace on this line)
    [(doc-string-close-ahead? port)
     (lex-doc-close port start-pos)]
    [else
     ;; Content line inside doc string — consume to end of line
     (define text (lex-to-eol port))
     (define-values (_l _c end-pos) (port-next-location port))
     (values text 'string #f start-pos end-pos 0 'doc)]))

(define (doc-string-close-ahead? port)
  ;; Look ahead: optional whitespace then """
  (let loop ([offset 0])
    (define ch (peek-char port offset))
    (cond
      [(eof-object? ch) #f]
      [(and (char-whitespace? ch) (not (char=? ch #\newline)))
       (loop (add1 offset))]
      [(char=? ch #\")
       (define s (peek-string 3 offset port))
       (and (string? s) (string=? s "\"\"\""))]
      [else #f])))

(define (lex-doc-close port start-pos)
  ;; We're at whitespace before """. Consume whitespace first if any.
  (define ch (peek-char port))
  (cond
    [(and (char? ch) (char-whitespace? ch) (not (char=? ch #\newline)))
     ;; Consume whitespace, return it, next call will get the """
     (lex-whitespace-in-doc port start-pos)]
    [else
     ;; At the """ itself — consume it and switch to sol
     (lex-doc-string-delimiter port start-pos 'sol)]))

(define (lex-whitespace-in-doc port start-pos)
  (let loop ()
    (define ch (peek-char port))
    (when (and (char? ch) (char-whitespace? ch) (not (char=? ch #\newline)))
      (read-char port)
      (loop)))
  (define-values (_l _c end-pos) (port-next-location port))
  (values " " 'white-space #f start-pos end-pos 0 'doc))

(define (try-section-keyword port start-pos)
  (for/or ([kw (in-list section-keywords)])
    (define len (string-length kw))
    (define peeked (peek-string len 0 port))
    (and (string? peeked)
         (string=? peeked kw)
         (begin
           (for ([_ (in-range len)]) (read-char port))
           (let-values ([(_l _c end-pos) (port-next-location port)])
             (list kw 'keyword #f start-pos end-pos 0 'mid))))))

(define (try-step-keyword port start-pos)
  (for/or ([kw (in-list step-keywords)])
    (define len (string-length kw))
    (define peeked (peek-string (add1 len) 0 port))
    (and (string? peeked)
         (= (string-length peeked) (add1 len))
         (string=? (substring peeked 0 len) kw)
         (char-whitespace? (string-ref peeked len))
         (begin
           (for ([_ (in-range len)]) (read-char port))
           (let-values ([(_l _c end-pos) (port-next-location port)])
             (list kw 'keyword #f start-pos end-pos 0 'mid))))))

(define (lex-to-eol port)
  (let loop ([chars '()])
    (define ch (peek-char port))
    (cond
      [(or (eof-object? ch) (char=? ch #\newline))
       (list->string (reverse chars))]
      [else
       (read-char port)
       (loop (cons ch chars))])))

(define (lex-rest-of-line port start-pos type end-mode)
  (define text (lex-to-eol port))
  (define-values (_l _c end-pos) (port-next-location port))
  (values text type #f start-pos end-pos 0 end-mode))

(define (lex-to-pipe-or-eol port start-pos)
  (define text
    (let loop ([chars '()])
      (define ch (peek-char port))
      (cond
        [(or (eof-object? ch) (char=? ch #\newline) (char=? ch #\|))
         (list->string (reverse chars))]
        [else
         (read-char port)
         (loop (cons ch chars))])))
  (define-values (_l _c end-pos) (port-next-location port))
  (values text 'other #f start-pos end-pos 0 'sol))
