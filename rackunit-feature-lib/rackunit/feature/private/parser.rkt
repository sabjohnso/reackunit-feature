#lang racket/base
(require racket/string "ast.rkt" "lexer.rkt")
(provide parse)

;; parse : (listof token) -> gherkin-document
(define (parse tokens)
  (define srcloc (if (pair? tokens) (token->srcloc (car tokens)) #f))
  (define features (parse-features tokens))
  (gherkin-document srcloc features))

(define (token->srcloc tok)
  (list (token-source tok) (token-line tok)))

;; consume-tags : (listof token) -> (values (listof string) (listof token))
;; Consumes consecutive 'tags tokens, merging into a flat list.
(define (consume-tags tokens)
  (let loop ([toks tokens] [tags '()])
    (cond
      [(and (pair? toks) (eq? (token-type (car toks)) 'tags))
       (loop (cdr toks) (append tags (token-value (car toks))))]
      [else (values tags toks)])))

;; consume-table-rows : (listof token) -> (values (listof (listof string)) (listof token))
(define (consume-table-rows tokens)
  (let loop ([toks tokens] [rows '()])
    (cond
      [(and (pair? toks) (eq? (token-type (car toks)) 'table-row))
       (loop (cdr toks) (cons (token-value (car toks)) rows))]
      [else (values (reverse rows) toks)])))

(define (parse-features tokens)
  (let loop ([toks tokens] [features '()])
    (cond
      [(null? toks) (reverse features)]
      [(or (eq? (token-type (car toks)) 'tags)
           (eq? (token-type (car toks)) 'feature))
       (define-values (tags after-tags) (consume-tags toks))
       (cond
         [(and (pair? after-tags) (eq? (token-type (car after-tags)) 'feature))
          (define-values (feature rest) (parse-feature after-tags tags))
          (loop rest (cons feature features))]
         [else (reverse features)])]
      [else (reverse features)])))

(define (parse-feature tokens tags)
  (define tok (car tokens))
  (define srcloc (token->srcloc tok))
  (define name (token-value tok))
  (define-values (background after-bg) (parse-background (cdr tokens)))
  (define-values (scenarios rest) (parse-scenarios after-bg))
  (values (gherkin-feature srcloc name tags background scenarios) rest))

(define (parse-background tokens)
  (cond
    [(and (pair? tokens) (eq? (token-type (car tokens)) 'background))
     (parse-steps (cdr tokens))]
    [else (values '() tokens)]))

;; scenario-start? : symbol -> boolean
;; Returns true if the token type is a scenario or scenario-outline keyword.
(define (scenario-start? type)
  (memq type '(scenario scenario-outline)))

(define (parse-scenarios tokens)
  (let loop ([toks tokens] [scenarios '()])
    (cond
      [(null? toks) (values (reverse scenarios) '())]
      [(or (eq? (token-type (car toks)) 'tags)
           (scenario-start? (token-type (car toks))))
       (define-values (tags after-tags) (consume-tags toks))
       (cond
         [(and (pair? after-tags)
               (eq? (token-type (car after-tags)) 'scenario))
          (define-values (scenario rest) (parse-scenario after-tags tags))
          (loop rest (cons scenario scenarios))]
         [(and (pair? after-tags)
               (eq? (token-type (car after-tags)) 'scenario-outline))
          (define-values (expanded rest) (parse-scenario-outline after-tags tags))
          (loop rest (append (reverse expanded) scenarios))]
         [else (values (reverse scenarios) toks)])]
      [else (values (reverse scenarios) toks)])))

(define (parse-scenario tokens tags)
  (define tok (car tokens))
  (define srcloc (token->srcloc tok))
  (define name (token-value tok))
  (define-values (steps rest) (parse-steps (cdr tokens)))
  (values (gherkin-scenario srcloc name tags steps) rest))

;; substitute-placeholders : string (listof (cons string string)) -> string
;; Replaces <key> with value from bindings.
(define (substitute-placeholders text bindings)
  (for/fold ([result text]) ([binding (in-list bindings)])
    (string-replace result
                    (string-append "<" (car binding) ">")
                    (cdr binding))))

;; expand-scenario-outline : string srcloc (listof string) (listof gherkin-step) (listof (listof string)) -> (listof gherkin-scenario)
;; Given template name, steps, header row, and data rows, produces N expanded scenarios.
(define (expand-scenario-outline name srcloc tags template-steps headers data-rows)
  (for/list ([row (in-list data-rows)])
    (define bindings (map cons headers row))
    (define param-desc
      (string-join (map (lambda (b) (string-append (car b) "=" (cdr b)))
                        bindings)
                   ", "))
    (define expanded-name (string-append name " (" param-desc ")"))
    (define expanded-steps
      (for/list ([step (in-list template-steps)])
        (gherkin-step (gherkin-step-srcloc step)
                      (gherkin-step-type step)
                      (substitute-placeholders (gherkin-step-text step) bindings)
                      (gherkin-step-argument step))))
    (gherkin-scenario srcloc expanded-name tags expanded-steps)))

;; parse-scenario-outline : (listof token) (listof string) -> (values (listof gherkin-scenario) (listof token))
(define (parse-scenario-outline tokens tags)
  (define tok (car tokens))
  (define srcloc (token->srcloc tok))
  (define name (token-value tok))
  (define-values (template-steps after-steps) (parse-steps (cdr tokens)))
  ;; Consume Examples: keyword
  (cond
    [(and (pair? after-steps)
          (eq? (token-type (car after-steps)) 'examples))
     (define-values (table-rows rest) (consume-table-rows (cdr after-steps)))
     (cond
       [(pair? table-rows)
        (define headers (car table-rows))
        (define data-rows (cdr table-rows))
        (values (expand-scenario-outline name srcloc tags template-steps headers data-rows)
                rest)]
       [else (values '() rest)])]
    [else (values '() after-steps)]))

(define (step-keyword? type)
  (memq type '(given when then and but)))

;; consume-step-argument : (listof token) -> (values argument (listof token))
;; Consumes table-row or doc-string tokens following a step.
(define (consume-step-argument tokens)
  (cond
    [(and (pair? tokens) (eq? (token-type (car tokens)) 'table-row))
     (let loop ([toks tokens] [rows '()])
       (cond
         [(and (pair? toks) (eq? (token-type (car toks)) 'table-row))
          (loop (cdr toks) (cons (token-value (car toks)) rows))]
         [else (values (reverse rows) toks)]))]
    [(and (pair? tokens) (eq? (token-type (car tokens)) 'doc-string))
     (values (token-value (car tokens)) (cdr tokens))]
    [else (values #f tokens)]))

(define (parse-steps tokens)
  (let loop ([toks tokens] [steps '()] [prev-type #f])
    (cond
      [(null? toks) (values (reverse steps) '())]
      [(step-keyword? (token-type (car toks)))
       (define tok (car toks))
       (define raw-type (token-type tok))
       (define resolved-type
         (cond
           [(memq raw-type '(and but)) (or prev-type 'given)]
           [else raw-type]))
       (define-values (argument rest-after-arg)
         (consume-step-argument (cdr toks)))
       (define step (gherkin-step (token->srcloc tok)
                                  resolved-type
                                  (token-value tok)
                                  argument))
       (loop rest-after-arg (cons step steps) resolved-type)]
      [else (values (reverse steps) toks)])))
