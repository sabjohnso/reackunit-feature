#lang racket/base
(require "ast.rkt" "lexer.rkt")
(provide parse)

;; parse : (listof token) -> gherkin-document
;; Recursive descent parser over a flat token list.
(define (parse tokens)
  (define-values (step-files rest) (parse-step-directives tokens))
  (define features (parse-features rest))
  (define srcloc (if (pair? tokens) (token->srcloc (car tokens)) #f))
  (gherkin-document srcloc step-files features))

(define (token->srcloc tok)
  (list (token-source tok) (token-line tok)))

(define (parse-step-directives tokens)
  (let loop ([toks tokens] [files '()])
    (if (and (pair? toks) (eq? (token-type (car toks)) 'steps))
        (loop (cdr toks) (append files (list (token-value (car toks)))))
        (values files toks))))

(define (parse-features tokens)
  (let loop ([toks tokens] [features '()])
    (cond
      [(null? toks) (reverse features)]
      [(eq? (token-type (car toks)) 'feature)
       (define-values (feature rest) (parse-feature toks))
       (loop rest (cons feature features))]
      [else (reverse features)])))

(define (parse-feature tokens)
  (define tok (car tokens))
  (define srcloc (token->srcloc tok))
  (define name (token-value tok))
  (define-values (scenarios rest) (parse-scenarios (cdr tokens)))
  (values (gherkin-feature srcloc name scenarios) rest))

(define (parse-scenarios tokens)
  (let loop ([toks tokens] [scenarios '()])
    (cond
      [(null? toks) (values (reverse scenarios) '())]
      [(eq? (token-type (car toks)) 'scenario)
       (define-values (scenario rest) (parse-scenario toks))
       (loop rest (cons scenario scenarios))]
      [else (values (reverse scenarios) toks)])))

(define (parse-scenario tokens)
  (define tok (car tokens))
  (define srcloc (token->srcloc tok))
  (define name (token-value tok))
  (define-values (steps rest) (parse-steps (cdr tokens)))
  (values (gherkin-scenario srcloc name steps) rest))

(define (step-keyword? type)
  (memq type '(given when then and but)))

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
       (define step (gherkin-step (token->srcloc tok)
                                  resolved-type
                                  (token-value tok)))
       (loop (cdr toks) (cons step steps) resolved-type)]
      [else (values (reverse steps) toks)])))
