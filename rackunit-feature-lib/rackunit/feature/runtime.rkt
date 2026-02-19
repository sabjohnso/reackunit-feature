#lang racket/base
(require racket/string racket/list)
(provide (struct-out step-def)
         compile-step-pattern
         run-step)

(struct step-def (type pattern handler) #:transparent)

;; compile-step-pattern : string -> regexp
;; Converts "{placeholder}" to "(.+?)" capture groups,
;; anchors the pattern for exact matching.
;; Strategy: split on {placeholder} first, escape each literal part,
;; then rejoin with (.+?) capture groups.
(define (compile-step-pattern pat)
  (define parts (regexp-split #rx"\\{[^}]+\\}" pat))
  (define escaped-parts (map regexp-quote parts))
  (define joined (string-join escaped-parts "(.+?)"))
  (pregexp (string-append "^" joined "$")))

;; run-step : (listof step-def) symbol string hash [#:argument any/c] -> hash
;; Finds the first step-def matching type+text, runs it, returns new context.
;; When argument is non-#f it is appended after captures.
(define (run-step step-defs type text ctx #:argument [argument #f])
  (let loop ([defs step-defs])
    (cond
      [(null? defs)
       (error 'run-step "no matching step definition for ~a \"~a\"" type text)]
      [else
       (define sd (car defs))
       (if (eq? (step-def-type sd) type)
           (let ([m (regexp-match (compile-step-pattern (step-def-pattern sd)) text)])
             (if m
                 (let ([captures (cdr m)])
                   (if argument
                       (apply (step-def-handler sd) ctx (append captures (list argument)))
                       (apply (step-def-handler sd) ctx captures)))
                 (loop (cdr defs))))
           (loop (cdr defs)))])))
