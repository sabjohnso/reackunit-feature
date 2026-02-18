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

;; run-step : (listof step-def) symbol string hash -> hash
;; Finds the first step-def matching type+text, runs it, returns new context.
(define (run-step step-defs type text ctx)
  (let loop ([defs step-defs])
    (cond
      [(null? defs)
       (error 'run-step "no matching step definition for ~a \"~a\"" type text)]
      [else
       (define sd (car defs))
       (if (eq? (step-def-type sd) type)
           (let ([m (regexp-match (compile-step-pattern (step-def-pattern sd)) text)])
             (if m
                 (apply (step-def-handler sd) ctx (cdr m))
                 (loop (cdr defs))))
           (loop (cdr defs)))])))
