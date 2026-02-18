#lang racket/base
(require rackunit-feature/private/lexer
         rackunit-feature/private/parser
         rackunit-feature/private/compiler)
(provide read-syntax)

(define (read-syntax src port)
  (define tokens (tokenize port (if (path? src) (path->string src) "feature")))
  (define ast (parse tokens))
  (define ir (compile-document ast))
  (define module-name
    (string->symbol
     (or (and (path? src) (path->string src)) "feature")))
  (datum->syntax
   #f
   `(module ,module-name rackunit-feature ,ir)
   (list src 1 0 1 0)))
