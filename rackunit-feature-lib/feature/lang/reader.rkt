#lang racket/base
(require rackunit/feature/private/lexer
         rackunit/feature/private/parser
         rackunit/feature/private/ast)
(provide read-syntax get-info)

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'feature/lang/color-lexer 'feature-color-lexer)]
      [(drracket:indentation)
       (dynamic-require 'feature/lang/indentation 'feature-indent)]
      [else default])))

(define (read-syntax src port)
  (define source-name (if (path? src) (path->string src) "feature"))
  (define tokens (tokenize port source-name))
  (define doc (parse tokens))
  (define feats (gherkin-document-features doc))
  (define module-name
    (string->symbol (or (and (path? src) (path->string src)) "feature")))
  (datum->syntax
   #f
   `(module ,module-name feature
      (define features ',feats))
   (list src 1 0 1 0)))
