#lang racket/base
(require racket/string racket/class)
(provide feature-indent gherkin-indent-line)

;;; Pure indentation logic — no editor dependency.
;;;
;;; gherkin-indent-line : (Natural → (or/c string? #f)) Natural → (or/c exact-nonneg-integer? #f)
;;;
;;; Takes a get-line function (line-number → string-or-#f) and a 0-based
;;; line number. Returns the number of spaces for indentation, or #f
;;; for "no opinion" (e.g., #lang line).

(define (gherkin-indent-line get-line line-num)
  (define raw (get-line line-num))
  (cond
    [(not raw) #f]
    [else
     (define stripped (string-trim raw #:right? #f))
     (cond
       ;; #lang line — no opinion
       [(string-prefix? stripped "#lang ") #f]
       ;; Keyword-based: check what the stripped line starts with
       [(starts-with-feature? stripped) 0]
       [(starts-with-scenario-block? stripped) 2]
       [(starts-with-step? stripped) 4]
       [(starts-with-examples? stripped) 4]
       [(starts-with-pipe? stripped) 6]
       [(starts-with-doc-string-delim? stripped) 6]
       [(starts-with-tag? stripped) (tag-indent get-line line-num)]
       ;; Inside a doc string block → 6
       [(inside-doc-string? get-line line-num) 6]
       ;; Blank, comment, or unknown text → context from previous line
       [else (context-indent get-line line-num)])]))

;; --- Keyword matchers ---

(define (starts-with-feature? s)
  (string-prefix? s "Feature:"))

(define (starts-with-scenario-block? s)
  (or (string-prefix? s "Background:")
      (string-prefix? s "Scenario Outline:")
      (string-prefix? s "Scenario:")))

(define (starts-with-step? s)
  (or (string-prefix? s "Given ")
      (string-prefix? s "When ")
      (string-prefix? s "Then ")
      (string-prefix? s "And ")
      (string-prefix? s "But ")))

(define (starts-with-examples? s)
  (string-prefix? s "Examples:"))

(define (starts-with-pipe? s)
  (string-prefix? s "|"))

(define (starts-with-doc-string-delim? s)
  (string-prefix? s "\"\"\""))

(define (starts-with-tag? s)
  (string-prefix? s "@"))

;; --- Tag indent: 0 if no Feature: above, else 2 ---

(define (tag-indent get-line line-num)
  (let loop ([n (sub1 line-num)])
    (cond
      [(< n 0) 0]
      [else
       (define raw (get-line n))
       (cond
         [(not raw) 0]
         [else
          (define s (string-trim raw #:right? #f))
          (cond
            [(starts-with-feature? s) 2]
            [else (loop (sub1 n))])])])))

;; --- Doc string detection: count """ above current line ---

(define (inside-doc-string? get-line line-num)
  (define count
    (let loop ([n 0] [c 0])
      (cond
        [(>= n line-num) c]
        [else
         (define raw (get-line n))
         (define s (if raw (string-trim raw #:right? #f) ""))
         (if (starts-with-doc-string-delim? s)
             (loop (add1 n) (add1 c))
             (loop (add1 n) c))])))
  (odd? count))

;; --- Context indent: scan backward to determine what follows ---

(define (context-indent get-line line-num)
  (let loop ([n (sub1 line-num)])
    (cond
      [(< n 0) #f]
      [else
       (define raw (get-line n))
       (cond
         [(not raw) #f]
         [else
          (define s (string-trim raw #:right? #f))
          (cond
            ;; Skip blank lines
            [(string=? s "") (loop (sub1 n))]
            ;; Structural lines → return indent for what follows
            [(starts-with-feature? s) 2]
            [(starts-with-scenario-block? s) 4]
            [(starts-with-step? s) 4]
            [(starts-with-examples? s) 6]
            [(starts-with-pipe? s) 6]
            [(starts-with-doc-string-delim? s)
             ;; Count """ from line 0 up to and including line n
             ;; If the count is even, this is a closing """ → next line is 4
             ;; If odd, this is an opening """ → next line is 6
             (define count
               (let count-loop ([i 0] [c 0])
                 (cond
                   [(> i n) c]
                   [else
                    (define r (get-line i))
                    (define t (if r (string-trim r #:right? #f) ""))
                    (if (starts-with-doc-string-delim? t)
                        (count-loop (add1 i) (add1 c))
                        (count-loop (add1 i) c))])))
             (if (even? count) 4 6)]
            [(starts-with-tag? s)
             ;; After a tag, return the same indent as that tag
             (tag-indent get-line n)]
            ;; Comment or unknown → keep scanning
            [(string-prefix? s "#") (loop (sub1 n))]
            ;; Unknown text — return #f
            [else #f])])])))

;; --- DrRacket editor wrapper ---
;;
;; feature-indent : color-textoid<%> exact-nonneg-integer? → (or/c exact-nonneg-integer? #f)
;;
;; Adapts the editor protocol to our pure get-line interface.

(define (feature-indent txt pos)
  (define line-num (send txt position-paragraph pos))
  (define total-lines (add1 (send txt position-paragraph (send txt last-position))))
  (define (get-line n)
    (cond
      [(or (< n 0) (>= n total-lines)) #f]
      [else
       (define start (send txt paragraph-start-position n))
       (define end (send txt paragraph-end-position n))
       (send txt get-text start end)]))
  (gherkin-indent-line get-line line-num))
