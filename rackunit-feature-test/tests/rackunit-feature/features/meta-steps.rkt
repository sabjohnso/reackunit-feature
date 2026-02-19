#lang racket/base
(require rackunit
         rackunit/feature)
(provide meta-steps)

;; Meta-steps: step definitions for feature files that describe
;; rackunit-feature's own behavior.
;;
;; Pattern: Given steps build inner gherkin-feature structs and step-def
;; lists in the outer context. The single When step calls run-features on
;; those inner constructs (with output suppressed). Then steps assert on
;; the results. Boxes bridge the inner/outer execution boundary.

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

;; Accumulate an inner step-def onto the context's 'inner-steps list.
(define (add-inner-step ctx type pattern handler)
  (define existing (hash-ref ctx 'inner-steps '()))
  (hash-set ctx 'inner-steps
            (append existing (list (step-def type pattern handler)))))

;; Build a one-scenario feature from a list of gherkin-steps.
(define (make-single-scenario-feature steps)
  (gherkin-feature #f "Inner"
                   '()
                   '()
                   (list (gherkin-scenario #f "S" '() steps))))

;; Run inner features without rackunit's test framework so that inner
;; failures don't leak into the outer test count. Mirrors the execution
;; logic of run-features but returns (values ok? last-scenario-context).
(define (run-inner-features features
          #:steps [steps '()]
          #:before-all [before-all (lambda (ctx) ctx)]
          #:after-all [after-all (lambda (ctx) ctx)]
          #:before-feature [before-feature (lambda (ctx f) ctx)]
          #:after-feature [after-feature (lambda (ctx f) ctx)]
          #:before-scenario [before-scenario (lambda (ctx sc) ctx)]
          #:after-scenario [after-scenario (lambda (ctx sc) ctx)]
          #:before-step [before-step (lambda (ctx st) ctx)]
          #:after-step [after-step (lambda (ctx st) ctx)])
  (define base-ctx (before-all (hash)))
  (define last-sc-ctx (box #f))
  (define ok
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (for ([feat (in-list features)])
        (define feat-ctx (before-feature base-ctx feat))
        (for ([sc (in-list (gherkin-feature-scenarios feat))])
          (define sc-ctx (before-scenario feat-ctx sc))
          (define all-steps
            (append (gherkin-feature-background feat)
                    (gherkin-scenario-steps sc)))
          (define final-ctx
            (for/fold ([ctx sc-ctx]) ([step (in-list all-steps)])
              (define pre-ctx (before-step ctx step))
              (define post-ctx
                (run-step steps
                          (gherkin-step-type step)
                          (gherkin-step-text step)
                          pre-ctx
                          #:argument (gherkin-step-argument step)))
              (after-step post-ctx step)))
          (set-box! last-sc-ctx final-ctx)
          (after-scenario final-ctx sc)))
      #t))
  (after-all base-ctx)
  (values ok (unbox last-sc-ctx)))

;; ---------------------------------------------------------------------------
;; Step definitions
;; ---------------------------------------------------------------------------

(define-steps meta-steps
  ;; NOTE: More specific "a step definition for..." patterns must appear
  ;; before the generic catch-all, since step matching uses first-match.

  ;; -- Given: inner step that sets a key to a value -------------------------
  (given "a step definition for {type} {pattern} that sets {key} to {value}"
    (lambda (ctx type-str pattern-str key-str value-str)
      (define type (string->symbol type-str))
      (define pattern (substring pattern-str 1 (sub1 (string-length pattern-str))))
      (add-inner-step ctx type pattern
                      (lambda (inner-ctx . captures)
                        (hash-set inner-ctx (string->symbol key-str) value-str)))))

  ;; -- Given: inner step that reads a key into read-box ---------------------
  (given "a step definition for {type} {pattern} that reads {key}"
    (lambda (ctx type-str pattern-str key-str)
      (define type (string->symbol type-str))
      (define pattern (substring pattern-str 1 (sub1 (string-length pattern-str))))
      (define read-box (hash-ref ctx 'read-box (lambda () (box #f))))
      (define ctx* (hash-set ctx 'read-box read-box))
      (add-inner-step ctx* type pattern
                      (lambda (inner-ctx . captures)
                        (set-box! read-box (hash-ref inner-ctx (string->symbol key-str) #f))
                        inner-ctx))))

  ;; -- Given: inner step that captures table argument ---------------------
  (given "a step definition for {type} {pattern} that receives a table"
    (lambda (ctx type-str pattern-str)
      (define type (string->symbol type-str))
      (define pattern (substring pattern-str 1 (sub1 (string-length pattern-str))))
      (define table-box (hash-ref ctx 'table-box (lambda () (box #f))))
      (define ctx* (hash-set ctx 'table-box table-box))
      (add-inner-step ctx* type pattern
                      (lambda (inner-ctx table)
                        (set-box! table-box table)
                        inner-ctx))))

  ;; -- Given: inner step that counts data table rows ----------------------
  (given "a step definition for {type} {pattern} that counts table rows"
    (lambda (ctx type-str pattern-str)
      (define type (string->symbol type-str))
      (define pattern (substring pattern-str 1 (sub1 (string-length pattern-str))))
      (define row-count-box (hash-ref ctx 'row-count-box (lambda () (box #f))))
      (define ctx* (hash-set ctx 'row-count-box row-count-box))
      (add-inner-step ctx* type pattern
                      (lambda (inner-ctx table)
                        (set-box! row-count-box (sub1 (length table)))
                        inner-ctx))))

  ;; -- Given: inner step that captures doc string argument -----------------
  (given "a step definition for {type} {pattern} that receives a doc string"
    (lambda (ctx type-str pattern-str)
      (define type (string->symbol type-str))
      (define pattern (substring pattern-str 1 (sub1 (string-length pattern-str))))
      (define doc-string-box (hash-ref ctx 'doc-string-box (lambda () (box #f))))
      (define ctx* (hash-set ctx 'doc-string-box doc-string-box))
      (add-inner-step ctx* type pattern
                      (lambda (inner-ctx doc)
                        (set-box! doc-string-box doc)
                        inner-ctx))))

  ;; -- Given: inner step that stores doc string content --------------------
  (given "a step definition for {type} {pattern} that stores the doc string"
    (lambda (ctx type-str pattern-str)
      (define type (string->symbol type-str))
      (define pattern (substring pattern-str 1 (sub1 (string-length pattern-str))))
      (define doc-string-box (hash-ref ctx 'doc-string-box (lambda () (box #f))))
      (define ctx* (hash-set ctx 'doc-string-box doc-string-box))
      (add-inner-step ctx* type pattern
                      (lambda (inner-ctx doc)
                        (set-box! doc-string-box doc)
                        inner-ctx))))

  ;; -- Given: register an inner step definition (logs call, captures values) -
  ;; Generic catch-all — must be AFTER more specific patterns above.
  (given "a step definition for {type} {pattern}"
    (lambda (ctx type-str pattern-str)
      (define type (string->symbol type-str))
      (define pattern (substring pattern-str 1 (sub1 (string-length pattern-str))))
      (define log (hash-ref ctx 'call-log (lambda () (box '()))))
      (define cap-box (hash-ref ctx 'capture-box (lambda () (box '()))))
      (define ctx* (hash-set (hash-set ctx 'call-log log) 'capture-box cap-box))
      (add-inner-step ctx* type pattern
                      (lambda (inner-ctx . captures)
                        (set-box! log (append (unbox log) (list pattern)))
                        (set-box! cap-box captures)
                        inner-ctx))))

  ;; -- Given: feature builders ------------------------------------------------
  ;; NOTE: More specific "a feature with..." patterns must appear before the
  ;; generic one-step builder, since step matching uses first-match.

  (given "a feature with a background step {text} and two scenarios"
    (lambda (ctx text-str)
      (define text (substring text-str 1 (sub1 (string-length text-str))))
      (define bg-log (hash-ref ctx 'call-log (lambda () (box '()))))
      (define ctx* (hash-set ctx 'call-log bg-log))
      (define ctx**
        (add-inner-step
         (add-inner-step
          (add-inner-step ctx* 'given text
                          (lambda (ic)
                            (set-box! bg-log (append (unbox bg-log) (list text)))
                            ic))
          'then "scenario A assertion"
          (lambda (ic) ic))
         'then "scenario B assertion"
         (lambda (ic) ic)))
      (hash-set ctx** 'inner-feature
                (gherkin-feature #f "BG Feature" '()
                                 (list (gherkin-step #f 'given text #f))
                                 (list (gherkin-scenario #f "A" '()
                                         (list (gherkin-step #f 'then "scenario A assertion" #f)))
                                       (gherkin-scenario #f "B" '()
                                         (list (gherkin-step #f 'then "scenario B assertion" #f))))))))

  (given "a feature with a scenario named {name}"
    (lambda (ctx name-str)
      (define name (substring name-str 1 (sub1 (string-length name-str))))
      (define ctx*
        (add-inner-step ctx 'given "noop"
                        (lambda (ic) ic)))
      (hash-set ctx* 'inner-feature
                (gherkin-feature #f "Named" '()
                                 '()
                                 (list (gherkin-scenario #f name '()
                                         (list (gherkin-step #f 'given "noop" #f))))))))

  (given "a feature with given {g} and then {t}"
    (lambda (ctx g-str t-str)
      (define g (substring g-str 1 (sub1 (string-length g-str))))
      (define t (substring t-str 1 (sub1 (string-length t-str))))
      (hash-set ctx 'inner-feature
                (make-single-scenario-feature
                 (list (gherkin-step #f 'given g #f)
                       (gherkin-step #f 'then t #f))))))

  (given "a feature with given {g} followed by and {a}"
    (lambda (ctx g-str a-str)
      (define g (substring g-str 1 (sub1 (string-length g-str))))
      (define a (substring a-str 1 (sub1 (string-length a-str))))
      (hash-set ctx 'inner-feature
                (make-single-scenario-feature
                 (list (gherkin-step #f 'given g #f)
                       (gherkin-step #f 'given a #f))))))

  (given "a feature with 3 steps"
    (lambda (ctx)
      (define ctx*
        (add-inner-step
         (add-inner-step
          (add-inner-step ctx 'given "step one"
                          (lambda (ic) ic))
          'when "step two"
          (lambda (ic) ic))
         'then "step three"
         (lambda (ic) ic)))
      (hash-set ctx* 'inner-feature
                (make-single-scenario-feature
                 (list (gherkin-step #f 'given "step one" #f)
                       (gherkin-step #f 'when "step two" #f)
                       (gherkin-step #f 'then "step three" #f))))))

  ;; -- Given: feature with data table argument on a step -------------------
  (given "a feature with a {type} step {text} with a data table"
    (lambda (ctx type-str text-str)
      (define type (string->symbol type-str))
      (define text (substring text-str 1 (sub1 (string-length text-str))))
      (hash-set ctx 'inner-feature
                (make-single-scenario-feature
                 (list (gherkin-step #f type text
                                     '(("name" "age")
                                       ("Alice" "30")
                                       ("Bob" "25"))))))))

  ;; -- Given: feature with specific doc string content --------------------
  (given "a feature with a {type} step {text} with doc string content {content}"
    (lambda (ctx type-str text-str content-str)
      (define type (string->symbol type-str))
      (define text (substring text-str 1 (sub1 (string-length text-str))))
      (define content (substring content-str 1 (sub1 (string-length content-str))))
      (hash-set ctx 'inner-feature
                (make-single-scenario-feature
                 (list (gherkin-step #f type text content))))))

  ;; -- Given: feature with doc string argument on a step ------------------
  (given "a feature with a {type} step {text} with a doc string"
    (lambda (ctx type-str text-str)
      (define type (string->symbol type-str))
      (define text (substring text-str 1 (sub1 (string-length text-str))))
      (hash-set ctx 'inner-feature
                (make-single-scenario-feature
                 (list (gherkin-step #f type text "sample doc string"))))))

  ;; -- Given: tagged feature builder --------------------------------------
  (given "a feature tagged with {tag} containing a scenario"
    (lambda (ctx tag-str)
      (define tag (substring tag-str 1 (sub1 (string-length tag-str))))
      (hash-set ctx 'inner-feature
                (gherkin-feature #f "Tagged Feature"
                                 (list tag) '()
                                 (list (gherkin-scenario #f "S" '()
                                         (list (gherkin-step #f 'given "noop" #f))))))))

  ;; -- Given: feature with tagged scenario --------------------------------
  (given "a feature containing a scenario tagged with {tag}"
    (lambda (ctx tag-str)
      (define tag (substring tag-str 1 (sub1 (string-length tag-str))))
      (hash-set ctx 'inner-feature
                (gherkin-feature #f "Feature" '() '()
                                 (list (gherkin-scenario #f "Tagged Scenario"
                                         (list tag)
                                         (list (gherkin-step #f 'given "noop" #f))))))))

  ;; -- Given: pre-expanded scenario outline (n scenarios) -----------------
  (given "a feature with a scenario outline and {n} example rows"
    (lambda (ctx n-str)
      (define n (string->number n-str))
      (hash-set ctx 'inner-feature
                (gherkin-feature #f "Outline Feature" '() '()
                                 (for/list ([i (in-range n)])
                                   (gherkin-scenario
                                    #f (format "Expanded (~a)" (add1 i)) '()
                                    (list (gherkin-step #f 'given "noop" #f))))))))

  ;; Generic one-step feature builder — must be AFTER specific patterns.
  (given "a feature with a {type} step {text}"
    (lambda (ctx type-str text-str)
      (define type (string->symbol type-str))
      (define text (substring text-str 1 (sub1 (string-length text-str))))
      (hash-set ctx 'inner-feature
                (make-single-scenario-feature
                 (list (gherkin-step #f type text #f))))))

  ;; -- Given: build a two-scenario feature for isolation testing ------------
  (given "a feature where the first scenario sets a key and the second checks it"
    (lambda (ctx)
      (define read-box (hash-ref ctx 'read-box (box 'not-set)))
      (define ctx* (hash-set ctx 'read-box read-box))
      (define ctx**
        (add-inner-step
         (add-inner-step ctx* 'given "set the key"
                         (lambda (inner-ctx)
                           (hash-set inner-ctx 'mykey "hello")))
         'given "check the key"
         (lambda (inner-ctx)
           (set-box! read-box (hash-ref inner-ctx 'mykey 'missing))
           inner-ctx)))
      (hash-set ctx** 'inner-feature
                (gherkin-feature #f "Isolation" '()
                                 '()
                                 (list (gherkin-scenario #f "S1" '()
                                         (list (gherkin-step #f 'given "set the key" #f)))
                                       (gherkin-scenario #f "S2" '()
                                         (list (gherkin-step #f 'given "check the key" #f))))))))

  ;; -- Given: build a feature where background sets env and scenario reads --
  (given "a feature where the background sets env and the scenario reads it"
    (lambda (ctx)
      (define read-box (hash-ref ctx 'read-box (lambda () (box #f))))
      (define ctx* (hash-set ctx 'read-box read-box))
      (define ctx**
        (add-inner-step
         (add-inner-step ctx* 'given "set env"
                         (lambda (ic) (hash-set ic 'env "production")))
         'then "read env"
         (lambda (ic)
           (set-box! read-box (hash-ref ic 'env #f))
           ic)))
      (hash-set ctx** 'inner-feature
                (gherkin-feature #f "BG Context" '()
                                 (list (gherkin-step #f 'given "set env" #f))
                                 (list (gherkin-scenario #f "S" '()
                                         (list (gherkin-step #f 'then "read env" #f))))))))

  ;; -- Given: hook setup steps ----------------------------------------------
  (given "a before-all hook that sets {key} to {value}"
    (lambda (ctx key-str value-str)
      (hash-set ctx 'hook-before-all
                (lambda (ic) (hash-set ic (string->symbol key-str) value-str)))))

  (given "a before-scenario hook that logs the scenario name"
    (lambda (ctx)
      (define hook-log (hash-ref ctx 'hook-log (box '())))
      (hash-set (hash-set ctx 'hook-log hook-log)
                'hook-before-scenario
                (lambda (ic sc)
                  (set-box! hook-log
                            (append (unbox hook-log)
                                    (list (gherkin-scenario-name sc))))
                  ic))))

  (given "an after-scenario hook that captures the {key} key"
    (lambda (ctx key-str)
      (define capture-box (hash-ref ctx 'capture-box (box #f)))
      (hash-set (hash-set ctx 'capture-box capture-box)
                'hook-after-scenario
                (lambda (ic sc)
                  (set-box! capture-box (hash-ref ic (string->symbol key-str) #f))
                  ic))))

  (given "before-step and after-step hooks that count invocations"
    (lambda (ctx)
      (define before-count (box 0))
      (define after-count (box 0))
      (hash-set
       (hash-set
        (hash-set
         (hash-set ctx
                   'count-before-step before-count)
         'count-after-step after-count)
        'hook-before-step
        (lambda (ic st)
          (set-box! before-count (add1 (unbox before-count)))
          ic))
       'hook-after-step
       (lambda (ic st)
         (set-box! after-count (add1 (unbox after-count)))
         ic))))

  (given "a before-step hook that logs each step"
    (lambda (ctx)
      (define step-log (hash-ref ctx 'step-log (box '())))
      (hash-set (hash-set ctx 'step-log step-log)
                'hook-before-step
                (lambda (ic st)
                  (set-box! step-log
                            (append (unbox step-log)
                                    (list (gherkin-step-text st))))
                  ic))))

  ;; -- When: run the inner feature ------------------------------------------
  (when "the feature is run"
    (lambda (ctx)
      (define feat (hash-ref ctx 'inner-feature))
      (define steps (hash-ref ctx 'inner-steps '()))
      (define inner-ctx-box (box #f))
      ;; Read hooks from context, defaulting to identity.
      ;; Wrap defaults in thunks — hash-ref calls the third arg as a
      ;; failure thunk when the key is missing.
      (define ba  (hash-ref ctx 'hook-before-all       (lambda () (lambda (ic) ic))))
      (define bs  (hash-ref ctx 'hook-before-scenario  (lambda () (lambda (ic sc) ic))))
      (define as-user (hash-ref ctx 'hook-after-scenario (lambda () (lambda (ic sc) ic))))
      (define bst (hash-ref ctx 'hook-before-step      (lambda () (lambda (ic st) ic))))
      (define ast (hash-ref ctx 'hook-after-step       (lambda () (lambda (ic st) ic))))
      ;; Compose after-scenario to capture inner context
      (define as-composed
        (lambda (ic sc)
          (define result (as-user ic sc))
          (set-box! inner-ctx-box result)
          result))
      ;; Run inner features directly (bypassing rackunit's test framework
      ;; so inner failures don't leak into the outer test count).
      (define-values (ok inner-ctx-val)
        (run-inner-features (list feat)
                            #:steps steps
                            #:before-all ba
                            #:before-scenario bs
                            #:after-scenario as-composed
                            #:before-step bst
                            #:after-step ast))
      (when (and ok (not (unbox inner-ctx-box)))
        (set-box! inner-ctx-box inner-ctx-val))
      (hash-set (hash-set ctx 'run-ok ok)
                'inner-context (unbox inner-ctx-box))))

  ;; -- Then: assertions -----------------------------------------------------
  (then "the step handler should have been called"
    (lambda (ctx)
      (define log (unbox (hash-ref ctx 'call-log)))
      (check-true (pair? log) "Expected at least one call logged")
      ctx))

  (then "both step handlers should have been called"
    (lambda (ctx)
      (define log (unbox (hash-ref ctx 'call-log)))
      (check-equal? (length log) 2 "Expected exactly two calls logged")
      ctx))

  (then "the captured values should be {a} and {b}"
    (lambda (ctx a-str b-str)
      (define a (substring a-str 1 (sub1 (string-length a-str))))
      (define b (substring b-str 1 (sub1 (string-length b-str))))
      (define captures (unbox (hash-ref ctx 'capture-box)))
      (check-equal? captures (list a b))
      ctx))

  (then "the execution should have failed"
    (lambda (ctx)
      (check-false (hash-ref ctx 'run-ok) "Expected inner run to fail")
      ctx))

  (then "the read value should be {value}"
    (lambda (ctx value-str)
      (define val (unbox (hash-ref ctx 'read-box)))
      (check-equal? val value-str)
      ctx))

  (then "the second scenario should not have seen the key"
    (lambda (ctx)
      (define val (unbox (hash-ref ctx 'read-box)))
      (check-equal? val 'missing "Second scenario should not see first scenario's key")
      ctx))

  (then "the logged scenario name should be {name}"
    (lambda (ctx name-str)
      (define name (substring name-str 1 (sub1 (string-length name-str))))
      (define log (unbox (hash-ref ctx 'hook-log)))
      (check-not-false (member name log)
                      (format "Expected ~a in hook log ~a" name log))
      ctx))

  (then "the captured result should be {value}"
    (lambda (ctx value-str)
      (define val (unbox (hash-ref ctx 'capture-box)))
      (check-equal? val value-str)
      ctx))

  (then "before-step should have fired {n} times"
    (lambda (ctx n-str)
      (check-equal? (unbox (hash-ref ctx 'count-before-step))
                    (string->number n-str))
      ctx))

  (then "after-step should have fired {n} times"
    (lambda (ctx n-str)
      (check-equal? (unbox (hash-ref ctx 'count-after-step))
                    (string->number n-str))
      ctx))

  (then "the log should show {step} before each scenario"
    (lambda (ctx step-str)
      (define step (substring step-str 1 (sub1 (string-length step-str))))
      (define log (unbox (hash-ref ctx 'call-log)))
      ;; The step should appear at least twice (once per scenario)
      (define count (length (filter (lambda (s) (equal? s step)) log)))
      (check-true (>= count 2)
                  (format "Expected ~a to appear >= 2 times in ~a" step log))
      ctx))

  ;; -- Then: data table assertions ----------------------------------------
  (then "the step handler should have received the table"
    (lambda (ctx)
      (define table (unbox (hash-ref ctx 'table-box)))
      (check-not-false table "Expected step handler to receive a table")
      ctx))

  (then "the table row count should be {n}"
    (lambda (ctx n-str)
      (define count (unbox (hash-ref ctx 'row-count-box)))
      (check-equal? count (string->number n-str))
      ctx))

  ;; -- Then: doc string assertions ----------------------------------------
  (then "the step handler should have received the doc string"
    (lambda (ctx)
      (define doc (unbox (hash-ref ctx 'doc-string-box)))
      (check-not-false doc "Expected step handler to receive a doc string")
      ctx))

  (then "the stored doc string should be {text}"
    (lambda (ctx text-str)
      (define text (substring text-str 1 (sub1 (string-length text-str))))
      (define doc (unbox (hash-ref ctx 'doc-string-box)))
      (check-equal? doc text)
      ctx))

  ;; -- Then: tag assertions -----------------------------------------------
  (then "the feature tag should be {tag}"
    (lambda (ctx tag-str)
      (define tag (substring tag-str 1 (sub1 (string-length tag-str))))
      (define feat (hash-ref ctx 'inner-feature))
      (check-not-false (member tag (gherkin-feature-tags feat))
                       (format "Expected tag ~a in ~a" tag (gherkin-feature-tags feat)))
      ctx))

  (then "the scenario tag should be {tag}"
    (lambda (ctx tag-str)
      (define tag (substring tag-str 1 (sub1 (string-length tag-str))))
      (define feat (hash-ref ctx 'inner-feature))
      (define sc (car (gherkin-feature-scenarios feat)))
      (check-not-false (member tag (gherkin-scenario-tags sc))
                       (format "Expected tag ~a in ~a" tag (gherkin-scenario-tags sc)))
      ctx))

  ;; -- Then: call count assertion -----------------------------------------
  (then "the step handler should have been called {n} times"
    (lambda (ctx n-str)
      (define log (unbox (hash-ref ctx 'call-log)))
      (check-equal? (length log) (string->number n-str))
      ctx))
  )
