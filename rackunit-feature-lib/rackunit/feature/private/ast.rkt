#lang racket/base
(provide (struct-out gherkin-step)
         (struct-out gherkin-scenario)
         (struct-out gherkin-feature)
         (struct-out gherkin-document))

(struct gherkin-step     (srcloc type text)     #:prefab)
(struct gherkin-scenario (srcloc name steps)     #:prefab)
(struct gherkin-feature  (srcloc name scenarios) #:prefab)
(struct gherkin-document (srcloc features)       #:prefab)
