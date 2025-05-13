#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out measure)
         measure/c
         measures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require data/ddict
         racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defns

(struct measure (procedure support density domain)
  #:property prop:procedure 0)

(define (measure/c dom cod)
  (and/c measure? (-> dom cod)))

;; Using a `ddict` allows a predictable iteration order for constructing
;; expressions over the measure map. This is critical for distributions whose
;; parameters depend on other distributions.
(define measures (mutable-ddict))
