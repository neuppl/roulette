#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out measurable-space)
         (rename-out [make-immutable-set/c immutable-set/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/match
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(struct measurable-space (point))

(struct immutable-set/c measurable-space (elem/c)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name
   (λ (self)
     (match-define (immutable-set/c _ elem/c) self)
     `(immutable-set/c ,(contract-name elem/c)))
   #:late-neg-projection
   (λ (self)
     (match-define (immutable-set/c _ elem/c) self)
     (get/build-late-neg-projection
      (set/c elem/c #:kind 'immutable)))))

(define (make-immutable-set/c elem/c)
  (immutable-set/c elem/c (coerce-contract/f elem/c)))
