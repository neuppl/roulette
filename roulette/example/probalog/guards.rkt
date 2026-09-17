#lang racket/base

;; The guard operations that `hash-set.rkt` builds sym-sets out of.
(provide guard-true guard-false
         guard-var guard-and guard-or guard-not
         guard-implies guard-iff
         guard-equiv? guard-prob
         guard-known-true? guard-known-false?
         reset-guards! add-reset-hook! guard-stats)

(require roulette/engine/rsdd
         data/gvector
         ffi/unsafe/custodian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The manager
;;

(define builder #f)
(define weights #f)      ; label -> (cons false-weight true-weight)
(define cache #f)        ; scratch cells allocated by wmc
(define ops 0)           ; every guard operation, for guard-stats

;; Run after each reset, so state holding guards (accumulated evidence,
;; caches) can drop what is now meaningless. Without this a stale guard
;; outlives its manager and the next operation on it faults in Rust
;; rather than raising.
(define reset-hooks '())
(define (add-reset-hook! thunk) (set! reset-hooks (cons thunk reset-hooks)))

(define (reset-guards!)
  (set! builder (mk-bdd-manager-default-order 0))
  (register-finalizer-and-custodian-shutdown builder free-bdd-manager)
  ;; labels come out sequentially from 0, so appending per variable
  ;; keeps gvector index and label in step
  (set! weights (make-gvector))
  (set! cache (box '()))
  (register-finalizer-and-custodian-shutdown cache free-weight-cache)
  (set! ops 0)
  ;; after the new manager is in place, so hooks may build guards
  (for-each (lambda (t) (t)) reset-hooks))

(reset-guards!)

(define (op!) (set! ops (add1 ops)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations

(define (guard-true)  (make-rsdd-true builder))
(define (guard-false) (make-rsdd-false builder))

;; A fresh variable holding with probability p, independent of every
;; other. p of 0 or 1 is a constant, not a variable, so an unannotated
;; program allocates none.
(define (guard-var p)
  (cond
    [(= p 0) (guard-false)]
    [(= p 1) (guard-true)]
    [else
     (define l (rsdd-label builder))
     (gvector-add! weights (cons (- 1 p) p))
     (rsdd-var builder l)]))

(define (guard-and a b) (op!) (rsdd-and builder a b))
(define (guard-or a b)  (op!) (rsdd-or builder a b))
(define (guard-not a)   (op!) (rsdd-not builder a))

(define (guard-implies a b) (guard-or (guard-not a) b))
(define (guard-iff a b)
  (guard-and (guard-implies a b) (guard-implies b a)))

;; Logical equivalence: a comparison, not a search.
(define (guard-equiv? a b) (op!) (rsdd-equal? builder a b))

(define (guard-known-true? g)  (rsdd-true? g))
(define (guard-known-false? g) (rsdd-false? g))

;; Unnormalised probability that a guard holds.
;;
;; SHARP EDGE: `wmc` memoises in each node's scratch cell and never
;; invalidates, so results hold only while `weights` is unchanged. True
;; here: conditioning conjoins evidence rather than reweighting.
(define (guard-prob g) (wmc g weights cache real-semiring))

(define (guard-stats)
  (list (cons 'guard-ops ops)
        (cons 'apply-calls (rsdd-num-recursive-calls builder))
        (cons 'variables (gvector-count weights))))
