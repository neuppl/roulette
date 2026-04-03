#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide check-close)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require rackunit
         roulette)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defns

(define-syntax-rule (check-close ϵ v1 v2)
  (check-true (hash-within ϵ v1 v2)))

(define (hash-within ϵ h1 h2)
  (and (set=? (hash-keys h1) (hash-keys h2))
       (for/and ([k (in-list (hash-keys h1))])
         (< (abs (- (hash-ref h1 k) (hash-ref h2 k))) ϵ))))
