#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [make-pmf pmf])
         pmf?
         pmf-support
         in-pmf
         for/pmf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/format
         racket/list
         racket/match
         racket/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define ARROW "↦")
(define SEP "|")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probability mass function

(struct pmf (hash)
  #:property prop:procedure
  (λ (self value) (hash-ref (pmf-hash self) value 0))
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) 'pmf)
      (λ (self)
        (match-define (pmf ht) self)
        (define mappings
          (for/list ([(k v) (in-hash ht)])
            (list k (unquoted-printing-string ARROW) (unquoted-printing-string (~r v)))))
        (define sep-unq (unquoted-printing-string SEP))
        (define separated-mappings (add-between mappings (list sep-unq)))
        (cons sep-unq (apply append separated-mappings)))))])

(define (make-pmf ht)
  (for/pmf ([(value measure) (in-hash ht)]
            #:when (not (zero? measure)))
    (values value measure)))

(define (pmf-support pmf)
  (hash-keys (pmf-hash pmf)))

(define (in-pmf pmf)
  (in-hash (pmf-hash pmf)))

(define-syntax-rule (for/pmf (for-clause ...) body-or-break ... body)
  (pmf (for/hash (for-clause ...) body-or-break ... body)))
