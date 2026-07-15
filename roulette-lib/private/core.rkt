#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require (except-in racket/contract any/c))
(provide
 ;; `wrap-modbeg.rkt`
 (for-syntax
  make-wrapping-module-begin
  make-wrapping-top-interaction)

 ;; `main.rkt`
 (rename-out
  [-define-measurable define-measurable]
  [-define-measurable* define-measurable*])
 (contract-out
  [infer (->i ([val (eng) (measurable-space-point (send (if (unsupplied-arg? eng) default-engine eng) domain))])
              (#:engine [eng (is-a?/c engine<%>)]
               #:path-aware? [path-aware? boolean?]
               #:lazy? [lazy? boolean?]
               #:environment [env (or/c #f hash?)])
              any)])

 ;; `private/measure.rkt`
 (contract-out
  [rename measure-support support (-> measure? any)]
  [rename measure-density density (-> measure? any)]
  [measure/c (-> measurable-space? flat-contract? chaperone-contract?)])

 ;; `private/engine.rkt`
 (contract-out
  [engine/c (-> measurable-space? flat-contract? chaperone-contract?)])

 ;; `private/measurable-space.rkt`
 (contract-out
  [immutable-set/c (-> chaperone-contract? any)]
  [measurable-space-point (-> measurable-space? any)]
  [measurable-space? predicate/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         rosette
         "../engine/rsdd.rkt"
         "engine.rkt"
         "wrap-modbeg.rkt"
         "measure.rkt"
         "measurable-space.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define default-engine (rsdd-engine))

(define-syntax -define-measurable
  (syntax-parser
    [(_ x:id ...+ e:expr)
     #'(begin
         (define m e)
         (define-symbolic x ... (measure-point m))
         (measures-set! x m) ...)]))

(define-syntax -define-measurable*
  (syntax-parser
    [(_ x:id ...+ e:expr)
     #'(begin
         (define m e)
         (define-symbolic* x ... (measure-point m))
         (measures-set! x m) ...)]))

(define (measure-point m)
  (measurable-space-point (measure-domain m)))

(define (infer val
               #:engine [eng default-engine]
               #:path-aware? [path-aware? #f]
               #:lazy? [lazy? #f]
               #:environment [env #f])
  (send eng infer val path-aware? lazy? env))
