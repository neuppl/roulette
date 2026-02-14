#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require (except-in racket/contract ->))
(provide
 (contract-out
  [bdd-engine (->* () engine?)]
  [bernoulli-measure (->* (number? number?) () measure?)])
  bdd-num-recursive-calls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rosette/base/core/reflect symbolics)
         racket/set
         racket/match
         rosette/base/core/bool
         rosette/base/core/term
         data/ddict
         "../private/engine.rkt"
         "../private/log.rkt"
         "../private/measure.rkt"
         "../private/measurable-space.rkt"
         "../private/util.rkt"
         "../../profiling/bdd.rkt")
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define bdd-true true-ptr)
(define bdd-false false-ptr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; engine

(define (bdd-engine)
  (engine (make-infer) (immutable-set/c any/c)))

(define (bernoulli-measure f t)

  ; Given a set of values, returns the sum of total probability of all values. 
  (define (proc st)
    (+ (if (set-member? st #f) f 0)
       (if (set-member? st #t) t 0)))

  ; Probability of value. 
  (define (density val)
    (if val t f))

  ; Set of all possible values in distribution. 
  (define support
    (for/set ([val '(#f #t)]
              #:unless (equal? (density val) 0))
      val))

  (measure proc support density (immutable-set/c @boolean?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding

; Produces a BDD pointer. 
(define (enc v)
  (match v
    [(? expression?) (enc-expr v)]
    [(? constant?)   (enc-const v)]
    [_               (enc-lit v)]))

(define (enc-expr v)
  (match v
    ;; Recognize `ite` shape and use BDD `ite` operation
    [(or (expression (== @||)
                     (expression (== @&&) (expression (== @!) g) e1)
                     (expression (== @&&) g e2))
         (expression (== @||)
                     (expression (== @&&) g e2)
                     (expression (== @&&) (expression (== @!) g) e1)))
     (bdd-ite (enc g) (enc e2) (enc e1))]
    [(expression (app bdd-encoder (? procedure? $op)) es ...)
     (apply $op (map enc es))]
    [_ (error 'enc "cannot encode ~a" v)]))

(define-encoder bdd-encoder
  [@! bdd-not]
  [@&& (lift-arity bdd-and)]
  [@|| (lift-arity bdd-or)]
  [@=> bdd-implies]
[@<=> bdd-iff])

(define (bdd-implies x y)
  (bdd-or (bdd-not x) y))

(define (bdd-iff x y)
  (bdd-ite x y (bdd-not y)))

(define/cache (enc-const v)
  (define ptr (fresh-var #t))
  ;; Set weight immediately if this constant has a measure
  (define measure (ddict-ref measures v #f))
  (when measure
    (set-var-weight! ptr (measure (set #f)) (measure (set #t))))
  ptr)

(define (enc-lit v)
  (match v
    [#t bdd-true]
    [#f bdd-false]
    [(? number?) (hash v bdd-true)]
    [_ (error 'enc "expected a boolean?, or number?, given ~a" v)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; infer
(define (make-infer) (λ (val lazy?)
  (define vars (list->set (symbolics val)))

  ;; Use `in-ddict-reverse` for "program order" as the variable order.
  (for ([(var measure) (in-ddict-reverse measures)]
        #:when (set-member? vars var))
    (define temp (enc-const var))
    (set-var-weight! temp (measure (set #f)) (measure (set #t))))

  ;; Compute measure
  (define ht (flatten-symbolic val))

  ;; Encode all expressions up front to ensure all BDD variables are created
  ;; and have their weights set before we call wmc
  (for ([(_ expr) (in-hash ht)])
    (enc expr))

  (define (procedure elems)
    (for/fold ([acc 0])
              ([elem (in-set elems)])
      (+ acc (density elem))))
  (define (density val)
    (begin0
      (if (hash-has-key? ht val)
          (wmc (enc/log! (hash-ref ht val)))
          0)
      #;(displayln "after enc/log!" (current-error-port))))
  (define support
    (list->set
      (if lazy?
          (hash-keys ht)
          (filter (compose not zero? density) (hash-keys ht)))))
  (measure procedure support density (immutable-set/c any/c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; size

(define (enc/log! expr)
  (define bdd (enc expr))
  (begin0
    bdd
    (log-roulette-info "~a total size" (cached-size bdd))
    (log-roulette-info "~a recursive calls" (bdd-num-recursive-calls))))

(define (bdd-const? ptr) (or (equal? bdd-true ptr)
                             (equal? bdd-false ptr)))

(define (cached-size bdd)
  0
  #;(cond
    [(or (bdd-const? bdd) (= 1 (bdd-scratch bdd 0))) 0]
    [else
     (rsdd-set-scratch! bdd 1)
     (+ 1 (cached-size (rsdd-low bdd)) (cached-size (rsdd-high bdd)))]))