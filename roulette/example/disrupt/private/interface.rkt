#lang roulette

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 ;; `disrupt.rkt`
 (rename-out
  [module-begin #%module-begin]
  [top-interaction #%top-interaction])

 ;; operations
 flip
 query

 observe!
 with-observe

 sample
 with-sample

 ;; profiling
 cost

 ;; debug
 clear-cache!
 recursive-calls
 size

 ;; `pmf.rkt`
 pmf
 pmf?
 pmf-support
 in-pmf
 for/pmf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         roulette/engine/rsdd
         text-table
         "pmf.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(gc-terms!)
(define engine (rsdd-engine))
(define o-evidence #t)
(define s-evidence #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic features

(struct unreachable ())

(define (flip pr)
  (cond
    [(= pr 0) #f]
    [(= pr 1) #t]
    [else
     (for/all ([pr pr])
       (define-measurable* x (bernoulli-measure (- 1 pr) pr))
       x)]))

(define (query e
               #:evidence [evidence (and o-evidence s-evidence)]
               #:environment [env #f])
  (define ⊥ (unreachable))
  (define unnormalized
    (infer (if evidence e ⊥)
           #:engine engine
           #:path-aware? #t
           #:lazy? #f
           #:environment env))
  (define prob (density unnormalized))
  (define normalizer
    (for/sum ([value (in-set (support unnormalized))]
              #:unless (unreachable? value))
      (prob value)))
  (and (positive? normalizer)
       (for/pmf ([value (in-set (support unnormalized))]
                 #:unless (unreachable? value)
                 #:do [(define weight (prob value))]
                 #:unless (zero? weight))
         (values value (/ weight normalizer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sampling

(define (sample e)
  (define ht (pmf-hash (query e)))
  (define result (hash-sample ht))
  (define pr (hash-ref ht result))
  (when o-evidence
    (define-measurable* γ (bernoulli-measure 1 (/ 1 pr)))
    (set! s-evidence (&& (equal? e result) γ s-evidence)))
  result)

(define-syntax with-sample
  (syntax-parser
    [(_ n body:expr ...+)
     #:declare n (expr/c #'natural?)
     #'(with-sample-fn n.c (λ () body ...))]))

(define (with-sample-fn n thk)
  (for/lists (vs ws #:result (mean vs ws))
             ([_ (in-range n)])
    (set! engine (rsdd-engine))
    (define old s-evidence)
    (begin0
      (with-observe
        (let ([result-pmf (query (thk))]
              [weight-pmf (query o-evidence #:evidence s-evidence)])
          (values result-pmf (weight-pmf #t))))
      (set! s-evidence old))))

(define (hash-sample ht)
  (define target (random))
  (let go ([seq (sequence->stream (in-hash ht))] [acc 0])
    (match-define (stream* (values v p) rst) seq)
    (define acc* (+ acc p))
    (if (< target acc*) v (go rst acc*))))

(define (mean vs ws)
  (define total (apply + ws))
  (define result
    (for/fold ([acc (hash)])
              ([v (in-list vs)]
               [w (in-list ws)]
               #:when (pmf? v)
               [(k p) (in-hash (pmf-hash v))])
      (hash-update acc k (curry + (/ (* p w) total)) 0)))
  (make-categorical (hash->list result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; observation

(struct exn:fail:observe-false exn:fail ())

(define (observe! e)
  (set! o-evidence (&& o-evidence e)))

(define-syntax-rule (with-observe body0 body ...)
  (let ([old o-evidence])
    (begin0
      (begin body0 body ...)
      (set! o-evidence old))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wrapping

(struct header (val))

(define-syntax module-begin
  (make-wrapping-module-begin #'wrap))

(define-syntax top-interaction
  (make-wrapping-top-interaction #'wrap))

(define (wrap e)
  (print-result (query e)))

(define ((~header f) x)
  (match x
    [(header x) (~a x)]
    [_ (f x)]))

(define (print-result res)
  (unless res
    (define ccm (current-continuation-marks))
    (raise (exn:fail:observe-false "observed false" ccm)))
  (define ht (pmf-hash res))
  (if (= (hash-count ht) 1)
      (first (hash-keys ht))
      (print-table
       #:row-sep? '(#t #f ...)
       #:->string (list (~header ~v) (~header ~a))
       (cons
        (map header '(Value Probability))
        (for/list ([(v p) (in-pmf res)])
          (list v p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cost

(define (cost val vars
              #:iterations [iters 10]
              #:budget [budget +inf.0]
              #:wait [wait 1/4])
  (define (make-env)
    (for/hash ([var (in-set vars)])
      (define pr (hash-ref (pmf-hash (query var)) #t 0))
      (values var (< (random) pr))))

  (define (budget-thread)
    (thread
     (λ ()
       (let go ()
         (sleep wait)
         (and (<= (recursive-calls) budget) (go))))))

  (define (query-thread)
    (thread
     (λ ()
       (query val #:environment (make-env)))))

  (gc-terms-hack! make-weak-hasheq)
  (begin0
    (for/list ([k (in-range iters)])
      (clear-cache!)

      (define b-thd (budget-thread))
      (define q-thd (query-thread))
      (sync b-thd q-thd)
      (begin0
        (and (thread-dead? q-thd) (recursive-calls))
        (kill-thread q-thd)
        (kill-thread b-thd)))
    (gc-terms-hack! make-weak-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug

(define (clear-cache!)
  (set! engine (rsdd-engine)))

(define (recursive-calls)
  (send engine recursive-calls))

(define (size v)
  (send engine size v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; categorical random variable

(define (make-categorical xs)
  (bin-cat (filter (λ (x) (not (zero? (cdr x)))) xs)))

(define (bin-cat xs)
  (match xs
    [(list) (assert #f)]
    [(list (cons x _)) x]
    [_
     (define-values (left right)
       (split-at xs (floor (/ (length xs) 2))))
     (define left-sum (foldl + 0 (map cdr left)))
     (if (flip left-sum)
         (bin-cat (renormalize left left-sum))
         (bin-cat (renormalize right (- 1 left-sum))))]))

(define (renormalize xs n)
  (for/list ([x+y (in-list xs)])
    (cons (car x+y) (/ (cdr x+y) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gc-terms-hack!

(require rackunit)
(require/expose rosette/base/core/term (current-terms))

;; This hack is necessary to convert Rosette's internal cache into an
;; `eq?`-based hash for kill safety. Unfortunately, performance is horrible
;; using an `eq?`-based hash for term caching, so it's enabled only during
;; profiling.
(define (gc-terms-hack! make)
  (define cache
    (impersonate-hash
     (make)
     (lambda (h k)
       (values k (lambda (h k e) (ephemeron-value e #f))))
     (lambda (h k v)
       (values k (make-ephemeron k v)))
     (lambda (h k) k)
     (lambda (h k) k)
     hash-clear!))

  (for ([(k v) (current-terms)])
    (hash-set! cache k v))

  (current-terms cache))
