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
 guided-sample

 ;; profiling
 cost
 profile
 process-results

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
                     syntax/parse
                     racket/syntax-srcloc)
         (prefix-in engine: racket/engine)
         racket/match
         "../../../../bdd-engine.rkt"
         (prefix-in rs: roulette/engine/rsdd)
         (prefix-in rkt: roulette/engine/rbdd)
         text-table
         "pmf.rkt"
         "profile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global parameters

(gc-terms!)
(define make-engine (if (equal? bdd-engine-backend "rsdd") 
                        rs:rsdd-engine 
                        rkt:rbdd-engine))

(define bernoulli-measure (if (equal? bdd-engine-backend "rsdd") 
                              rs:bernoulli-measure 
                              rkt:bernoulli-measure))
(define kill-signal-box (if (equal? bdd-engine-backend "rsdd") 
                            rs:kill-signal-box 
                            rkt:kill-signal-box))

(define engine (make-engine))
(define o-evidence #t)
(define s-evidence #t)
(define variable-contexts (make-hash))
(define var-label-map (make-hash))
(define (variable-from-label label)
  (first (hash-ref variable-contexts label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic features

(struct unreachable ())

(define-syntax flip
  (syntax-parser 
    [(_ pr (~optional (~seq #:label label) #:defaults ([label #'(gensym)])))
     #:do [(define src (syntax-srcloc this-syntax))]
     #:with source #`#,src
     #'(let ([out (flip-fn pr)]) 
          (begin 
            (hash-set! variable-contexts
                      label
                      (list
                        out
                        source
                        (continuation-mark-set->context (current-continuation-marks))))
            (hash-set! var-label-map
                      out
                      label)
          out))]))

(define (flip-fn pr) 
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
    (set! engine (make-engine))
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

(define (guided-sample e var-labels #:take [num-vars 10])
  (define vars (map variable-from-label var-labels))
  (define env
    (for/hash ([var (take vars num-vars)])
      (define pr (hash-ref (pmf-hash (query var)) #t 0))
      (values var (< (random) pr))))
  (query e #:environment env))
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

(define-syntax-rule (wrap e ...)
  (call-with-values (λ () e ...) print-values))

(define (print-values . es)
  (for ([e (in-list es)])
    (print-result (query e))))

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
      ((current-print) (first (hash-keys ht)))
      (print-table
       #:row-sep? '(#t #f ...)
       #:->string (list (~header ~v) (~header ~a))
       (cons
        (map header '(Value Probability))
        (for/list ([(v p) (in-pmf res)])
          (list v p))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cost

(define (with-timeout duration thnk default)
  (let* ([th (thread thnk #:keep 'results)]
         [out (sync/timeout duration th)])
    ;; break-thread fires at the (sleep 0) points in enc, which happen
    ;; before every BDD node encoding. Without waiting for th to die here,
    ;; the next iteration starts while th is still live, causing concurrent
    ;; access to the global encodable? cache and indefinite hangs.
    (break-thread th)
    (or (sync/timeout (* 10 duration) th)
        (kill-thread th))
    (if out
        (thread-wait out)
        (default))))


; Returns a hash from environments to the number of recursive calls needed to evaluate val with all 
; vars sampled randomly over a number of iterations. A value of #f means the val couldn't evaluate 
; within budget for that sample.
; Returns a hash from environments to the number of recursive calls needed to evaluate val with all 
; vars sampled randomly over a number of iterations. A value of #f means the val couldn't evaluate 
; within budget for that sample.
(define (cost val vars
              #:iterations [iters 10]
              #:budget [budget +inf.0]
              #:wait [wait 1/4])
  (define (make-env)
    (for/hash ([var (in-set vars)])
      (define pr (hash-ref (pmf-hash (query var)) #t 0))
      (values var (< (random) pr))))

  (define (budget-thread return)
    (thread
     (λ ()
       (let go ()
         (sleep wait)
         (displayln "after wait")
         (if (<= (recursive-calls) budget)
             (go)
             (begin
              (box-cas! kill-signal-box #f return)
              (displayln "after setting box")))))))

  (for/hash ([k (in-range iters)])
    (displayln rs:enc-instrumentation)
    (printf "~a: " k)
    (displayln "here")
    (clear-cache!)
    (define env (make-env))
    (let ([b-thd #f])
      (let/cc return
        (set! b-thd (budget-thread return))
        (query val #:environment env))
      (kill-thread b-thd))
    (values env
            (cond
              [(unbox kill-signal-box)
                (set-box! kill-signal-box #f)
                (displayln "after timeout")
                #f]
              [else 
                (let ([rec-calls (recursive-calls)])
                  (printf "completed sample (~a recursive calls)\n" rec-calls)
                  rec-calls)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; profiling



;Ordered list of most "expensive" symbolic variables in provided expression, based on heuristics
(define (profile e #:timeout [duration 1]
                   #:iterations [iters 10]
                   #:samples [samples 10]
                   #:specialize-amt [num-vars (inexact->exact (round (/ (length (symbolics e)) 2)))])
  
  (define transition (make-random-specialization-transition (symbolics e) num-vars))
  (define heuristics (make-heuristics))

  (display "\u001B[1mProfiler Configuration: \u001B[0m\n")
  (printf "timeout: ~a\n" duration)
  (printf "specialization-amt: ~a\n" num-vars)
  (printf "iterations: ~a\n" iters)
  (printf "samples: ~a\n" samples)
  (display "\u001B[1mRunning profiler...\u001B[0m\n")
  (for ([k (in-range samples)])
    (printf "Sample ~a\n" k)
    (define var-subset (transition))
    (define cost-map (cost e
                          var-subset
                          #:iterations iters
                          #:budget 0
                          #:wait duration))
    (heuristics cost-map))
  (display "\u001B[1mFinished running profiler.\u001B[0m\n")
  (heuristics #f))



(define (process-results profiler-results)
  (variable-labels var-label-map profiler-results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug

(define (clear-cache!)
  (rkt:reset-bdd!)
  (set! engine (make-engine)))

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


(gc-terms-hack! make-weak-hasheq)