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
 save-results
 visualize

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
         pkg/lib
         "../../../../bdd-engine.rkt"
         (prefix-in rs: roulette/engine/rsdd)
         (prefix-in rkt: roulette/engine/rbdd)
         text-table
         "pmf.rkt"
         "profile.rkt"
         json)

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
    (kill-thread th)
    (if out
        (thread-wait out)
        (default))))


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

  (for/hash ([k (in-range iters)])
    (printf "~a: " k)
    (clear-cache!)

    (define env (make-env))
    (define out (with-timeout wait 
                              (lambda () 
                                (query val #:environment env)
                                (define rec-calls (recursive-calls))
                                (printf "completed sample in ~a recursive calls \n" rec-calls)
                                rec-calls
                              )
                              (lambda () 
                                (displayln "timed out")
                                #f)))

    (begin0
      (values env out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; profiling

;Ordered list of most "expensive" symbolic variables in provided expression, based on heuristics
(define (profile e #:timeout [duration 1]
                   #:samples [samples 10]
                   #:iterations [iters 10]
                   #:specialize-amt [num-vars (inexact->exact (round (/ (length (symbolics e)) 2)))]
                   #:stream-visualization [stream-path? #f])
  
  (define transition (make-random-specialization-transition (symbolics e) num-vars))
  (define config-data (hash 'timeout duration
                            'specialization-amt num-vars
                            'iterations iters
                            'samples samples
                            'total-vars (length (symbolics e))))
  (define heuristics (make-heuristics config-data))

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
    (heuristics cost-map)
    (when stream-path?
          (define cur-cost-map (heuristics #f))
          (define json-path (save-results cur-cost-map stream-path? #:print #f))
          (visualize json-path #:print #f)))
  (display "\u001B[1mFinished running profiler.\u001B[0m\n")
  (heuristics #f))



(define (save-results profiler-results save-path #:print [print? #t])
  (define json-path (path->string (path-replace-extension save-path ".json")))
  (define rkt-path (path->string (path-replace-extension save-path ".rkt")))
  (define (srcloc->js-hash loc)
    (match-define (srcloc source line column position span) loc)
    (hash
      `source (~a source)
      `line line
      `column column
      `position position
      `span span))

  (define json-formatted-results 
    (hash-set*
      (for/hash ([(key value) (in-hash profiler-results)])
        (cond [(string? key) (values (string->symbol key) value)]
              [(symbolic? key) 
              (let ([var-label (hash-ref var-label-map key)])
                  (values (string->symbol var-label)
                          (hash 
                            'results (hash 'num-successful-samples (first value)
                                            'num-total-samples (second value)
                                            'total-recursive-calls (third value))
                            'cost-value (var-value value)
                            'syntactic-source (srcloc->js-hash (second (hash-ref variable-contexts var-label))))))]))
      'source-code (call-with-input-file rkt-path
                      (lambda (in) (port->string in)))
      'file-path rkt-path))
  
  (call-with-output-file json-path
    (lambda (out)
      (write-json 
        json-formatted-results
        out
        #:indent #\tab))
    #:exists 'replace)
  
  (when print? (printf "\u001B[1mSaved profiler results to ~a\u001B[0m\n" json-path))
  json-path)


(define (visualize json-path #:open [open? #f] #:print [print? #t])
  (define-values (viz-proc _out _in _err)
    (subprocess (current-output-port) (current-input-port) (current-error-port) (find-executable-path "python3") 
                (path->string (simplify-path (build-path (pkg-directory "roulette") "example/disrupt/private/visualize.py")))
                json-path))
  (subprocess-wait viz-proc)
  (define html-path (path->string (path-replace-extension json-path ".html")))
  
  (when open?
        (define-values (open-proc _out _in _err)
          (subprocess (current-output-port) (current-input-port) (current-error-port) (find-executable-path "open") html-path))
        (subprocess-wait open-proc))
  (when print?
    (printf "HTML file produced at: ~a" html-path))
  html-path)

(define (process-results profiler-results)
  (variable-labels var-label-map profiler-results))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; profiler evaluation



;Compares the time taken to randomly sample num-vars in e, against the time taken to sample the first 
; num-vars in var-labels. Returns the difference, and prints information to output port. 
(define (compare-sampling-duration e var-labels #:take [num-vars 10] 
                                                #:samples [num-samples 10])
  (define-values (_ __ real-guided ___) 
    (time-apply (lambda ()
      (for ([n (in-range num-samples)]) 
        (guided-sample e var-labels #:take num-vars))) 
    (list)))
  (printf "Time taken to guided sample ~a top vars ~a times: ~a" num-vars num-samples real-guided)
  (define shuffled-var-labels (shuffle var-labels))
  
  (define-values (____ _____ real-random ______) 
    (time-apply (lambda ()
      (for ([n (in-range num-samples)]) 
        (guided-sample e shuffled-var-labels #:take num-vars))) 
    (list)))
  (printf "Time taken to sample ~a random vars ~a times: ~a" num-vars num-samples real-random)
  (- real-guided real-random))


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