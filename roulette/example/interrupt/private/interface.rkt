#lang roulette
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 ;; `disrupt.rkt`
 (rename-out
  [module-begin #%module-begin]
  [top-interaction #%top-interaction])

 
 flip
 observe!
 with-observe
 query
 make-json-visualization


 ;; `pmf.rkt`
 pmf
 pmf?
 pmf-support
 in-pmf
 for/pmf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         (for-syntax syntax/parse)
         (for-syntax racket/syntax-srcloc)
         racket/serialize
         mischief/for
         roulette/engine/rsdd
         roulette/private/util
         rosette/base/core/bool
         racket/format
         json
         "pmf.rkt"
         "var-utils.rkt"
         relation/type)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic features

(struct unreachable ())

(define-syntax module-begin
  (make-wrapping-module-begin #'wrap))

(define-syntax top-interaction
  (make-wrapping-top-interaction #'wrap))

(define variable-contexts (make-weak-hash))

(define (wrap e)
  e) ;(if (symbolic? e) (query e) e)

(define (write-now data [port (current-output-port)])
	(displayln data port)
	(flush-output port))


(define (choose-ignored map)
  (values (first map) (rest map)))


(define (compute-pmf flattened-map)
  (fprintf (current-error-port) "Symbolic variables: ~v\n" (length (symbolics flattened-map)))

  (define-values (ignored computed-contents)  
    (choose-ignored flattened-map))

  (define computed-probs
    (for/list ([v+g computed-contents])
      (match-define (cons v g) v+g)
      (fprintf (current-error-port)
              "Finding probability of value: ~v, with guard: ~v\n" 
              v 
              (if (<= (length (symbolics flattened-map)) 50)
                g
                "[redacted]"))
      (define pr ((infer g) (set #t)))
      (cons v pr)))

  (define mass
    (for/sum ([v+p (in-list computed-probs)])
      (cdr v+p)))
  
  (define complete-probs
    (cons (cons (car ignored) (- 1 mass))
          computed-probs))

  (define pmf-probs
    (for/fold ([acc (hash)]
              #:result (pmf acc))
              ([k+v (in-list complete-probs)])
      (match-define (cons k v) k+v)
      (hash-update acc k (curry + v) 0)))

  (define total-prob 
    (for/sum ([(val prob) (in-pmf pmf-probs)]
              #:unless (unreachable? val)) 
      prob))

  (define normalized
    (if (zero? total-prob)
        (pmf (hash))
        (for/pmf ([(val prob) (in-pmf pmf-probs)]
                  #:unless (unreachable? val))
          (values val (/ prob total-prob)))))
  normalized)


(define (with-timeout timeout-duration thnk default)
  (let* ([ch (make-channel)]
         [th (thread (lambda () 
                       (channel-put ch (thnk))))]
         [out (sync/timeout timeout-duration ch)])
    (if out 
        out
        (begin
          (kill-thread th)  ; Kill the thread if timeout occurred
          (default)))))


(define (query e)
  (define variables (symbolics e))
  (write-now (length variables))
  
  (define timeout (read))
  (define assignments (read))
  (define ⊥ (unreachable))
  (define symbolic-map 
    (hash->list (flatten-symbolic (if evidence e ⊥))))
  (define subst-map (for/hash ([idx+asgn assignments])
                              (match-define (cons idx asgn) idx+asgn)
                              (values (list-ref variables idx) asgn)))
  (define result (with-timeout 
                   timeout
                   (lambda () (begin 
                                (compute-pmf (set-symbolic-vars symbolic-map subst-map))
                                "done"))
                   (lambda () "timed-out")))
  (write-now result)
  pmf)

(define (flip-fn pr)
  (cond
    [(= pr 0) #f]
    [(= pr 1) #t]
    [else
     (for/all ([pr pr])
                  (define-measurable* x (bernoulli-measure (- 1 pr) pr))
                  x)]))
          
(define-syntax flip
  (syntax-parser 
    [(_ pr (~optional (~seq #:label label) #:defaults ([label #'(gensym)])))
     #:do [(define src (syntax-srcloc this-syntax))]
     #:with source #`#,src
     #'(let ([out (flip-fn pr)])
          (hash-set! variable-contexts 
                      out 
                      (list
                        source
                        (continuation-mark-set->context (current-continuation-marks))
                        label))
          out)]))


;; Writes all arguments to a json file containing profiling results
(define (write-json-visualization out-file-path
                                  json-file-path 
                                  json-source-code 
                                  json-variable-contexts
                                  json-profiling-results)
  (call-with-output-file out-file-path
    (lambda (out)
      (write-json 
        (hash
          'file-path json-file-path
          'source-code json-source-code
          'stack-contexts json-variable-contexts
          'heuristics json-profiling-results)
        out
        #:indent #\tab))
    #:exists 'replace)
    
  (fprintf (current-error-port) "JSON results produced at: ~a\n" out-file-path)


	(displayln "Running visualize.py to generate html ..." (current-error-port))
	(system (string-append "python3 visualize.py " out-file-path)))

;; Converts global variable contexts into a json serializable format
(define (make-json-variable-contexts e)
  (define variables (symbolics e))
  (define (srcloc->js-hash loc)
    (if (srcloc? loc)
      (begin 
        (match-define (srcloc source line column position span) loc)
        (hash
          `source (~a source)
          `line line
          `column column
          `position position
          `span span))
      #f))

  (for/hash ([(key value) (in-hash variable-contexts)])
    (values (->symbol (index-of variables key)) 
            (hash 'syntactic-source (srcloc->js-hash (first value))
                  'context (map (lambda (ctx-pair)
                                  (list (~a (car ctx-pair)) 
                                        (srcloc->js-hash (cdr ctx-pair)))) 
                                (second value))
                  'label (if (symbol? (third value))
                             (symbol->string (third value))
                             (third value))))))


; Manual fix for broken serialization for hashes in port writing
(define (deserialize-to-heuristics-hash results)
  (if (list? results)
      (make-hash (map (lambda (item) (if (and (list? item) (= (length item) 3))
                                         (list (first item)
                                           (list (second item)
                                           (third item)))
                                         item))
                      results))
      results))

;; Converts provided heuristics/results into a json serializable format
(define (make-json-profiling-results results num-vars)
  (if (hash? results)
      (begin
        (define json-results 
          (for/hash! ([(key value) (in-hash results)])
            (values (->symbol key) 
                    value)))
        (for ([key (in-range num-vars)])
          (hash-update! json-results 
                        (->symbol key) 
                        (lambda (x) x) 
                        (list 0 0)))

        json-results)
      results))


;provided information from the input port, writes profiling results into a json
(define (make-json-visualization e)
  (define file-path (read))
  (define out-file-path (path->string (path-replace-extension file-path ".json")))

  (define source-code (read))
  (define result-type (read)) ; 'stream or 'single
  

  ;first time getting results (required regardless of stream/single)
  (define results (read))
  (define json-profiling-results
        (make-json-profiling-results (deserialize-to-heuristics-hash results)
                                     (length (symbolics e))))
  
  ;these remain the same across all runs of profiler.
  (define json-variable-contexts (make-json-variable-contexts e))
  
  (define write-js-viz
    (lambda (results) 
      (write-json-visualization 
        out-file-path
        file-path 
        source-code 
        json-variable-contexts
        results)))
  
  (write-js-viz json-profiling-results)

  (when (equal? result-type 'stream)
    (let loop ()
      (define results (read))
      (unless (equal? results 'stop)
        (define json-profiling-results
          (make-json-profiling-results (deserialize-to-heuristics-hash results)
                                       (length (symbolics e))))

        (write-js-viz json-profiling-results)
        (loop))))
    
    (fprintf 
      (current-error-port)
      "Completed running profiler, visualization of results can be found at ~v\n"
      (path->string (path-replace-extension out-file-path ".html")))
    (write-now out-file-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; observation

(define evidence #t)

(define (observe! e)
  (set! evidence (&& evidence e)))

(define-syntax-rule (with-observe body0 body ...)
  (let ([old evidence])
    (begin0
      (begin body0 body ...)
      (set! evidence old))))