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
         roulette/engine/rsdd
         roulette/private/util
         rosette/base/core/bool
         racket/format
         json
         "pmf.rkt"
         "var-utils.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic features

(struct unreachable ())

(define-syntax module-begin
  (make-wrapping-module-begin #'wrap))

(define-syntax top-interaction
  (make-wrapping-top-interaction #'wrap))

(define variable-contexts (make-weak-hash))

(define expr-to-profile #f)

(define (wrap e)
  e) ;(if (symbolic? e) (query e) e)
(define (choose-ignored map)
  (values (first map) (rest map)))
 
#;(define (find-costly-variable-assignment timeout flattened-map vars #:samples [samples #f])
  (define initial-state (for/list ([v vars])
                          (cons v UNKNOWN)))
  (define score-func
    (lambda (state)
      (define assignments (for/hash ([var+asgn state]
                                      #:unless (unknown? (cdr var+asgn)))
                              (match-define (cons var asgn) var+asgn)
                              (values var asgn)))
      (define after-assignments 
        (if (empty? assignments)
            flattened-map
            (begin
              (set-symbolic-vars flattened-map assignments))))

      (with-timeout timeout 
                    (lambda () 
                      (compute-pmf after-assignments)
                      (for/sum ([var+asgn state])
                              (match-define (cons var asgn) var+asgn)
                              (if (unknown? asgn) 1 0)))
                    (lambda () (displayln "timed out") -inf.0)))) 
                    ; assign a negative score if computing the pmf times out
  (search initial-state
          timeout 
          (random-specialization-transition initial-state) 
          score-func
          #:samples samples))


(define (compute-pmf flattened-map)
  (printf "Symbolic variables: ~v\n" (length (symbolics flattened-map)))

  (define-values (ignored computed-contents)  
    (choose-ignored flattened-map))

  (define computed-probs
    (for/list ([v+g computed-contents])
      (match-define (cons v g) v+g)
      (printf "Finding probability of value: ~v, with guard: ~v\n" 
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

(define (src e)
  (hash-ref variable-contexts (first (symbolics e))))

#;(define (query e #:samples [samples #f])
  (define ⊥ (unreachable))
      
  (define symbolic-map 
    (hash->list (flatten-symbolic (if evidence e ⊥))))

  (define DEFAULT-SAMPLES 5)

  (define INITIAL-TIMEOUT 5)
  (define SAMPLING-TIMEOUT 5)


  (define sampling-search 
    (lambda (samples)
      (find-costly-variable-assignment
        SAMPLING-TIMEOUT
        #:samples samples
        symbolic-map  
        (symbolics (if evidence e ⊥)))))

  (if samples
    (sampling-search samples)
    (with-timeout 
      INITIAL-TIMEOUT
      '(compute-pmf symbolic-map)
      (lambda () (printf "timed out, sampling for heuristics\n\n")
                 (sampling-search DEFAULT-SAMPLES)))))

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


(define (query e pch)
(displayln expr-to-profile)
  (define variables (symbolics e))
  (place-channel-put pch (length variables))
  
  (define timeout (place-channel-get pch))
  (define assignments (place-channel-get pch))
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
  (place-channel-put pch result)
  pmf)

(define (flip-fn pr)
  (cond
    [(= pr 0) #f]
    [(= pr 1) #t]
    [else
     (let ([out (for/all ([pr pr])
                  (define-measurable* x (bernoulli-measure (- 1 pr) pr))
                  x)])
          (hash-set! variable-contexts 
                     out 
                     (continuation-mark-set->context (current-continuation-marks)))
          out)]))
          
(define-syntax flip
  (syntax-parser 
    [(_ pr)
     #:do [(define src (syntax-srcloc this-syntax))]
     #:with source #`#,src
     #'(let ([out (flip-fn pr)])
          (hash-update! variable-contexts out (lambda (x) (cons source x)))
          out)]))



(define (make-json-visualization e pch)
  (define variables (symbolics e))
  (define file-path (place-channel-get pch))
  (define out-file-path (path->string (path-replace-extension file-path ".json")))
  (define source-code (place-channel-get pch))
  (define profiling-results (place-channel-get pch))
  (define profiling-results-js
    (if (hash? profiling-results)
        (for/hash ([(key value) (in-hash profiling-results)])
          (values (string->symbol (number->string key)) value))
          profiling-results))

  (define (srcloc->js-hash loc)
    (match-define (srcloc source line column position span) loc)
    (hash
      `source (~a source)
      `line line
      `column column
      `position position
      `span span))
  (define variable-contexts-js 
    (for/hash ([(key value) (in-hash variable-contexts)])
      (values (string->symbol (number->string (index-of variables key))) 
              (hash 'syntactic-source (srcloc->js-hash (car value))
                    'context (map (lambda (ctx-pair)
                                    (list (~a (car ctx-pair)) 
                                          (srcloc->js-hash (cdr ctx-pair)))) 
                                  (cdr value))))))
  (call-with-output-file out-file-path
    (lambda (out)
      (write-json 
        (hash
          'source-code source-code
          'stack-contexts variable-contexts-js
          'heuristics profiling-results-js)
        out
        #:indent #\tab))
    #:exists 'replace)
  (place-channel-put pch "Done"))


(define (place-main pch)
  (if expr-to-profile
      (query pch)
      (error "no expression registered to profile")))

(define (generate-json pch)
  (if expr-to-profile
      (make-json-visualization pch)
      (error "no expression registered to profile")))
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




;(define m (list (cons #t #f) (cons #f #t)))

;(with-timeout 1 (lambda () (compute-pmf m)) (lambda () (displayln "timed out")))

