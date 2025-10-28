#lang roulette
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 ;; `disrupt.rkt`
 (rename-out
  [module-begin #%module-begin]
  [top-interaction #%top-interaction])

 query
 flip
 test

 observe!
 with-observe

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
         roulette/engine/rsdd
         roulette/private/util
         rosette/base/core/bool
         roulette/example/interrupt/private/search
         "pmf.rkt"
         "var-utils.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic features

(struct unreachable ())

(define-syntax module-begin
  (make-wrapping-module-begin #'wrap))

(define-syntax top-interaction
  (make-wrapping-top-interaction #'wrap))

(define (wrap e)
  (if (symbolic? e) (query e) e))
(define (choose-ignored map)
  (values (first map) (rest map)))
 
(define (find-costly-variable-assignment timeout flattened-map vars #:samples [samples #f])
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



(define (query e #:samples [samples #f])
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
      (lambda () (compute-pmf symbolic-map))
      (lambda () (printf "timed out, sampling for heuristics\n\n")
                 (sampling-search DEFAULT-SAMPLES)))))


(define (test e)
  (define symbolic-map 
    (hash->list (flatten-symbolic e)))
  (displayln symbolic-map)
  (define var (first (symbolics e)))

  (printf "\n\n\n~v\n\n\n" (set-symbolic-vars symbolic-map (hash var #t)))
  42)

(define (flip pr)
  (cond
    [(= pr 0) #f]
    [(= pr 1) #t]
    [else
     (for/all ([pr pr])
       (define-measurable* x (bernoulli-measure (- 1 pr) pr))
       x)]))

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

