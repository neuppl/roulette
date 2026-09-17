#lang roulette

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (all-from-out roulette)
 flip
 reward!
 observe!
 expected-utility)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require roulette/engine/rsdd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constructs

(define engine
  (rsdd-engine #:semiring expectation-semiring))

(define (flip θ)
  (define-measurable* x
    (bernoulli-measure `(,(- 1 θ) 0) `(,θ 0)
                       #:semiring expectation-semiring))
  x)

(define (reward! r)
  (define-measurable* x
    (bernoulli-measure `(0 0) `(1 ,r)
                       #:semiring expectation-semiring))
  (void))

(define (observe! o)
  (unless o
    (define-measurable* x
      (bernoulli-measure `(0 0) `(0 0)
                         #:semiring expectation-semiring))
    (void)))

(define (expected-utility e)
  (match-define (list prob rew)
    ((density (infer #:engine engine e)) #t))
  (/ rew prob))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module reader syntax/module-reader
  #:language 'roulette/example/bentham)
