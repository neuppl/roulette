#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(provide main)

(define (model)
  (define evidence (flip 0.5))
  (define coin (flip 0.5))
  (cond
    [evidence
     (observe! coin)
     evidence]
    [else evidence]))

(define (main) (benchmark (model)))


(module+ main
  (main))
