#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(provide main)

(define (model)
  (define evidence (flip 0.5))
  (cond
    [evidence
     (define coin1 (flip 0.5))
     (observe! coin1)
     coin1]
    [else (flip 0.5)]))

(define (main) (benchmark (model)))


(module+ main
  (main))
