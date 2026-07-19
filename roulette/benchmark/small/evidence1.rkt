#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(provide main)

(define evidence (flip 0.5))
(define coin (flip 0.5))

(define (main)
  (benchmark (cond
           [evidence
            (observe! coin)
            evidence]
           [else evidence])))


(module+ main
  (main))
