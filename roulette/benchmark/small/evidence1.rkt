#lang roulette/example/disrupt
(provide small-evidence1)

(define evidence (flip 0.5))
(define coin (flip 0.5))

(define (small-evidence1)
  (query (cond
           [evidence
            (observe! coin)
            evidence]
           [else evidence])))
