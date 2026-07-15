#lang roulette/example/disrupt

(define evidence (flip 0.5))
(define coin (flip 0.5))

(benchmark (cond
              [evidence
              (observe! coin)
              evidence]
              [else evidence]))