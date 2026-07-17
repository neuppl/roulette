#lang roulette/example/disrupt
(provide small-evidence2)

(define evidence (flip 0.5))

(define (small-evidence2)
  (query
   (cond
     [evidence
      (define coin1 (flip 0.5))
      (observe! coin1)
      coin1]
     [else (flip 0.5)])))
