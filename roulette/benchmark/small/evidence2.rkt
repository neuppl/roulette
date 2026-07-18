#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(provide main)

(define evidence (flip 0.5))

(define (main)
  (benchmark (query
   (cond
     [evidence
      (define coin1 (flip 0.5))
      (observe! coin1)
      coin1]
     [else (flip 0.5)]))))


(module+ main
  (main))
