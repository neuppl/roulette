#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/gossip.rkt")

(provide main)

(define (gossip-nodes n) (gossip n 3))

(define (main) (scale gossip-nodes (3 4 5 6 7)))

(module+ main
  (main))
