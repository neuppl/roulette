#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/gossip.rkt")

(provide main)

(define (gossip-nodes n) (gossip n 3))

(define (main) (max-arg gossip-nodes #:start 2 #:step 1))

(module+ main
  (main))
