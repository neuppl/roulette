#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(require "../shared/gossip.rkt")
(provide main)


(define (main) (benchmark (gossip 6 4)))


(module+ main
  (main))
