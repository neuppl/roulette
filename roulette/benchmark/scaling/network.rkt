#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/network.rkt")

(provide main)

(define (main) (scale network (1 5 10 15 20 25 30 35)))

(module+ main
  (main))


