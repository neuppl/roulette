#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/network.rkt")

(provide main)

(define (main) (max-arg network #:start 1 #:step 2))

(module+ main
  (main))


