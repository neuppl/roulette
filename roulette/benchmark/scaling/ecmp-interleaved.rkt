#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/ecmp.rkt")

(provide main)

(define (main) (scale ecmp-reliability/interleaved (10 25 50 75 100)))

(module+ main
  (main))
