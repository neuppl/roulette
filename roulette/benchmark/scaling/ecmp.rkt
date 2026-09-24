#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/ecmp.rkt")

(provide main)

(define (main) (scale ecmp-reliability (2 4 6 8 10)))

(module+ main
  (main))
