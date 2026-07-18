#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/n-grid.rkt")

(provide main)

(define (main) (scale make-grid (10 15 20 25 30 35 40 45)))

(module+ main
  (main))
