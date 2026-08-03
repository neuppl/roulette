#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/hardware.rkt")

(provide main)

(define (main) (scale hardware (1 2 3 4 5 6 7 8 9 10)))

(module+ main
  (main))
