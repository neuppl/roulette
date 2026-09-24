#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/hmm.rkt")

(provide main)

(define (main) (scale hmm (20 40 60 80 100 120 140 160 180 200)))

(module+ main
  (main))
