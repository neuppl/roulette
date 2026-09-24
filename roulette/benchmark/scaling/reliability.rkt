#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/reliability.rkt")

(provide main)

(define (reliability-depth d) (reliability 4 d))

(define (main) (scale reliability-depth (2 4 6 8 10 12 14 16)))

(module+ main
  (main))
