#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/n-grid.rkt")

(provide main)

(define (main) (max-arg make-grid #:start 1 #:step 5 #:rec-limit 500000))

(module+ main
  (main))
