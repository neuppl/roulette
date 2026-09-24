#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/reliability.rkt")

(provide main)

(define (reliability-width w) (reliability w 4))

(define (main) (max-arg reliability-width #:start 1 #:step 1))

(module+ main
  (main))
