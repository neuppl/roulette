#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/ecmp.rkt")

(provide main)

(define (main) (max-arg ecmp-reliability #:start 1 #:step 1))

(module+ main
  (main))
