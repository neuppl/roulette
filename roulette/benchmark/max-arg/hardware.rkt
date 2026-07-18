#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/hardware.rkt")

(provide main)

(define (main) (max-arg hardware #:start 1 #:step 1))

(module+ main
  (main))
