#lang roulette/example/disrupt
(require "../benchmarking.rkt"
         "../shared/hmm.rkt")

(provide main)

(define (main) (max-arg hmm #:start 20 #:step 20))

(module+ main
  (main))
