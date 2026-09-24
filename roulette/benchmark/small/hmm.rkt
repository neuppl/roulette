#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(require "../shared/hmm.rkt")
(provide main)


(define (main) (benchmark (hmm 40)))


(module+ main
  (main))
