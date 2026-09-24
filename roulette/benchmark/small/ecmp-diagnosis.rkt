#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(require "../shared/ecmp.rkt")
(provide main)


(define (main) (benchmark (ecmp-diagnosis 5 2)))


(module+ main
  (main))
