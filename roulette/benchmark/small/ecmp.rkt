#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(require "../shared/ecmp.rkt")
(provide main)


(define (main) (benchmark (ecmp-reliability 8)))


(module+ main
  (main))
