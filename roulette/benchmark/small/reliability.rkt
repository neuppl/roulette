#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(require "../shared/reliability.rkt")
(provide main)


(define (main) (benchmark (reliability 4 4)))


(module+ main
  (main))
