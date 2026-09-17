#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(require "../shared/n-grid.rkt")
(provide main)


(define (main) (benchmark (make-grid 40)))


(module+ main
  (main))
