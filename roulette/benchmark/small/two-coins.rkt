#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(provide main)


(define first-coin (flip 0.5))
(define second-coin (flip 0.5))
(define both-heads (&& first-coin second-coin))
(observe! (! both-heads))

(define (main) (benchmark first-coin))


(module+ main
  (main))
