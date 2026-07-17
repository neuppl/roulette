#lang roulette/example/disrupt
(provide small-two-coins)


(define first-coin (flip 0.5))
(define second-coin (flip 0.5))
(define both-heads (&& first-coin second-coin))
(observe! (! both-heads))

(define (small-two-coins) (query first-coin))
