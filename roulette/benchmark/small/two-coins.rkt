#lang roulette/example/disrupt


(define first-coin (flip 0.5))
(define second-coin (flip 0.5))
(define both-heads (&& first-coin second-coin))
(observe! (! both-heads))
(benchmark first-coin)