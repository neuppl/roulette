#lang benchmark_ci


(define first-coin (flip 0.5))
(define second-coin (flip 0.5))
(define both-heads (&& first-coin second-coin))
(observe! (! both-heads))
first-coin