#lang benchmark_ci

(define evidence (flip 0.5))

(cond
	[evidence
		(define coin1 (flip 0.5))
		(observe! coin1)
		coin1]
	[else (flip 0.5)])