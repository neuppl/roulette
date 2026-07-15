#lang roulette/example/disrupt

(define evidence (flip 0.5))

(benchmark
	(cond
		[evidence
			(define coin1 (flip 0.5))
			(observe! coin1)
			coin1]
		[else (flip 0.5)]))