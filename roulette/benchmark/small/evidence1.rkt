#lang benchmark_ci

(define evidence (flip 0.5))
(define coin (flip 0.5))

(cond
  [evidence
   (observe! coin)
   evidence]
  [else evidence])