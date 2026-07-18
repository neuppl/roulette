#lang roulette/example/disrupt

(require "../private/bayes.rkt" "../benchmarking.rkt")
(provide main)


(define (main)
  (benchmark
    (main* "bayesian-networks/survey.bif"
           'T
           '(A S E R O T))))
