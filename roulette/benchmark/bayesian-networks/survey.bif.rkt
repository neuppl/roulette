#lang roulette/example/disrupt

(require "private/bayes.rkt")


(benchmark
      (main "bayesian-networks/survey.bif" 
            'T
            '(A S E R O T)))

