#lang roulette/example/disrupt

(require "private/bayes.rkt")
(provide bn-survey)


(define (bn-survey)
      (main "bayesian-networks/survey.bif"
            'T
            '(A S E R O T)))

