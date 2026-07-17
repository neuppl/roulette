#lang roulette/example/disrupt

(require "private/bayes.rkt")
(provide bn-cancer)


(define (bn-cancer)
      (main "bayesian-networks/cancer.bif" 'Xray `(Pollution Smoker Cancer Dyspnoea Xray)))
