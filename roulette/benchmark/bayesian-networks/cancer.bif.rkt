#lang roulette/example/disrupt

(require "private/bayes.rkt")



(benchmark
      (main "bayesian-networks/cancer.bif" 'Xray `(Pollution Smoker Cancer Dyspnoea Xray)))
