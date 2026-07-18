#lang roulette/example/disrupt

(require "../private/bayes.rkt" "../benchmarking.rkt")
(provide main)


(define (main)
  (benchmark
    (main* "bayesian-networks/cancer.bif" 'Xray `(Pollution Smoker Cancer Dyspnoea Xray))))
