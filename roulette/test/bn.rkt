#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (only-in "../example/disrupt/core.rkt" pmf-hash)
           "util.rkt")

  (check-close 0.0000001
               (pmf-hash
                (query
                 ;; A bit silly, but we have to do that for the regions to
                 ;; work because flips need to occur within `query`.
                 (define Cancer (dynamic-require "bn/cancer.rkt" 'Cancer))
                 (define Xray (dynamic-require "bn/cancer.rkt" 'Xray))
                 (observe! (equal? Xray 'positive))
                 Cancer))
               (hash 'True 0.050288025905515975
                     'False 0.949711974094484)))
