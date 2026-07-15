#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require "util.rkt"
           "../example/disrupt/private/pmf.rkt"
           "bn/cancer.rkt")

  (define ϵ 0.0000001)
  (observe! (equal? Xray 'positive))
  (check-close ϵ
               (pmf-hash (query Cancer))
               (hash 'True 0.050288025905515975
                     'False 0.949711974094484)))
