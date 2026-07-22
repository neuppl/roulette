#lang roulette/example/disrupt

(require "shared/n-grid.rkt")

(define tmp (make-grid 150))

;(time (query tmp))
(time (size tmp))
