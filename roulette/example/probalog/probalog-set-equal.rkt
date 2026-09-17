#lang roulette/example/disrupt
(require "probalog-core.rkt")
(provide run-datalog saturate-semi)

(define (saturate-semi base rules)
  (let loop ([full base] [delta (for/list ([(k g) base]) k)])
    (define next (immediate-semi full delta rules))
    (define changed (changed-keys full next))
    (if (null? changed) full (loop next changed))))

(define (run-datalog base-fact-probs rules)
  (saturate-semi (make-base-set base-fact-probs) rules))
