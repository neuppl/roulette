#lang roulette/example/disrupt
(require "probalog-core.rkt")
(provide run-datalog)

;; Loop immediate-prob until the guards stop changing.
(define (saturate-prob full delta rules)
  (define-values (next-full next-delta) (immediate-prob full delta rules))
  (define changed-keys (for/list ([(k g) next-delta]) k))
  (if (set-equal? next-full full changed-keys)
      full
      (saturate-prob next-full next-delta rules)))

(define (run-datalog base-fact-probs rules)
  (define base-set (make-base-set base-fact-probs))
  (saturate-prob base-set base-set rules))