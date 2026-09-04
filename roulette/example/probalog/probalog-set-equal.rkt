#lang roulette/example/disrupt
(require "probalog-core.rkt")
(provide run-datalog saturate-prob saturate-naive)

;; Loop immediate-prob until the guards stop changing.
(define (saturate-prob full delta rules)
  (define-values (next-full next-delta) (immediate-prob full delta rules))
  (define changed-keys (for/list ([(k g) next-delta]) k))
  (if (set-equal? next-full full changed-keys)
      full
      (saturate-prob next-full next-delta rules)))

;; The same loop over the naive operator. There is no delta to narrow
;; the fixpoint check to, so every key is compared each round -- which is
;; affordable precisely on the backends this is for, where comparing two
;; guards is a pointer comparison rather than a solver call.
(define (saturate-naive full rules)
  (define next (immediate-naive full rules))
  (define keys (for/list ([(k g) next]) k))
  (if (set-equal? next full keys)
      full
      (saturate-naive next rules)))

;; Which fixpoint loop to run is a property of how guards are
;; represented, not of the program. Semi-naive evaluation avoids
;; redundant derivations, which is worth it when a guard is a term that
;; grows with every derivation; it is a large loss when a guard is
;; canonical, because the "newly derivable this round" conditions it
;; builds are far more complex than the plain conditions naive
;; evaluation builds. See immediate-naive in probalog-core.rkt.
(define (run-datalog base-fact-probs rules)
  (define base-set (make-base-set base-fact-probs))
  (if (bdd-guards?)
      (saturate-naive base-set rules)
      (saturate-prob base-set base-set rules)))
