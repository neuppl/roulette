#lang roulette/example/disrupt
(require "probalog-core.rkt")
(provide run-datalog saturate-prob saturate-naive saturate-semi)

;; Loop immediate-prob until the guards stop changing.
(define (saturate-prob full delta rules)
  (define-values (next-full next-delta) (immediate-prob full delta rules))
  (define changed-keys (for/list ([(k g) next-delta]) k))
  (if (set-equal? next-full full changed-keys)
      full
      (saturate-prob next-full next-delta rules)))

;; The same loop over the naive operator, kept as a baseline.
(define (saturate-naive full rules)
  (define next (immediate-naive full rules))
  (define keys (for/list ([(k g) next]) k))
  (if (set-equal? next full keys)
      full
      (saturate-naive next rules)))

;; Semi-naive over accumulated guards. The delta is a plain list of the
;; keys whose guard changed, so it prunes which derivations are
;; attempted without ever entering into a guard. It also doubles as the
;; fixpoint test: nothing changed means nothing left to do.
(define (saturate-semi base rules)
  (let loop ([full base] [delta (for/list ([(k g) base]) k)])
    (define next (immediate-semi full delta rules))
    (define changed (changed-keys full next))
    (if (null? changed) full (loop next changed))))

;; Which fixpoint loop to run is a property of how guards are
;; represented, not of the program. Deriving from a delta of guards is
;; worth it when a guard is a term that grows with every derivation; it
;; is a large loss when a guard is canonical. See the operators in
;; probalog-core.rkt.
(define (run-datalog base-fact-probs rules)
  (define base-set (make-base-set base-fact-probs))
  (if (bdd-guards?)
      (saturate-semi base-set rules)
      (saturate-prob base-set base-set rules)))
