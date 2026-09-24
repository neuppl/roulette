#lang roulette/example/disrupt
(provide hmm)

;; Filtering in a hidden Markov chain of `n` steps: every emission is
;; observed, so the evidence grows with the model.
(define (hmm n)
  (define STAY 0.9)     ; P(state unchanged from one step to the next)
  (define ACCURATE 0.8) ; P(emission agrees with the state)
  (let loop ([i 0] [state (flip 0.5)])
    (cond
      [(= i n) state]
      [else
       (define next (if state (flip STAY) (flip (- 1 STAY))))
       (define emit (if next (flip ACCURATE) (flip (- 1 ACCURATE))))
       (observe! emit)
       (loop (add1 i) next)])))
