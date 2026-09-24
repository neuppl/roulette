#lang roulette/example/disrupt
(provide reliability)

;; Two-terminal network reliability on a layered graph: `width` nodes per
;; layer, `depth` layers, fully connected between layers, edges failing
;; independently. Paths share edges, so this is the disjoint-sum problem.
;; `width` drives cost far harder than `depth`.
(define (reliability width depth)
  (define WORKS 0.9)

  (define (reachable-from prev)
    (for/fold ([acc #f]) ([src (in-list prev)])
      (|| acc (&& src (flip WORKS)))))

  (define last-layer
    (for/fold ([prev (list #t)])   ; the source, reachable by definition
              ([_ (in-range depth)])
      (for/list ([_ (in-range width)])
        (reachable-from prev))))

  (reachable-from last-layer))
