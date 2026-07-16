#lang roulette/example/disrupt
(require "../../../bdd-engine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grid

(define (make-grid n)
  (define grid (make-vector (* n n)))
  (define (get row col)
    (if (and (< -1 row n) (< -1 col n))
        (vector-ref grid (+ (* row n) col))
        #t))
  (for* ([row (in-range n)]
         [col (in-range n)])
    (define node
      (make-node
       (get (- col 1) row)
       (get col (- row 1))))
    (vector-set! grid (+ (* row n) col) node))
  (get (- n 1) (- n 1)))

(define (make-node dep1 dep2)
  (if (and dep1 dep2)
      (flip 0.5)
      (flip 0.4)))



(when (not (equal? bdd-engine-backend "rsdd")) (max-arg make-grid #:start 1 #:end 200 #:timeout 1))
