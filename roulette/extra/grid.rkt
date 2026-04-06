#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "lattice.rkt")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

;; lattice test
#;(cost-lattice (make-grid 2) #:iterations 10)

;; cost test
#;(cost (make-grid 200) '() #:budget 2500000 #:wait 0.2 #:iterations 10)
