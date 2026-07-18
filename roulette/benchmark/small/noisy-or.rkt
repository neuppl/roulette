#lang roulette/example/disrupt
(require "../benchmarking.rkt")
(provide main)


(define n0 (flip 0.5))
(define n4 (flip 0.5))
(define n1 (if n0 (flip 0.8) (flip 0.1)))
(define n21 (if n0 (flip 0.8) (flip 0.1)))
(define n22 (if n4 (flip 0.8) (flip 0.1)))
(define n33 (if n4 (flip 0.8) (flip 0.1)))
(define n2 (|| n21 n22))
(define n31 (if n1 (flip 0.8) (flip 0.1)))
(define n32 (if n2 (flip 0.8) (flip 0.1)))
(define n3 (|| n31 n32 n33))

(define (main) (benchmark (query n3)))


(module+ main
  (main))
