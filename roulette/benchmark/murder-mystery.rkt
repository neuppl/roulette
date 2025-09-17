#lang benchmark_ci

(define (mystery)
  (define alice-dunnit (flip 0.3))
  (define with-gun (if alice-dunnit (flip 0.03) (flip 0.8)))
  (list alice-dunnit with-gun))

(define (gun-found-at-scene gun-found)
  (define res (mystery))
  (define alice-dunnit (first res))
  (define with-gun (second res))
  (define obs (if with-gun gun-found (! gun-found)))
  (observe! obs)
  alice-dunnit)

(query (gun-found-at-scene #true))
