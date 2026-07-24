#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base)
           rackunit
           "util.rkt")

  (define (run datum)
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require 'roulette/example/bentham)
      (eval `(expected-utility ,datum))))

  ;; util
  (define-syntax-rule (check-program prog answer)
    (check-program-fn run 'prog answer))

  (define (check-program-fn ev prog answer)
    (define result (ev prog))
    (with-check-info (['program prog] ['result result])
      (check-equal? result answer)))

  ;; tests
  (check-program
   (let ()
     (define x (flip 1/2))
     (define y (flip 1/2))
     (if x
         (if y (reward! 2) (reward! 3))
         (reward! 4))
     #t)
   13/4)
  )
