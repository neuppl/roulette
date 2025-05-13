#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base
                       syntax/parse)
           racket/sandbox
           roulette
           "../example/private/pmf.rkt"
           "util.rkt")

  ;; sandbox
  (sandbox-namespace-specs
   (append (sandbox-namespace-specs)
           '(roulette "../example/private/pmf.rkt")))
  (define eval
    (call-with-trusted-sandbox-configuration
     (λ ()
       (make-evaluator 'roulette/example/disrupt))))

  ;; util
  (define-syntax check-program
    (syntax-parser
      [(_ prog ([val pr] ...))
       (syntax/loc this-syntax
         (check-close (pmf->hash (eval 'prog))
                      (hash (~@ val pr) ...)))]))

  (define (pmf->hash pmf)
    (for/hash ([(val pr) (in-pmf pmf)])
      (values val pr)))

  ;; tests
  (check-program
   (flip 1/2)
   ([#t 1/2] [#f 1/2]))

  (check-program
   (not (flip 0.5))
   ([#t 1/2] [#f 1/2]))

  (check-program
   (let ([x (flip 0.5)]
         [y (flip 0.7)])
     x)
   ([#t 1/2] [#f 1/2]))

  (check-program
   (let ([x (flip 0.5)]
         [y (flip 0.7)])
     (and x y))
   ([#t 0.35] [#f 0.65]))

  (check-program
   (let ([x (flip 0.5)]
         [y (flip 0.7)])
     (or x y))
   ([#t 0.85] [#f 0.15]))

  (check-program
   (let ([x (flip 0.5)])
     (if x (flip 0.5) (flip 0.7)))
   ([#t 0.6] [#f 0.4]))

  (check-program
   (let ([x (flip 0.6)]
         [y (flip 0.3)])
     (observe! (or x y))
     x)
   ([#t 5/6] [#f 1/6]))

  (check-program
   (let* ([x (flip 0.7)]
          [y (if x (flip 0.5) (flip 0.3))])
     (observe! y)
     x)
   ([#t 35/44] [#f 9/44]))

  (check-program
   (let* ([x (flip 0.5)]
          [y (flip 0.5)])
     (if x (observe! y) (observe! (not y)))
     (or (and x y) (and (not x) (not y))))
   ([#t 1]))

  (check-program
   (cond
     [(flip 0.5) 'a]
     [(flip 0.5) 'b]
     [else 'c])
   (['a 0.5] ['b 0.25] ['c 0.25]))

  (check-program
   (let ([x (flip 0.5)] [y #f] [z #f])
     (cond
       [x (set! y 'a)]
       [else (set! y 'b) (set! z #t)])
     (list y z))
   (['(a #f) 0.5] ['(b #t) 0.5]))

  (check-program
   (let ([x (flip 1/2)] [y (flip 1/2)])
     (with-observe
       (observe! (or x y)))
     x)
   ([#t 0.5] [#f 0.5]))

  (check-program
   (let ([x (flip 1/2)] [y (flip 1/2)])
     (with-observe
       (observe! (or x y))
       (query x)))
   ([#t 2/3] [#f 1/3]))

  (check-program
   (= (+ (if (flip 1/2) 0 1) (if (flip 1/2) 0 1)) 0)
   ([#t 0.25] [#f  0.75]))
  )
