#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base
                       syntax/parse)
           rackunit
           "util.rkt"
           "../example/disrupt/private/pmf.rkt")

  ;; eval
  (define-namespace-anchor here)
  (define anchored-ns (namespace-anchor->namespace here))

  ;; must have rosette's #%top-interaction for set! to work properly
  (define ((make-run lang) datum #:query? [query? #t])
    (define ns (make-base-namespace))
    (namespace-attach-module anchored-ns "../example/disrupt/private/pmf.rkt" ns)
    (parameterize ([current-namespace ns])
      (namespace-require lang)
      (namespace-require `(prefix rosette: rosette))
      (eval (if query?
                `(query (rosette:#%top-interaction . ,datum))
                `(rosette:#%top-interaction . ,datum)))))

  (define run (make-run 'roulette/example/disrupt))
  (define run-safe (make-run 'roulette/example/disrupt/safe))

  ;; util
  (define-syntax check-program
    (syntax-parser
      [(_ prog ([val pr] ...))
       #:with (?eval ...) #'(run run-safe)
       #'(begin
           (check-program-fn ?eval 'prog (hash (~@ val pr) ...)) ...)]))

  (define (check-program-fn ev prog ht)
    (with-check-info (['program prog])
      (check-close (current-ϵ) (pmf-hash (ev prog)) ht)))

  (define current-ϵ (make-parameter 0.0000001))
  (define-syntax-rule (check-program-samples body ... expected)
    (check-program
     (with-sample 2500 body ...)
     expected))

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

  (check-close
   (current-ϵ)
   (pmf-hash
    (run #:query? #f
         '(let ([x (flip 1/2)] [y (flip 1/2)])
            (with-observe
              (observe! (or x y))
              (query x)))))
   (hash #t 2/3 #f 1/3))

  (check-program
   (= (+ (if (flip 1/2) 0 1) (if (flip 1/2) 0 1)) 0)
   ([#t 0.25] [#f 0.75]))

  (check-program
   (let ([x (flip 1/2)])
     (if x ((query x) #t) 'none))
   ([1.0 1/2] ['none 1/2]))

  (check-false
   (run '(observe! #f)))

  ;; samples tests
  (parameterize ([current-ϵ 0.03])
    (check-program-samples
     (let ([x (flip 0.5)])
       (and (sample x) (sample x)))
     ([#t 0.5] [#f 0.5]))

    (check-program-samples
     (let ([x (flip 0.2)]
           [y (flip 0.25)])
       (observe! (or x y))
       (sample y))
     ([#t 0.625] [#f 0.375]))

    (check-program-samples
     (let ([x (flip 0.5)])
       (cond
         [x
          (observe! (or x (flip 0.5)))
          (sample x)]
         [else x]))
     ([#t 0.5] [#f 0.5]))

    (check-program-samples
     (let ([x (flip 0.5)]
           [y (flip 0.5)])
       (observe! (or x y))
       (sample x))
     ([#t 2/3] [#f 1/3]))

    ;; rejection samples
    (check-program-samples
     (let* ([x (flip 0.5)]
            [y (sample x)])
       (observe! x)
       y)
     ([#t 1]))

    ;; nested collapsing
    (check-program-samples
     (let* ([x (flip 1/3)]
            [y (or (sample x) (flip 1/5))])
       (observe! (or x y))
       x)
     ([#t 0.714285714] [#f 0.285714286]))

    ;; full samples via exact
    (check-program-samples
     (let ([x (sample (flip 1/3))]
           [y (sample (flip 1/2))])
       (observe! (or x y))
       y)
     ([#t 3/4] [#f 1/4]))

    ;; sample consistency
    (check-program-samples
     (let* ([x (flip 1/3)]
            [y (sample x)])
       (sample x))
     ([#t 1/3] [#f 2/3]))

    ;; observe under path condition
    (check-program-samples
     (let ([x (flip 0.5)]
           [y (flip 0.5)])
       (cond
         [y
          (observe! x)
          (sample x)]
         [else x]))
     ([#t 2/3] [#f 1/3]))

    (check-program-samples
     (let* ([x (flip 1/3)]
            [y (or (sample x) (flip 1/5))])
       (observe! (or x y))
       x)
     ([#t 0.715] [#f 0.285]))

    ;; sample consistency with `and`
    (check-program-samples
     (let* ([x (flip 1/3)]
            [y (and (sample x) (sample x))])
       y)
     ([#t 1/3] [#f 2/3]))

    ;; observe under conditional with sample
    (check-program-samples
     (define x (sample (flip 1/2)))
     (define y (flip 1/2))
     (when y
       (observe! x))
     (or x y)
     ([#t 2/3] [#f 1/3]))

    ;; generated programs
    (define (make-prog body)
      `(with-sample 2500
         (let* ([x (sample (flip 1/3))]
                [z (flip 1/4)])
           (observe! (or x z))
           (define y (sample z))
           ,body)))

    (check-program-fn run (make-prog 'x) (hash #t 2/3 #f 1/3))
    (check-program-fn run (make-prog 'y) (hash #t 1/2 #f 1/2))
    (check-program-fn run (make-prog '(or x y)) (hash #t 1))
    (check-program-fn run (make-prog '(and x y)) (hash #t 1/6 #f 5/6))
    ))
