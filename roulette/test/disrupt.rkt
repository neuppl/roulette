#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base)
           rackunit
           "util.rkt")

  ;; must have rosette's #%top-interaction for set! to work properly
  (define (run datum #:query? [query? #t])
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require 'roulette/example/disrupt)
      (namespace-require `(prefix rosette: rosette))
      (eval (if query?
                `(pmf-hash (query (rosette:#%top-interaction . ,datum)))
                `(pmf-hash (rosette:#%top-interaction . ,datum))))))

  ;; util
  (define-syntax-rule (check-program prog ([val pr] ...))
    (check-program-fn run 'prog (hash (~@ val pr) ...) #f))

  (define-syntax-rule (check-program-samples body ... ([val pr] ...))
    (check-program-fn run '(with-sample 2500 body ...) (hash (~@ val pr) ...)))

  (define (check-program-fn ev prog ht [tol 0.03])
    (define result (ev prog))
    (with-check-info (['program prog] ['result result])
      (if tol (check-close tol result ht) (check-equal? result ht))))

  ;; tests
  (check-program
   (flip 1/2)
   ([#t 1/2] [#f 1/2]))

  (check-program
   (not (flip 1/2))
   ([#t 1/2] [#f 1/2]))

  (check-program
   (let ([x (flip 1/2)]
         [y (flip 7/10)])
     x)
   ([#t 1/2] [#f 1/2]))

  (check-program
   (let ([x (flip 1/2)]
         [y (flip 7/10)])
     (and x y))
   ([#t 7/20] [#f 13/20]))

  (check-program
   (let ([x (flip 1/2)]
         [y (flip 7/10)])
     (or x y))
   ([#t 17/20] [#f 3/20]))

  (check-program
   (let ([x (flip 1/2)])
     (if x (flip 1/2) (flip 7/10)))
   ([#t 3/5] [#f 2/5]))

  (check-program
   (let ([x (flip 3/5)]
         [y (flip 3/10)])
     (observe! (or x y))
     x)
   ([#t 5/6] [#f 1/6]))

  (check-program
   (let* ([x (flip 7/10)]
          [y (if x (flip 1/2) (flip 3/10))])
     (observe! y)
     x)
   ([#t 35/44] [#f 9/44]))

  (check-program
   (let* ([x (flip 1/2)]
          [y (flip 1/2)])
     (if x (observe! y) (observe! (not y)))
     (or (and x y) (and (not x) (not y))))
   ([#t 1]))

  (check-program
   (cond
     [(flip 1/2) 'a]
     [(flip 1/2) 'b]
     [else 'c])
   (['a 1/2] ['b 1/4] ['c 1/4]))

  (check-program
   (let ([x (flip 1/2)] [y #f] [z #f])
     (cond
       [x (set! y 'a)]
       [else (set! y 'b) (set! z #t)])
     (list y z))
   (['(a #f) 1/2] ['(b #t) 1/2]))

  (check-program
   (let ([x (flip 1/2)] [y (flip 1/2)])
     (with-observe
       (observe! (or x y)))
     x)
   ([#t 1/2] [#f 1/2]))

  (check-equal?
   (run #:query? #f
        '(let ([x (flip 1/2)] [y (flip 1/2)])
           (with-observe
             (observe! (or x y))
             (query x))))
   (hash #t 2/3 #f 1/3))

  (check-program
   (= (+ (if (flip 1/2) 0 1) (if (flip 1/2) 0 1)) 0)
   ([#t 1/4] [#f 3/4]))

  (check-program
   (let ([x (flip 1/2)])
     (if x ((query x) #t) 'none))
  ([1 1/2] ['none 1/2]))

  ;; Should yield an error
  #;(check-false
   ((make-run 'roulette/example/disrupt)
    '(observe! #f)))

  ;; samples tests
  (check-program-samples
   (let ([x (flip 1/2)])
     (and (sample x) (sample x)))
   ([#t 1/2] [#f 1/2]))

  (check-program-samples
   (let ([x (flip 1/5)]
         [y (flip 1/4)])
     (observe! (or x y))
     (sample y))
   ([#t 5/8] [#f 3/8]))

  (check-program-samples
   (let ([x (flip 1/2)])
     (cond
       [x
        (observe! (or x (flip 1/2)))
        (sample x)]
       [else x]))
   ([#t 1/2] [#f 1/2]))

  (check-program-samples
   (let ([x (flip 1/2)]
         [y (flip 1/2)])
     (observe! (or x y))
     (sample x))
   ([#t 2/3] [#f 1/3]))

  ;; rejection samples
  (check-program-samples
   (let* ([x (flip 1/2)]
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
   ([#t 5/7] [#f 2/7]))

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
   (let ([x (flip 1/2)]
         [y (flip 1/2)])
     (cond
       [y
        (observe! x)
        (sample x)]
       [else x]))
   ([#t 2/3] [#f 1/3]))

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
  )
