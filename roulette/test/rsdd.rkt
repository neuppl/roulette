#lang roulette

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base
                       syntax/parse)
           rackunit
           roulette/engine/rsdd
           json
           "util.rkt")

  (require/expose roulette/engine/rsdd
    (mk-bdd-manager-default-order
     new-wmc-params-f64
     new-wmc-params-complex
     rsdd-label rsdd-var
     rsdd-and rsdd-or rsdd-not rsdd-ite
     rsdd-true? rsdd-false? rsdd-equal?
     rsdd-set-real-measure! rsdd-real-wmc
     rsdd-set-complex-measure! rsdd-complex-wmc
     rsdd-num-recursive-calls rsdd-to-json))

  ;; shorthand
  (define-syntax check-program
    (syntax-parser
      [(_ ([x:id rx:expr] ...) def:expr ... body:expr ([y:expr ry:expr] ...))
       (syntax/loc this-syntax
         (check-close 0.0000001
                      (let ()
                        (define-measurable x (bernoulli-measure (- 1 rx) rx)) ...
                        def ...
                        (define inferred (infer body))
                        (for/hash ([elem (in-set (support inferred))])
                          (values elem ((density inferred) elem))))
                      (hash (~@ y ry) ...)))]))

  ;; tests
  (test-case "basic FFI calls"
    (define b (mk-bdd-manager-default-order 0))
    (define rw (new-wmc-params-f64))
    (define cw (new-wmc-params-complex))

    (define x-lab (rsdd-label b))
    (define x (rsdd-var b x-lab))

    (define y-lab (rsdd-label b))
    (define y (rsdd-var b y-lab))

    (define z-lab (rsdd-label b))
    (define z (rsdd-var b z-lab))

    (define w-lab (rsdd-label b))
    (define w (rsdd-var b w-lab))

    (rsdd-set-real-measure! rw x-lab 0.5 0.5)
    (rsdd-set-real-measure! rw y-lab 0.5 0.5)

    (rsdd-set-complex-measure! cw z-lab 0 0+1i)
    (rsdd-set-complex-measure! cw w-lab 0 0+1i)

    (check-equal? (rsdd-real-wmc rw (rsdd-and b x y)) 0.25)
    (check-equal? (rsdd-complex-wmc cw (rsdd-and b z w)) -1.0+0.0i)
    (check-pred exact-nonnegative-integer? (rsdd-num-recursive-calls b))
    (check-equal? (rsdd-real-wmc rw (rsdd-or b x y)) 0.75)
    (check-equal? (rsdd-real-wmc rw (rsdd-not b (rsdd-and b x y))) 0.75)
    (check-equal? (rsdd-real-wmc rw (rsdd-ite b x x (rsdd-not b x))) 1.0)

    (check-equal?
     (string->jsexpr (rsdd-to-json (rsdd-and b x y)))
     #hasheq((nodes . (#hasheq((high . "True") (low . "False") (topvar . 1))
                       #hasheq((high . #hasheq((Ptr . #hasheq((compl . #f) (index . 0)))))
                               (low . "False") (topvar . 0))))
             (roots . (#hasheq((Ptr . #hasheq((compl . #f) (index . 1))))))))

    (check-true (rsdd-equal? b (rsdd-and b x y) (rsdd-and b y x)))
    (check-true (rsdd-true? (rsdd-or b x (rsdd-not b x))))
    (check-true (rsdd-false? (rsdd-and b x (rsdd-not b x)))))

  (test-case "conditionals over atomic data"
    (check-program ([x0 0.1] [y0 0.5])
      (= (if x0 1 2) (if y0 1 3))
      ([#t 0.05] [#f 0.95]))

    (check-program ([x1 0.5] [y1 0.2])
      (if x1 (if y1 1 2) (if y1 3 4))
      ([1 0.1] [2 0.4] [3 0.1] [4 0.4]))

    (check-program ([x2 0.5] [y2 0.2])
      (if x2 y2 (cond [y2 #t] [else 'b]))
      ([#t 0.2] [#f 0.4] ['b 0.4])))

  (test-case "conditionals over lists"
    (check-program ([x1 0.3] [y1 0.1])
      (if x1 (list (if y1 'a 'b)) '())
      (['(a) 0.03] ['(b) 0.27] ['() 0.7]))

    (check-program ([x2 0.3] [y2 0.1])
      (if x2 (list (if y2 'a 'b)) (list (if y2 'b 'a)))
      (['(a) 0.66] ['(b) 0.34])))

  (test-case "conditionals inside operations"
    (check-program ([x1 0.3] [y1 0.1])
      (+ (if x1 1 2) (if y1 3 4))
      ([4 0.03] [5 0.34] [6 0.63])))

  (test-case "recursive function over symbolic lists"
    (check-program ([x1 0.1] [y1 0.2] [z1 0.5])
      (define (sum xs)
        (if (empty? xs) 0 (+ (first xs) (sum (rest xs)))))
      (define xs (cons 1 (if x1 '() (list (if y1 2 3) 4))))
      (define ys '(5 6 7))
      (sum (if z1 xs ys))

      ([18 0.5]
       [1 (* 0.5 0.1)]
       [7 (* 0.5 (- 1 0.1) 0.2)]
       [8 (* 0.5 (- 1 0.1) (- 1 0.2))])))

  (test-case "mutation"
    (check-program ([x1 0.5])
      (define y 10)
      (when x1 (set! y 12))
      y

      ([10 0.5] [12 0.5])))

  (test-case "polynomial"
    (let ()
      (define-measurable x
        (bernoulli-measure '(0.1 0.6) '(0.9 0.4) #:semiring polynomial-semiring))
      (define-measurable y
        (bernoulli-measure '(0.2 0.7) '(0.8 0.3) #:semiring polynomial-semiring))
      (define f
        (density (infer (and x y) #:engine (rsdd-engine #:semiring polynomial-semiring))))
      (check-equal? (f #t) (list (* 0.9 0.8) (+ (* 0.9 0.3) (* 0.4 0.8)) (* 0.4 0.3)))))
  )
