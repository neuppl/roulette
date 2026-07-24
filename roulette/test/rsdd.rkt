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
     rsdd-label rsdd-var
     rsdd-and rsdd-or rsdd-not
     rsdd-true? rsdd-false? rsdd-equal?
     rsdd-num-recursive-calls rsdd-to-json))

  ;; shorthand
  (define-syntax check-program
    (syntax-parser
      [(_ ([x:id rx:expr] ...) def:expr ... body:expr ([y:expr ry:expr] ...))
       (syntax/loc this-syntax
         (check-equal?
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

    (define x-lab (rsdd-label b))
    (define x (rsdd-var b x-lab))

    (define y-lab (rsdd-label b))
    (define y (rsdd-var b y-lab))

    (define z-lab (rsdd-label b))
    (define z (rsdd-var b z-lab))

    (define w-lab (rsdd-label b))
    (define w (rsdd-var b w-lab))

    (check-pred exact-nonnegative-integer? (rsdd-num-recursive-calls b))
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
    (check-program ([x0 1/10] [y0 1/2])
      (= (if x0 1 2) (if y0 1 3))
      ([#t 1/20] [#f 19/20]))

    (check-program ([x1 1/2] [y1 1/5])
      (if x1 (if y1 1 2) (if y1 3 4))
      ([1 1/10] [2 2/5] [3 1/10] [4 2/5]))

    (check-program ([x2 1/2] [y2 1/5])
      (if x2 y2 (cond [y2 #t] [else 'b]))
      ([#t 1/5] [#f 2/5] ['b 2/5])))

  (test-case "conditionals over lists"
    (check-program ([x1 3/10] [y1 1/10])
      (if x1 (list (if y1 'a 'b)) '())
      (['(a) #e0.03] ['(b) #e0.27] ['() #e0.7]))

    (check-program ([x2 3/10] [y2 1/10])
      (if x2 (list (if y2 'a 'b)) (list (if y2 'b 'a)))
      (['(a) #e0.66] ['(b) #e0.34])))

  (test-case "conditionals inside operations"
    (check-program ([x1 3/10] [y1 1/10])
      (+ (if x1 1 2) (if y1 3 4))
      ([4 #e0.03] [5 #e0.34] [6 #e0.63])))

  (test-case "recursive function over symbolic lists"
    (check-program ([x1 1/10] [y1 1/2] [z1 1/2])
      (define (sum xs)
        (if (empty? xs) 0 (+ (first xs) (sum (rest xs)))))
      (define xs (cons 1 (if x1 '() (list (if y1 2 3) 4))))
      (define ys '(5 6 7))
      (sum (if z1 xs ys))

      ([18 1/2]
       [1 (* 1/2 1/10)]
       [7 (* 1/2 (- 1 1/10) 1/2)]
       [8 (* 1/2 (- 1 1/10) (- 1 1/2))])))

  (test-case "mutation"
    (check-program ([x1 1/2])
      (define y 10)
      (when x1 (set! y 12))
      y
      ([10 1/2] [12 1/2])))

  (test-case "polynomial"
    (let ()
      (define number-polynomial-semiring (polynomial-semiring number-semiring))
      (define-measurable x
        (bernoulli-measure '(#e0.1 #e0.6) '(#e0.9 #e0.4)
                           #:semiring number-polynomial-semiring))
      (define-measurable y
        (bernoulli-measure '(#e0.2 #e0.7) '(#e0.8 #e0.3)
                           #:semiring number-polynomial-semiring))
      (define f
        (density (infer (and x y) #:engine (rsdd-engine #:semiring number-polynomial-semiring))))
      (check-equal? (f #t) (list (* #e0.9 #e0.8) (+ (* #e0.9 #e0.3) (* #e0.4 #e0.8)) (* #e0.4 #e0.3)))))
  )
