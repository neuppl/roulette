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
    (rsdd-label rsdd-var rsdd-true rsdd-false
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
         (check-close (let ()
                        (define-measurable x (bernoulli-measure (- 1 rx) rx)) ...
                        def ...
                        (define inferred (infer body))
                        (for/hash ([elem (in-set (support inferred))])
                          (values elem ((density inferred) elem))))
                      (hash (~@ y ry) ...)))]))


  ;; tests
  (test-case "basic FFI calls"
    (define x-lab (rsdd-label))
    (define x (rsdd-var x-lab))

    (define y-lab (rsdd-label))
    (define y (rsdd-var y-lab))

    (define z-lab (rsdd-label))
    (define z (rsdd-var z-lab))

    (define w-lab (rsdd-label))
    (define w (rsdd-var w-lab))

    (rsdd-set-real-measure! x-lab 0.5 0.5)
    (rsdd-set-real-measure! y-lab 0.5 0.5)

    (rsdd-set-complex-measure! z-lab 0 0+1i)
    (rsdd-set-complex-measure! w-lab 0 0+1i)

    (check-equal? (rsdd-real-wmc (rsdd-and x y)) 0.25)
    (check-equal? (rsdd-complex-wmc (rsdd-and z w)) -1.0+0.0i)
    (check-pred exact-nonnegative-integer? (rsdd-num-recursive-calls))
    (check-equal? (rsdd-real-wmc (rsdd-or x y)) 0.75)
    (check-equal? (rsdd-real-wmc (rsdd-not (rsdd-and x y))) 0.75)
    (check-equal? (rsdd-real-wmc (rsdd-ite x x (rsdd-not x))) 1.0)

    (check-equal?
     (string->jsexpr (rsdd-to-json (rsdd-and x y)))
     #hasheq((nodes . (#hasheq((high . "True") (low . "False") (topvar . 1))
                       #hasheq((high . #hasheq((Ptr . #hasheq((compl . #f) (index . 0)))))
                               (low . "False") (topvar . 0))))
             (roots . (#hasheq((Ptr . #hasheq((compl . #f) (index . 1))))))))

    (check-true (rsdd-equal? (rsdd-and x y) (rsdd-and y x)))
    (check-true (rsdd-true? (rsdd-or x (rsdd-not x))))
    (check-true (rsdd-true? rsdd-true))

    (check-true (rsdd-false? (rsdd-and x (rsdd-not x))))
    (check-true (rsdd-false? rsdd-false)))

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
  )
