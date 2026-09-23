#lang racket/base

(require rackunit "blog-test-support.rkt")

(define run (make-blog-runner 'arithmetic.blog))

(module+ test
  (test-case "arithmetic precedence and associativity"
    (check-equal? (run "query 2 + 3 * 4 - 8;") (hash 6 1))
    (check-equal? (run "query -(2 + 3) * 2;") (hash -10 1)))
  (test-case "division, powers and modulo remain supported"
    (check-equal? (run "query 7 / 2;") (hash 7/2 1))
    (check-equal? (run "query 2 ^ 3;") (hash 8 1))
    (check-equal? (run "query 7 % 2;") (hash 1 1)))
  (test-case "comparisons compose with logic and arithmetic"
    (check-equal?
     (run "query 1 + 2 < 4 & 3 <= 3 & 5 > 4 & 5 >= 5 & 1 != 2;")
     (hash #t 1)))
  (test-case "uniform inclusive bounds and singleton"
    (check-equal? (run "random Integer X ~ UniformInt(-1, 1); query X;")
                  (hash -1 1/3 0 1/3 1 1/3))
    (check-equal? (run "random Integer X ~ UniformInt(4, 4); query X;")
                  (hash 4 1)))
  (test-case "Bernoulli probabilities and endpoints"
    (check-equal? (run "random Integer X ~ Bernoulli(0.25); query X;")
                  (hash 0 0.75 1 0.25))
    (check-equal? (run "random Integer X ~ Bernoulli(0); query X;") (hash 0 1))
    (check-equal? (run "random Integer X ~ Bernoulli(1); query X;") (hash 1 1)))
  (test-case "random arithmetic and observation"
    (check-equal?
     (run "random Integer X ~ UniformInt(1, 4); obs X > 2 = true; query X * X;")
     (hash 9 1/2 16 1/2)))
  (test-case "correlated random bounds and probabilities"
    (check-equal?
     (run "random Integer X ~ Bernoulli(0.5);
           random Integer Y ~ UniformInt(X, X + 1); query Y - X;")
     (hash 0 0.5 1 0.5))
    (check-equal?
     (run "random Integer X ~ Bernoulli(0.5);
           random Integer Y ~ Bernoulli(X); query X == Y;")
     (hash #t 1.0)))
  (test-case "invalid parameters"
    (for ([source '("random Integer X ~ UniformInt(2, 1); query X;"
                    "random Integer X ~ UniformInt(0.5, 2); query X;"
                    "random Integer X ~ Bernoulli(-0.1); query X;"
                    "random Integer X ~ Bernoulli(1.1); query X;")])
      (check-exn exn:fail:contract? (lambda () (run source))))))
