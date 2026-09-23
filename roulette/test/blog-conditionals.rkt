#lang racket/base

(require rackunit "blog-test-support.rkt")

(define run (make-blog-runner 'conditionals.blog))

(module+ test
  (test-case "optional else returns null"
    (check-equal? (run "query if true then 7;") (hash 7 1))
    (check-equal? (run "query (if false then 7) == null;") (hash #t 1))
    (check-equal? (run "query null == false;") (hash #f 1)))
  (test-case "else binds to the nearest if"
    (check-equal? (run "query if true then if false then 1 else 2;") (hash 2 1))
    (check-equal? (run "query (if false then if true then 1 else 2) == null;") (hash #t 1))
    (check-equal? (run "query if true then 1 + 2 * 3;") (hash 7 1)))
  (test-case "case values and unmatched cases"
    (check-equal? (run "query case 2 in {1 -> 10, 2 -> 20};") (hash 20 1))
    (check-equal? (run "query (case 3 in {1 -> 10, 2 -> 20}) == null;") (hash #t 1)))
  (test-case "unselected branches are not evaluated"
    (check-equal? (run "type T; distinct T P[1]; query if false then P[9] else 7;") (hash 7 1))
    (check-equal? (run "type T; distinct T P[1]; query case 0 in {0 -> 7, 1 -> P[9]};") (hash 7 1)))
  (test-case "symbolic selectors preserve correlation"
    (check-equal?
     (run "random Integer X ~ UniformInt(0, 1);
           fixed Integer Y = case X in {0 -> 10, 1 -> 20};
           query (X == 0) == (Y == 10);") (hash #t 1)))
  (test-case "distribution cases and optional distribution else"
    (check-equal?
     (run "random Integer X ~ UniformInt(0, 1);
           random Integer Y ~ case X in {0 -> Bernoulli(0), 1 -> Bernoulli(1)};
           query X == Y;") (hash #t 1))
    (check-equal?
     (run "random Integer X ~ UniformInt(0, 1);
           random Integer Y ~ if X == 1 then Bernoulli(1);
           query Y == null;") (hash #t 1/2 #f 1/2)))
  (test-case "nested case expressions"
    (check-equal?
     (run "query case 1 in {0 -> 3, 1 -> case true in {true -> if false then 4 else 5}};")
     (hash 5 1))))
