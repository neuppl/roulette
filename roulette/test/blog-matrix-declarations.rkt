#lang racket/base

(require rackunit
         "blog-test-support.rkt"
         (submod "../example/blog.rkt" test-support))

(define run (make-blog-runner 'matrix-declarations.blog))

(define row (matrix '(1 2)))
(define square (matrix '(1 2) '(3 4)))

(module+ test
  (test-case "fixed constants convert lists to matrix values"
    (check-equal? (run "fixed RealMatrix a = [1, 2]; query a;") (hash row 1))
    (check-equal? (run "fixed RealMatrix a() = [1, 2; 3, 4]; query a();")
                  (hash square 1))
    (check-equal? (run "fixed RealMatrix a = [[1, 2], [3, 4]]; query a;")
                  (hash square 1))
    (check-equal? (run "fixed RealMatrix a = []; query a;")
                  (hash (make-blog-matrix (blog-list)) 1)))

  (test-case "fixed function results convert on each call"
    (check-equal?
     (run "fixed RealMatrix f(Integer x, Integer y) = [x, y]; query f(1, 2);")
     (hash row 1))
    (check-exn exn:fail:contract?
               (lambda ()
                 (run "fixed RealMatrix f(Boolean x) = [x]; query f(true);"))))

  (test-case "existing matrices pass through return annotations"
    (check-equal?
     (run "fixed RealMatrix a = [1, 2];
           fixed RealMatrix f() = a; random RealMatrix b = f(); query b;")
     (hash row 1)))

  (test-case "deterministic random constants preserve parent correlations"
    (define zero (make-blog-matrix (blog-list 0 0)))
    (define one (make-blog-matrix (blog-list 1 1)))
    (check-equal?
     (run "random Integer x ~ UniformInt(0, 1);
           random RealMatrix a = [x, x]; query a;")
     (hash zero 1/2 one 1/2))
    (check-equal?
     (run "random Integer x ~ UniformInt(0, 1);
           fixed RealMatrix f(Integer y) = [y, y];
           random RealMatrix a = f(x); query a == f(x);")
     (hash #t 1)))

  (test-case "distribution draws convert selected lists into matrices"
    (check-equal?
     (run "random RealMatrix a ~ Categorical({[1, 2] -> 0.5,
                                              [3, 4] -> 0.5}); query a;")
     (hash row 0.5 (make-blog-matrix (blog-list 3 4)) 0.5))
    (check-equal?
     (run "fixed RealMatrix a = [1, 2];
           random RealMatrix b ~ Categorical({a -> 1}); query b;")
     (hash row 1)))

  (test-case "grounded matrix draws are independent per object and reused"
    (check-equal?
     (run "type Person; distinct Person Alice, Bob;
           random RealMatrix a(Person p) ~
             Categorical({[0] -> 0.5, [1] -> 0.5});
           query a(Alice) == a(Bob);")
     (hash #t 0.5 #f 0.5))
    (check-equal?
     (run "type Person; distinct Person Alice, Bob;
           random RealMatrix a(Person p) ~
             Categorical({[0] -> 0.5, [1] -> 0.5});
           random RealMatrix b(Person p) = a(p);
           obs a(Alice) = b(Alice);
           query a(Alice) == a(Alice) & b(Alice) == a(Alice);")
     (hash #t 1)))

  (test-case "matrix validation applies to all declaration forms"
    (for ([source '("fixed RealMatrix a = 1; query a;"
                    "fixed RealMatrix a = [1; 2, 3]; query a;"
                    "fixed RealMatrix a = [true]; query a;"
                    "random RealMatrix a = [false]; query a;"
                    "random RealMatrix a ~ Categorical({[true] -> 1}); query a;"
                    "type T; distinct T t;
                     random RealMatrix a(T x) = [false]; query a(t);"
                    "type T; distinct T t;
                     random RealMatrix a(T x) ~ Categorical({[false] -> 1});
                     query a(t);")])
      (check-exn exn:fail:contract? (lambda () (run source)) source)))

  (test-case "other annotations retain existing behavior and lists stay lists"
    (check-equal? (run "fixed Other a = [1, 2]; query a;")
                  (hash (blog-list 1 2) 1))
    (check-equal? (run "random Other a = [1, 2]; query a;")
                  (hash (blog-list 1 2) 1))
    (check-equal? (run "fixed RealMatrix a = [1, 2]; query a == [1, 2];")
                  (hash #f 1))))
