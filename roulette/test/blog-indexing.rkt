#lang racket/base

(require rackunit
         "blog-test-support.rkt"
         (submod "../example/blog.rkt" test-support))

(define run (make-blog-runner 'indexing.blog))

(module+ test
  (test-case "list indexing returns the stored value, including nested lists"
    (check-equal? (run "query [10, 20, 30][1];") (hash 20 1))
    (check-equal? (run "query [[10, 20], [30, 40]][1];")
                  (hash (blog-list 30 40) 1))
    (check-equal? (run "query [[10, 20], [30, 40]][1][0];") (hash 30 1))
    (check-equal? (run "query [true, false][1];") (hash #f 1))
    (check-equal? (run "type P; distinct P a, b; query [a, b][1] == b;")
                  (hash #t 1)))

  (test-case "matrix indexing preserves matrix type and row orientation"
    (check-equal? (run "fixed RealMatrix m = [10, 20, 30; 40, 50, 60]; query m[1];")
                  (hash (matrix '(40 50 60)) 1))
    (check-equal? (run "fixed RealMatrix m = [10, 20, 30; 40, 50, 60]; query m[1][2];")
                  (hash (matrix '(60)) 1)))

  (test-case "row and column vector indexing returns singleton matrices"
    (for ([literal '("[10, 20, 30]" "[10; 20; 30]")])
      (check-equal? (run (string-append "fixed RealMatrix v = " literal "; query v[1];"))
                    (hash (matrix '(20)) 1)))
    (check-equal? (run "fixed RealMatrix v = [7]; query v[0][0];")
                  (hash (matrix '(7)) 1)))

  (test-case "transpose exchanges dimensions and enables column selection"
    (check-equal? (run "fixed RealMatrix m = [10, 20, 30; 40, 50, 60]; query transpose(m);")
                  (hash (matrix '(10 40) '(20 50) '(30 60)) 1))
    (check-equal? (run "fixed RealMatrix m = [10, 20, 30; 40, 50, 60]; query transpose(m)[1];")
                  (hash (matrix '(20 50)) 1))
    (check-equal? (run "fixed RealMatrix v = [1, 2]; query transpose(v);")
                  (hash (matrix '(1) '(2)) 1))
    (check-equal? (run "fixed RealMatrix m = [1, 2; 3, 4]; query transpose(transpose(m)) == m;")
                  (hash #t 1)))

  (test-case "empty matrices transpose and zero-width rows retain dimensions"
    (check-equal? (run "fixed RealMatrix m = []; query transpose(m);")
                  (hash (matrix) 1))
    (check-equal? (run "fixed RealMatrix m = [[], []]; query transpose(m);")
                  (hash (blog-matrix-value 0 2 '()) 1))
    (check-equal? (run "fixed RealMatrix m = [[], []]; query m[1];")
                  (hash (blog-matrix-value 1 0 '()) 1)))

  (test-case "singleton scalar conversions and truncation toward zero"
    (check-equal? (run "fixed RealMatrix m = [10, 20; 30, 40]; query toReal(m[1][0]);")
                  (hash 30.0 1))
    (check-equal? (run "fixed RealMatrix m = [-3.8]; query toInt(m);") (hash -3 1))
    (check-equal? (run "fixed RealMatrix m = [3.8]; query toInt(m);") (hash 3 1))
    (check-equal? (run "query toReal(2);") (hash 2.0 1))
    (check-equal? (run "query toInt(-3.8);") (hash -3 1))
    (check-equal? (run "query toReal(true);") (hash 1.0 1))
    (check-equal? (run "query toInt(false);") (hash 0 1)))

  (test-case "random row selection composes with scalar conversion"
    (check-equal?
     (run "fixed RealMatrix a = [1, 2; 3, 4];
           random Integer i ~ UniformInt(0, 1);
           random Real x = toReal(a[i][1]); query x;")
     (hash 2.0 1/2 4.0 1/2)))

  (test-case "random indices and matrix cells preserve shared choices"
    (check-equal?
     (run "random Integer i ~ UniformInt(0, 1);
           random RealMatrix a = [i, 10; 20, i]; query toInt(a[i][i]) == i;")
     (hash #t 1))
    (check-equal?
     (run "random Integer i ~ UniformInt(0, 1); query [i, i + 10][i];")
     (hash 0 1/2 11 1/2))
    (check-equal?
     (run "random Integer i ~ UniformInt(0, 1);
           random RealMatrix a = [i, i + 10]; query toInt(transpose(a)[i]) == i + 10 * i;")
     (hash #t 1)))

  (test-case "correlated matrix shapes and indices validate only reachable pairs"
    (check-equal?
     (run "random Integer i ~ UniformInt(0, 1);
           random RealMatrix a = if i == 0 then [7] else [10, 20; 30, 40];
           query toInt(a[i][0]);")
     (hash 7 1/2 30 1/2))
    (check-equal?
     (run "random Integer i ~ UniformInt(0, 1);
           fixed Other a = if i == 0 then [7] else [10, 20]; query a[i];")
     (hash 7 1/2 20 1/2)))

  (test-case "runtime dispatch handles mixtures of families, lists and matrices"
    (check-equal?
     (run "type Person; distinct Person P[2];
           random Integer i ~ UniformInt(0, 1);
           fixed Other a = if i == 0 then P else [P[1]];
           query a[0] == P[i];")
     (hash #t 1))
    (check-equal?
     (run "random Integer i ~ UniformInt(0, 1);
           fixed RealMatrix m = [8];
           fixed Other a = if i == 0 then [8] else m;
           query toInt(a[0]);")
     (hash 8 1)))

  (test-case "arithmetic indices, conversions and observations compose"
    (check-equal?
     (run "fixed RealMatrix m = [10, 20, 30]; random Integer i ~ UniformInt(0, 1);
           query toInt(m[i + 1]) - toInt(m[i]);")
     (hash 10 1))
    (check-equal?
     (run "fixed RealMatrix m = [10, 20]; random Integer i ~ UniformInt(0, 1);
           obs toInt(m[i]) = 20; query i;")
     (hash 1 1))
    (check-equal? (run "random Integer i ~ UniformInt(0, 1); query toInt(i + 0.8);")
                  (hash 0 1/2 1 1/2)))

  (test-case "invalid concrete indices, targets and scalar shapes raise errors"
    (for ([source '("query [1][-1];" "query [1][1];" "query [1][0.5];"
                    "query [1][true];" "query [1][0.0];" "query [][0];"
                    "query 42[0];" "query null[0];"
                    "fixed RealMatrix m = [1, 2; 3, 4]; query m[2];"
                    "fixed RealMatrix m = [1, 2; 3, 4]; query m[0][2];"
                    "fixed RealMatrix m = [1; 2]; query m[2];"
                    "fixed RealMatrix m = []; query m[0];"
                    "fixed RealMatrix m = [[], []]; query m[0][0];"
                    "query transpose([1, 2]);" "query transpose(1);"
                    "fixed RealMatrix m = [1, 2]; query toReal(m);"
                    "fixed RealMatrix m = []; query toInt(m);"
                    "query toInt([1]);" "query toReal(null);")])
      (check-exn exn:fail:contract? (lambda () (run source)) source)))

  (test-case "invalid reachable symbolic alternatives raise instead of disappearing"
    (for ([source '("random Integer i ~ UniformInt(0, 2); query [10, 20][i];"
                    "random Integer i ~ UniformInt(0, 2);
                     fixed RealMatrix m = [1, 2; 3, 4]; query m[i];"
                    "random Integer i ~ UniformInt(0, 1);
                     fixed Other a = if i == 0 then [7] else 42; query a[0];"
                    "random Integer i ~ UniformInt(0, 1);
                     fixed Other a = if i == 0 then [1, 2] else [1]; query a[i];"
                    "random Integer i ~ UniformInt(0, 1);
                     fixed Other a = if i == 0 then 0 else true; query [1, 2][a];"
                    "random Integer i ~ UniformInt(0, 1);
                     fixed RealMatrix m = if i == 0 then [1] else [1, 2]; query toInt(m);"
                    "random Integer i ~ UniformInt(0, 1);
                     fixed RealMatrix m = [1];
                     fixed Other a = if i == 0 then m else [1]; query transpose(a);")])
      (check-exn exn:fail:contract? (lambda () (run source)) source))))
