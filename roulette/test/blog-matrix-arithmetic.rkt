#lang racket/base

(require rackunit
         "blog-test-support.rkt"
         (prefix-in blog: "../example/blog.rkt")
         (submod "../example/blog.rkt" test-support))

(define run (make-blog-runner 'matrix-arithmetic.blog))

(define (check-run source expected)
  (check-equal? (run source) (hash expected 1)))

(module+ test
  (test-case "scalar arithmetic and exported arities retain their behavior"
    (check-run "query +2 + 3 * 4 - 8;" 6)
    (check-run "query -(2 + 3) * 2;" -10)
    (check-run "query 7 / 2;" 7/2)
    (check-run "query 2 ^ 3 + 7 % 2;" 9)
    (check-equal? (blog:+) 0)
    (check-equal? (blog:*) 1)
    (check-equal? (blog:+ 1 2 3) 6)
    (check-equal? (blog:- 10 2 3) 5)
    (check-equal? (blog:/ 2) 1/2)
    (check-equal? (blog:/ 12 2 3) 2))

  (test-case "matrix addition, subtraction, unary signs and chained expressions"
    (check-run "fixed RealMatrix a = [1, 2; 3, 4];
                fixed RealMatrix b = [10, 20; 30, 40]; query a + b;"
               (matrix '(11 22) '(33 44)))
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query +a - -a;"
               (matrix '(2 4) '(6 8)))
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query -a;"
               (matrix '(-1 -2) '(-3 -4)))
    (check-run "fixed RealMatrix a = [1, 2]; query a + a + a;" (matrix '(3 6))))

  (test-case "scalar addition and subtraction apply to every matrix cell"
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query a + 10;"
               (matrix '(11 12) '(13 14)))
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query 10 + a;"
               (matrix '(11 12) '(13 14)))
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query a - 10;"
               (matrix '(-9 -8) '(-7 -6)))
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query 10 - a;"
               (matrix '(9 8) '(7 6))))

  (test-case "scalar arithmetic preserves exactness, vector shape and operand order"
    (check-run "fixed RealMatrix a = [1; 2]; query a + 1 / 2;"
               (matrix '(3/2) '(5/2)))
    (check-run "fixed RealMatrix a = [1, 2]; query -0.5 - a;"
               (matrix '(-1.5 -2.5)))
    (check-run "fixed RealMatrix a = [1, 2]; query 10 - a - 2;"
               (matrix '(7 6))))

  (test-case "scalar arithmetic retains empty matrix dimensions"
    (for ([source '("query zeros(0, 3) + 2;" "query 2 + zeros(0, 3);"
                    "query zeros(0, 3) - 2;" "query 2 - zeros(0, 3);")])
      (check-run source (blog-matrix-value 0 3 '())))
    (check-run "query 2 - zeros(2, 0);" (matrix '() '())))

  (test-case "scalar arithmetic preserves correlations with matrix entries"
    (check-run "random Integer x ~ UniformInt(0, 1); random RealMatrix a = [x, x + 1];
                query a - x;" (matrix '(0 1)))
    (check-run "random Integer x ~ UniformInt(0, 1); random RealMatrix a = [x, x + 1];
                query x - a;" (matrix '(0 -1)))
    (check-equal?
     (run "random Integer x ~ UniformInt(0, 1); random RealMatrix a = [x, x + 1];
           query a + x;")
     (hash (matrix '(0 1)) 1/2 (matrix '(2 3)) 1/2))
    (check-run "random Integer x ~ UniformInt(0, 1);
                fixed Other a = if x == 0 then 2 else eye(2);
                query a + eye(2) == (if x == 0 then ones(2, 2) * 2 + eye(2)
                                    else 2 * eye(2));" #t))

  (test-case "scalar addition and subtraction reject nonnumeric alternatives"
    (for ([source '("query true + eye(2);" "query eye(2) - null;"
                    "query [1] + eye(2);"
                    "random Integer x ~ UniformInt(0, 1);
                     fixed Other s = if x == 0 then 1 else false; query eye(2) + s;")])
      (check-exn exn:fail:contract? (lambda () (run source)) source)))

  (test-case "matrix multiplication is a dot product, not elementwise multiplication"
    (check-run "fixed RealMatrix a = [1, 2, 3; 4, 5, 6];
                fixed RealMatrix b = [7, 8; 9, 10; 11, 12]; query a * b;"
               (matrix '(58 64) '(139 154)))
    (check-run "fixed RealMatrix a = [1, 2]; fixed RealMatrix b = [3; 4]; query a * b;"
               (matrix '(11)))
    (check-run "fixed RealMatrix a = [1; 2]; fixed RealMatrix b = [3, 4]; query a * b;"
               (matrix '(3 4) '(6 8))))

  (test-case "scalar multiplication in both orders and matrix/scalar division"
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query 2 * a;"
               (matrix '(2 4) '(6 8)))
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query a * 2;"
               (matrix '(2 4) '(6 8)))
    (check-run "fixed RealMatrix a = [1, 2]; query a / 2;" (matrix '(1/2 1))))

  (test-case "identity, zero, and one constructors"
    (check-run "query eye(3);" (matrix '(1 0 0) '(0 1 0) '(0 0 1)))
    (check-run "query zeros(2, 3);" (matrix '(0 0 0) '(0 0 0)))
    (check-run "query ones(2, 1);" (matrix '(1) '(1)))
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query eye(2) * a == a;" #t))

  (test-case "diagonal construction requires a column vector"
    (check-run "fixed RealMatrix a = [2; 3; 4]; query diag(a);"
               (matrix '(2 0 0) '(0 3 0) '(0 0 4)))
    (check-run "fixed RealMatrix a = [5]; query diag(a);" (matrix '(5))))

  (test-case "repmat tiles rather than repeating individual cells"
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query repmat(a, 2, 3);"
               (matrix '(1 2 1 2 1 2) '(3 4 3 4 3 4)
                       '(1 2 1 2 1 2) '(3 4 3 4 3 4))))

  (test-case "stacking scalars and compatible matrices"
    (check-run "query hstack(1, 2, 3);" (matrix '(1 2 3)))
    (check-run "query vstack(1, 2, 3);" (matrix '(1) '(2) '(3)))
    (check-run "fixed RealMatrix a = [1; 2]; fixed RealMatrix b = [3, 4; 5, 6];
                query hstack(a, b);" (matrix '(1 3 4) '(2 5 6)))
    (check-run "fixed RealMatrix a = [1, 2]; fixed RealMatrix b = [3, 4; 5, 6];
                query vstack(a, b);" (matrix '(1 2) '(3 4) '(5 6)))
    (check-run "fixed RealMatrix a = [1, 2]; query hstack(a, 3);" (matrix '(1 2 3))))

  (test-case "column-wise sums and square-matrix trace"
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query sum(a);" (matrix '(4 6)))
    (check-run "fixed RealMatrix a = [1, 2, 3]; query sum(a);" (matrix '(1 2 3)))
    (check-run "fixed RealMatrix a = [1; 2; 3]; query sum(a);" (matrix '(6)))
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query trace(a);" 5))

  (test-case "absolute value and exponential apply elementwise or to scalars"
    (check-run "fixed RealMatrix a = [-1, 2; -3, 4]; query abs(a);"
               (matrix '(1 2) '(3 4)))
    (check-run "query abs(-3);" 3)
    (check-run "fixed RealMatrix a = [0, 1]; query exp(a);" (matrix (list 1 (exp 1))))
    (check-run "query exp(0);" 1))

  (test-case "determinant uses pivoting and accepts singular matrices"
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query det(a);" -2)
    (check-run "fixed RealMatrix a = [0, 1; 2, 3]; query det(a);" -2)
    (check-run "fixed RealMatrix a = [1, 2; 2, 4]; query det(a);" 0)
    (check-run "fixed RealMatrix a = [7]; query det(a);" 7)
    (check-run "fixed RealMatrix a = [2, 1, 3; 0, 4, 5; 1, 0, 6]; query det(a);" 41))

  (test-case "inverse preserves exact arithmetic and handles row swaps"
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query inv(a);"
               (matrix '(-2 1) '(3/2 -1/2)))
    (check-run "fixed RealMatrix a = [0, 1; 2, 3]; query inv(a) * a == eye(2);" #t)
    (check-run "fixed RealMatrix a = [2, 1, 3; 0, 4, 5; 1, 0, 6];
                query a * inv(a) == eye(3);" #t)
    (check-run "fixed RealMatrix a = [2]; query inv(a);" (matrix '(1/2))))

  (test-case "empty shapes preserve dimensions across operations"
    (check-run "query eye(0);" (matrix))
    (check-run "query zeros(0, 3);" (blog-matrix-value 0 3 '()))
    (check-run "query ones(2, 0);" (matrix '() '()))
    (check-run "query zeros(2, 0) * zeros(0, 3);" (matrix '(0 0 0) '(0 0 0)))
    (check-run "query zeros(0, 2) + zeros(0, 2);" (blog-matrix-value 0 2 '()))
    (check-run "query sum(zeros(0, 3));" (matrix '(0 0 0)))
    (check-run "query trace(eye(0));" 0)
    (check-run "query det(eye(0));" 1)
    (check-run "query inv(eye(0));" (matrix))
    (check-run "query diag(zeros(0, 1));" (matrix))
    (check-run "query repmat(ones(2, 1), 0, 3);" (blog-matrix-value 0 3 '()))
    (check-run "query repmat(zeros(0, 2), 3, 2);" (blog-matrix-value 0 4 '()))
    (check-run "query hstack(zeros(0, 2), zeros(0, 3));" (blog-matrix-value 0 5 '())))

  (test-case "random cells and arithmetic preserve correlations"
    (check-equal?
     (run "random Integer x ~ UniformInt(0, 1); random RealMatrix a = [x, 1; 0, x];
           query toInt((a * a)[0][1]);")
     (hash 0 1/2 2 1/2))
    (check-run "random Integer x ~ UniformInt(0, 1); random RealMatrix a = [x, x];
                query a - a == zeros(1, 2);" #t)
    (check-equal?
     (run "random Integer x ~ UniformInt(0, 1); random RealMatrix a = [x; x];
           query trace(diag(a));")
     (hash 0 1/2 2 1/2)))

  (test-case "random dimensions and correlated operand shapes remain valid"
    (check-run "random Integer n ~ UniformInt(1, 2);
                query eye(n) * ones(n, 1) == ones(n, 1);" #t)
    (check-run "random Integer n ~ UniformInt(1, 2);
                query ones(n + 1, 1) + zeros(n + 1, 1) == ones(n + 1, 1);" #t)
    (check-run "random Integer n ~ UniformInt(1, 2);
                query hstack(ones(n, 1), zeros(n, 1)) == transpose(vstack(ones(1, n), zeros(1, n)));"
               #t))

  (test-case "scalar versus matrix alternatives dispatch correctly"
    (check-run "random Integer x ~ UniformInt(0, 1);
                fixed Other a = if x == 0 then 2 else eye(2);
                query a * eye(2) == (if x == 0 then 2 * eye(2) else eye(2));" #t))

  (test-case "determinant and inverse preserve finite random alternatives"
    (check-equal?
     (run "random Integer x ~ UniformInt(1, 2); random RealMatrix a = [x, 1; 0, x];
           query det(a);")
     (hash 1 1/2 4 1/2))
    (check-run "random Integer x ~ UniformInt(1, 2); random RealMatrix a = [x, 1; 0, x];
                query inv(a) * a == eye(2);" #t))

  (test-case "elementwise functions preserve finite scalar arithmetic"
    (check-equal? (run "random Integer x ~ UniformInt(0, 1); query exp(x + 1);")
                  (hash (exp 1) 1/2 (exp 2) 1/2))
    (check-equal? (run "random Integer x ~ UniformInt(-1, 1); query abs(x);")
                  (hash 0 1/3 1 2/3)))

  (test-case "matrix arithmetic composes with observation and indexing"
    (check-equal?
     (run "random Integer x ~ UniformInt(0, 2); random RealMatrix a = [x, 1; 0, x];
           obs det(a) = 4; query toInt((a + eye(2))[0][0]);")
     (hash 3 1)))

  (test-case "bad dimensions, types, singular matrices and zero divisors are rejected"
    (for ([source '("query zeros(-1, 2);" "query ones(1.5, 2);" "query eye(true);"
                    "query eye(2) + ones(1, 2);"
                    "query eye(2) * ones(3, 1);"
                    "query eye(2) / 0;" "query 1 / 0;"
                    "query eye(2) / eye(2);" "query 1 / eye(2);"
                    "query [1] + [2];" "query true * eye(2);"
                    "query diag(ones(1, 2));" "query repmat(eye(2), -1, 2);"
                    "query repmat([1], 2, 2);" "query hstack();" "query vstack();"
                    "query hstack(ones(1, 2), ones(2, 1));"
                    "query vstack(ones(1, 2), ones(2, 1));"
                    "query sum([1]);" "query trace(ones(2, 3));"
                    "query det(ones(2, 3));" "query inv(ones(2, 3));"
                    "query inv(ones(2, 2));" "query abs([1]);" "query exp(true);")])
      (check-exn exn:fail:contract? (lambda () (run source)) source)))

  (test-case "invalid reachable alternatives raise instead of being discarded"
    (for ([source '("random Integer n ~ UniformInt(1, 2); query eye(n) + eye(1);"
                    "random Integer n ~ UniformInt(1, 2); query eye(n) * ones(1, 1);"
                    "random Integer n ~ UniformInt(-1, 1); query eye(n);"
                    "random Integer n ~ UniformInt(0, 1); query eye(2) / n;"
                    "random Integer n ~ UniformInt(0, 1);
                     random RealMatrix a = [n, 0; 0, 1]; query inv(a);"
                    "random Integer n ~ UniformInt(1, 2); query hstack(eye(n), eye(1));"
                    "random Integer n ~ UniformInt(1, 2); query trace(ones(n, 1));")])
      (check-exn exn:fail:contract? (lambda () (run source)) source))))
