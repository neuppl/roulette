#lang racket/base

(require rackunit
         parser-tools/lex
         (only-in "../example/blog.rkt" blog-lex type distinct)
         (submod "../example/blog.rkt" test-support)
         "blog-test-support.rkt"
         (only-in (submod "../example/blog.rkt" reader)
                  [read-syntax blog-read-syntax]))

(define parse parse-blog)
(define run (make-blog-runner))

(module+ test
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Parser: declarations, precedence and numeric literals

  (test-case "constants, deterministic random definitions, indexed names"
    (check-equal?
     (parse "distinct Person Alice, P[2]; fixed Boolean Yes() = true;
             random Boolean R = Yes(); obs R = false; query P[0];")
     '((distinct Person Alice (indexed P 2))
       (fixed Boolean Yes () #t)
       (random Boolean R () = (call Yes))
       (obs R #f)
       (query (index P 0)))))

  (test-case "propositional precedence and implication associativity"
    (check-equal?
     (parse "query !a == b & c | d => e => f;")
     `((query (=> (,(string->symbol "|") (& (== (! a) b) c) d)
                  (=> e f)))))
    (check-equal? (parse "query !(a != b);")
                  '((query (! (!= a b)))))
    (check-equal? (parse "query if a then b else c & d;")
                  '((query (if a b (& c d)))))
    (check-equal? (parse "query (if a then b else c) & d;")
                  '((query (& (if a b c) d)))))

  (test-case "decimal forms, exponents and signs"
    (check-equal?
     (parse "query D(0, 1., .5, 1e-2, 2E+1, -0.25);")
     '((query (call D 0 1.0 0.5 0.01 20.0 -0.25)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Lexer and reader: comments, tokens and module wrapping

  (test-case "comments, keyword boundaries and empty input"
    (check-equal? (parse "// no declarations\n/* comment */") '())
    (check-equal? (parse "/* /* non-nesting */query trueValue; // eof")
                  '((query trueValue)))
    (check-equal? (parse "query iffy(_x2, BooleanDistrib(.5));")
                  '((query (call iffy _x2 (call BooleanDistrib 0.5))))))

  (test-case "lexer uses longest tokens and retains positions"
    (define in (open-input-string "query a!=b => true;"))
    (port-count-lines! in)
    (check-equal?
     (for/list ([i (in-range 8)])
       (token-name (position-token-token (blog-lex in))))
     '(query ident unequal ident implies truet semicolon eof)))

  (test-case "reader wraps parsed expressions in the BLOG language module"
    (define module-syntax
      (blog-read-syntax 'model.blog
                        (open-input-string "// heading\nquery !Healthy(Alice);")))
    (check-true (syntax? module-syntax))
    (check-equal? (syntax->datum module-syntax)
                  '(module _ roulette/example/blog
                     (query (! (call Healthy Alice))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Syntax errors: invalid input and source locations

  (test-case "malformed statements, queries and lists report their source"
    (for ([source '("query true" "query X,;" "query [1,];")])
      (check-exn
       (lambda (exn)
         (and (exn:fail:read? exn)
              (pair? (exn:fail:read-srclocs exn))
              (equal? (srcloc-source (car (exn:fail:read-srclocs exn))) 'example.blog)))
       (lambda () (parse source)) source)))

  (test-case "error location after a multiline comment"
    (check-exn
     (lambda (exn)
       (and (exn:fail:read? exn)
            (let ([loc (car (exn:fail:read-srclocs exn))])
              (and (= (srcloc-line loc) 3) (= (srcloc-column loc) 6)))))
     (lambda () (parse "/* one\n two */\nquery @;"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Runtime: type descriptors and distinct object identities

  (test-case "type declarations bind distinct descriptors"
    (type Person)
    (type Ball)
    (check-not-false Person)
    (check-eq? Person Person)
    (check-false (eq? Person Ball)))

  (test-case "distinct binds separate objects with stable identities"
    (type Person)
    (distinct Person Alice Bob Carol)
    (define alice-again Alice)
    (check-eq? Alice alice-again)
    (check-false (equal? Alice Bob))
    (check-false (equal? Alice Carol))
    (check-false (equal? Bob Carol))
    (check-false (eq? Alice Person)))

  (test-case "distinct requires a type descriptor"
    (check-exn
     exn:fail:contract?
     (lambda ()
       (define NotType 'Person)
       (distinct NotType Alice)
       Alice)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Scalar arithmetic and logic

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Distributions and grounded random functions

  (test-case "uniform bounds, Bernoulli probabilities and invalid parameters"
    (check-equal? (run "random Integer X ~ UniformInt(-1, 1); query X;")
                  (hash -1 1/3 0 1/3 1 1/3))
    (check-equal? (run "random Integer X ~ Bernoulli(0.25); query X;")
                  (hash 0 0.75 1 0.25))
    (check-invalid "random Integer X ~ UniformInt(2, 1); query X;"
                   "random Integer X ~ Bernoulli(1.1); query X;"))

  (test-case "grounded random functions reuse draws and remain independent"
    (check-equal?
     (run "type Person; distinct Person P[2];
                     random Integer X(Person p) ~ UniformInt(0, 1);
                     query X(P[0]), X(P[0]), X(P[1]);")
     (hash '(0 0 0) 1/4 '(0 0 1) 1/4 '(1 1 0) 1/4 '(1 1 1) 1/4)))

  (test-case "conditional categorical draws preserve parent correlation"
    (check-equal?
     (run "random Boolean Parent ~ BooleanDistrib(0.5);
                     random Boolean Child ~ if Parent then
                       Categorical({true -> 1, false -> 0})
                       else Categorical({true -> 0.25, false -> 0.75});
                     query Parent, Child;")
     (hash '(#t #t) 0.5 '(#f #t) 0.125 '(#f #f) 0.375)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Conditionals, case expressions and null

  (test-case "optional else returns null"
    (check-equal? (run "query if true then 7;") (hash 7 1))
    (check-equal? (run "query (if false then 7) == null;") (hash #t 1))
    (check-equal? (run "query null == false;") (hash #f 1)))

  (test-case "unselected branches are not evaluated"
    (check-equal? (run "type T; distinct T P[1]; query if false then P[9] else 7;") (hash 7 1))
    (check-equal? (run "type T; distinct T P[1]; query case 0 in {0 -> 7, 1 -> P[9]};") (hash 7 1)))

  (test-case "distribution cases and optional distribution else"
    (check-equal?
     (run "random Integer X ~ UniformInt(0, 1);
           random Integer Y ~ case X in {0 -> Bernoulli(0), 1 -> Bernoulli(1)};
           query X == Y;") (hash #t 1))
    (check-equal?
     (run "random Integer X ~ UniformInt(0, 1);
           random Integer Y ~ if X == 1 then Bernoulli(1);
           query Y == null;") (hash #t 1/2 #f 1/2)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Indexed objects and quantified domains

  (test-case "indexed objects populate the quantified domain"
    (check-equal?
     (run "type Person; distinct Person P[3];
           query (forall Person p (p == P[0] | p == P[1] | p == P[2]))
                 & (exists Person p p == P[2]);")
     (hash #t 1)))

  (test-case "random indices preserve correlation and support arithmetic"
    (check-equal?
     (run "type Person; distinct Person P[3];
           random Integer I ~ UniformInt(0, 2); query P[I] == P[1];")
     (hash #t 1/3 #f 2/3))
    (check-equal?
     (run "type Person; distinct Person P[3];
           random Integer I ~ UniformInt(0, 1);
           query P[I] != P[I + 1];")
     (hash #t 1)))

  (test-case "empty families and invalid indices"
    (check-run "type Person; distinct Person P[0]; query exists Person p true;" #f)
    (check-invalid "type Person; distinct Person P[2]; query P[2];"
                   "type Person; distinct Person P[2]; query P[-1];"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Lists: literals, row shorthand and symbolic values

  (test-case "empty, flat and nested list literals"
    (check-equal? (parse "query []; query [1, 2.5, -3]; query [[], [4]];")
                  '((query (blog-list))
                    (query (blog-list 1 2.5 -3))
                    (query (blog-list (blog-list) (blog-list 4))))))

  (test-case "row shorthand normalizes to nested list expressions"
    (check-equal? (parse "query [1, 2, 3; 4, 5, 6];")
                  (parse "query [[1, 2, 3], [4, 5, 6]];"))
    (check-equal? (parse "query [1; 2; 3];")
                  '((query (blog-list (blog-list 1) (blog-list 2) (blog-list 3)))))
    ;; Rectangularity is a matrix-conversion concern, not a grammar rule.
    (check-equal? (parse "query [1; 2, 3];")
                  '((query (blog-list (blog-list 1) (blog-list 2 3))))))

  (test-case "parsed lists evaluate and preserve symbolic correlations"
    (check-equal? (run "query [1, 2; 3, 4] == [[1, 2], [3, 4]];")
                  (hash #t 1))
    (check-equal? (run "random Integer x ~ UniformInt(0, 1);
                       query [x, x] == [0, 0];")
                  (hash #t 1/2 #f 1/2)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Matrix construction and declarations

  (test-case "matrices retain row-major storage and empty dimensions"
    (define square (matrix '(1 2) '(3 4)))
    (check-equal? square (blog-matrix-value 2 2 '(1 2 3 4)))
    (check-eq? (make-blog-matrix square) square)
    (check-equal? (matrix '() '()) (blog-matrix-value 2 0 '()))
    (check-false (equal? (blog-list 1 2) (matrix '(1 2)))))

  (test-case "fixed functions and random draws convert lists into matrices"
    (check-run "fixed RealMatrix f(Integer x, Integer y) = [x, y]; query f(1, 2);"
               (matrix '(1 2)))
    (check-equal?
     (run "random Integer x ~ UniformInt(0, 1);
           random RealMatrix a = [x, x]; query a;")
     (hash (matrix '(0 0)) 1/2 (matrix '(1 1)) 1/2))
    (check-equal?
     (run "random RealMatrix a ~ Categorical({[1, 2] -> 0.5, [3, 4] -> 0.5}); query a;")
     (hash (matrix '(1 2)) 0.5 (matrix '(3 4)) 0.5)))

  (test-case "matrix declarations reject ragged rows and nonnumeric cells"
    (check-invalid "fixed RealMatrix a = [1; 2, 3]; query a;"
                   "random RealMatrix a ~ Categorical({[true] -> 1}); query a;"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Indexing, transpose and scalar conversion

  (test-case "list and matrix indexing preserve their value types"
    (check-run "query [[10, 20], [30, 40]][1];" (blog-list 30 40))
    (check-run "fixed RealMatrix m = [10, 20; 30, 40]; query m[1];"
               (matrix '(30 40)))
    (check-run "fixed RealMatrix m = [10, 20; 30, 40]; query m[1][0];"
               (matrix '(30))))

  (test-case "transpose and conversions compose with correlated random indices"
    (check-run "fixed RealMatrix m = [10, 20; 30, 40]; query transpose(m)[1];"
               (matrix '(20 40)))
    (check-run "random Integer i ~ UniformInt(0, 1);
                random RealMatrix a = [i, 10; 20, i]; query toInt(a[i][i]) == i;" #t)
    (check-run "fixed RealMatrix m = [-3.8]; query toInt(m);" -3)
    (check-run "fixed RealMatrix m = [2]; query toReal(m);" 2.0))

  (test-case "invalid indices and reachable nonsingleton conversions raise errors"
    (check-invalid "query [1][0.5];"
                   "random Integer i ~ UniformInt(0, 2); query [10, 20][i];"
                   "random Integer i ~ UniformInt(0, 1);
                    fixed RealMatrix m = if i == 0 then [1] else [1, 2]; query toInt(m);"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Matrix arithmetic

  (test-case "matrix arithmetic preserves shape and symbolic correlations"
    (check-run "fixed RealMatrix a = [1, 2; 3, 4]; query +a - -a;"
               (matrix '(2 4) '(6 8)))
    (check-run "fixed RealMatrix a = [1, 2]; query (10 - a) / 2;" (matrix '(9/2 4)))
    (check-run "random Integer x ~ UniformInt(0, 1);
                random RealMatrix a = [x, x + 1]; query a - x;" (matrix '(0 1))))

  (test-case "matrix multiplication is a dot product, not elementwise multiplication"
    (check-run "fixed RealMatrix a = [1, 2, 3; 4, 5, 6];
                fixed RealMatrix b = [7, 8; 9, 10; 11, 12]; query a * b;"
               (matrix '(58 64) '(139 154)))
    (check-run "fixed RealMatrix a = [1, 2]; fixed RealMatrix b = [3; 4]; query a * b;"
               (matrix '(11)))
    (check-run "fixed RealMatrix a = [1; 2]; fixed RealMatrix b = [3, 4]; query a * b;"
               (matrix '(3 4) '(6 8))))

  (test-case "incompatible dimensions and reachable zero divisors raise errors"
    (check-invalid "query eye(2) + ones(1, 2);" "query eye(2) * ones(3, 1);"
                   "random Integer n ~ UniformInt(0, 1); query eye(2) / n;"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Matrix utility functions

  (test-case "constructors, tiling and stacking compose"
    (check-run "query hstack(eye(2), zeros(2, 1));" (matrix '(1 0 0) '(0 1 0)))
    (check-run "query vstack(ones(1, 2), zeros(1, 2));" (matrix '(1 1) '(0 0)))
    (check-run "fixed RealMatrix a = [1, 2]; query repmat(a, 2, 2);"
               (matrix '(1 2 1 2) '(1 2 1 2))))

  (test-case "reductions, diagonal and elementwise operations"
    (check-run "fixed RealMatrix a = [-1, 2; -3, 4]; query sum(abs(a));"
               (matrix '(4 6)))
    (check-run "fixed RealMatrix a = [2; 3]; query trace(diag(a));" 5)
    (check-run "fixed RealMatrix a = [0, 1]; query exp(a);" (matrix (list 1 (exp 1)))))

  (test-case "determinant pivoting, inverse and singular-matrix validation"
    (check-run "fixed RealMatrix a = [0, 1; 2, 3]; query det(a);" -2)
    (check-run "random Integer x ~ UniformInt(1, 2);
                random RealMatrix a = [x, 1; 0, x]; query inv(a) * a == eye(2);" #t)
    (check-invalid "query inv(ones(2, 2));" "query trace(ones(2, 3));"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Module execution: statement order, joint queries and observations

  (test-case "empty modules are silent and certain results retain statement order"
    (define-values (empty-results empty-output) (module-results ""))
    (check-equal? empty-results '())
    (check-equal? empty-output "")
    (define-values (certain output) (module-results "query 42; query 1, 2; query 42;"))
    (check-equal? certain '(42 (1 2) 42))
    (check-equal? output ""))

  (test-case "BLOG modules print joint distributions before and after observation"
    (define-values (certain output)
      (module-results "random Integer X ~ UniformInt(0, 1); query X, X + X;
                       obs X = 1; query X; query X, X + X;"))
    (check-equal? certain '(1 (1 2)))
    (check-equal? (length (regexp-match* #rx"1/2" output)) 2)
    (check-equal? (length (regexp-match* #rx"Probability" output)) 1)
    (check-true (regexp-match? #rx"0 0" output))
    (check-true (regexp-match? #rx"1 2" output)))

  (test-case "test module observations are scoped even when execution fails"
    (check-exn
     exn:fail:contract?
     (lambda ()
       (module-results "random Integer X ~ UniformInt(0, 1);
                        obs X = 1; query [1][2];")))
    (define-values (certain output)
      (module-results "random Integer X ~ UniformInt(0, 1); query X;"))
    (check-equal? certain '())
    (check-equal? (length (regexp-match* #rx"1/2" output)) 2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; End-to-end inference

  (test-case "observations and quantified random functions infer the posterior"
    (define result
      (run "type Person; distinct Person Alice, Bob;
            random Real prob ~ Categorical({0.1 -> 0.5, 0.9 -> 0.5});
            random Boolean Smokes(Person p) ~ BooleanDistrib(prob);
            random Boolean Infected(Person p) ~ BooleanDistrib(prob);
            random Boolean ExtraHealthy(Person p) ~
              if (exists Person q Infected(q)) then BooleanDistrib(0.2)
              else BooleanDistrib(0.8);
            obs Smokes(Alice) = true; query ExtraHealthy(Alice);"))
    (check-= (hash-ref result #t) 0.254 0.000001)
    (check-= (hash-ref result #f) 0.746 0.000001)))
