#lang racket/base

(require rackunit
         racket/list
         racket/port
         (only-in "../example/disrupt/core.rkt" call-with-query-session)
         (only-in "../example/blog.rkt"
                  blog-run blog-query-result-number blog-query-result-expressions
                  blog-query-result-probabilities)
         (only-in (submod "../example/blog.rkt" reader) [read-syntax blog-read-syntax])
         "blog-test-support.rkt")

(define (run source)
  (blog-run 'queries.blog (open-input-string source)))
(define probabilities (make-blog-multi-runner 'queries.blog))

;; Exercise actual BLOG module execution, including its automatic printer.
;; A fresh session keeps definitions and evidence isolated between fixtures.
(define-namespace-anchor anchor)
(define (module-results source)
  (define ns (make-base-namespace))
  (namespace-attach-module (namespace-anchor->namespace anchor)
                           'roulette/example/blog ns)
  (define name (gensym 'query-module))
  (define module-datum
    (syntax->datum (blog-read-syntax 'queries.blog (open-input-string source))))
  (define results '())
  (define output (open-output-string))
  (parameterize ([current-namespace ns]
                 [current-output-port output]
                 [current-print (lambda (value)
                                  (unless (void? value)
                                    (set! results (cons value results))))])
    (call-with-query-session
     (lambda (_infer)
       (eval `(module ,name ,(caddr module-datum) ,@(cdddr module-datum)))
       (dynamic-require `(quote ,name) #f))))
  (values (reverse results) (get-output-string output)))

(module+ test
  (test-case "query syntax distinguishes scalar, joint, and list-valued queries"
    (check-equal? (parse-blog "query X; query X, X + X; query [X, X];")
                  '((query X) (query X (+ X X)) (query (blog-list X X))))
    (check-equal? (parse-blog "query f(1, 2), [3, 4], (a + b)[0];")
                  '((query (call f 1 2) (blog-list 3 4) (index (+ a b) 0)))))

  (test-case "empty and trailing joint components are located parse errors"
    (for ([source '("query;" "query , X;" "query X,;" "query X,, Y;")])
      (check-exn
       (lambda (exn)
         (and (exn:fail:read? exn)
              (equal? (srcloc-source (car (exn:fail:read-srclocs exn))) 'queries.blog)))
       (lambda () (run source)) source)))

  (test-case "zero queries returns an empty result collection"
    (check-equal? (run "") '())
    (check-equal? (run "type T; distinct T t; random Integer x ~ UniformInt(0, 1);") '()))

  (test-case "single queries retain scalar outcomes and record expression metadata"
    (define results (run "query 42;"))
    (check-equal? (length results) 1)
    (check-equal? (blog-query-result-number (car results)) 1)
    (check-equal? (blog-query-result-expressions (car results)) '(42))
    (check-equal? (blog-query-result-probabilities (car results)) (hash 42 1)))

  (test-case "separate queries return ordered marginal distributions"
    (define results
      (run "random Integer X ~ UniformInt(0, 1); query X; query X + X; query X;"))
    (check-equal? (map blog-query-result-number results) '(1 2 3))
    (check-equal? (map blog-query-result-expressions results) '((X) ((+ X X)) (X)))
    (check-equal? (map blog-query-result-probabilities results)
                  (list (hash 0 1/2 1 1/2) (hash 0 1/2 2 1/2) (hash 0 1/2 1 1/2))))

  (test-case "later observations do not change earlier snapshots"
    (check-equal?
     (probabilities "random Integer X ~ UniformInt(0, 1);
                     query X; obs X = 1; query X; query X + X;")
     (list (hash 0 1/2 1 1/2) (hash 1 1) (hash 2 1))))

  (test-case "joint queries preserve correlation and expression order"
    (define results
      (run "random Integer X ~ UniformInt(0, 1); query X, X + X, X == 1;"))
    (check-equal? (blog-query-result-expressions (car results))
                  '(X (+ X X) (== X 1)))
    (check-equal? (blog-query-result-probabilities (car results))
                  (hash '(0 0 #f) 1/2 '(1 2 #t) 1/2)))

  (test-case "independent variables give the full joint distribution"
    (check-equal?
     (probabilities "random Integer X ~ UniformInt(0, 1);
                     random Integer Y ~ UniformInt(0, 1); query X, Y;")
     (list (hash '(0 0) 1/4 '(0 1) 1/4 '(1 0) 1/4 '(1 1) 1/4))))

  (test-case "joint queries and marginal queries use evidence at their own position"
    (check-equal?
     (probabilities "random Integer X ~ UniformInt(0, 1);
                     query X, X + X; obs X = 1; query X; query X, X + X;")
     (list (hash '(0 0) 1/2 '(1 2) 1/2) (hash 1 1) (hash '(1 2) 1))))

  (test-case "grounded random functions are not redrawn between queries"
    (check-equal?
     (probabilities "type Person; distinct Person Alice, Bob;
                     random Integer X(Person p) ~ UniformInt(0, 1);
                     query X(Alice); obs X(Alice) = 1;
                     query X(Alice), X(Bob); query X(Alice) == X(Alice);")
     (list (hash 0 1/2 1 1/2)
           (hash '(1 0) 1/2 '(1 1) 1/2) (hash #t 1))))

  (test-case "queries may be interleaved with later declarations"
    (check-equal?
     (probabilities "query 7; random Integer X ~ UniformInt(0, 1);
                     query X; fixed Integer Y = 10; query X, Y;")
     (list (hash 7 1) (hash 0 1/2 1 1/2) (hash '(0 10) 1/2 '(1 10) 1/2))))

  (test-case "a BLOG list value and a joint tuple remain distinct"
    (define results (probabilities "query [1, 2]; query 1, 2;"))
    (check-not-equal? (first results) (second results))
    (check-equal? (second results) (hash '(1 2) 1)))

  (test-case "joint queries include matrices without losing scalar correlations"
    (check-equal?
     (probabilities "random Integer X ~ UniformInt(0, 1);
                     random RealMatrix a = [X, X]; query a, toInt(a[1]);")
     (list (hash (list (matrix '(0 0)) 0) 1/2 (list (matrix '(1 1)) 1) 1/2))))

  (test-case "embedded runs are silent and isolate observations across calls"
    (define result #f)
    (check-equal?
     (with-output-to-string
       (lambda ()
         (set! result (probabilities "random Integer X ~ UniformInt(0, 1);
                                     obs X = 1; query X;")))) "")
    (check-equal? result (list (hash 1 1)))
    (check-equal? (probabilities "random Integer X ~ UniformInt(0, 1); query X;")
                  (list (hash 0 1/2 1 1/2))))

  (test-case "BLOG modules print sequential results and support joint syntax"
    (define-values (certain output)
      (module-results "random Integer X ~ UniformInt(0, 1); query X;
                       obs X = 1; query X; query X, X + X;"))
    (check-equal? certain '(1 (1 2)))
    (check-equal? (length (regexp-match* #rx"1/2" output)) 2))

  (test-case "BLOG modules print one joint table with correlated tuple outcomes"
    (define-values (certain output)
      (module-results "random Integer X ~ UniformInt(0, 1); query X, X + X;"))
    (check-equal? certain '())
    (check-equal? (length (regexp-match* #rx"Probability" output)) 1)
    (check-true (regexp-match? #rx"0 0" output))
    (check-true (regexp-match? #rx"1 2" output)))

  (test-case "errors propagate and the next session remains usable"
    (check-exn exn:fail:contract?
               (lambda () (run "query 7; query [1][2];")))
    (check-equal? (probabilities "query 8;") (list (hash 8 1)))))
