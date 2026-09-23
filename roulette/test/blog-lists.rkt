#lang racket/base

(require rackunit "blog-test-support.rkt")

(define (parse source) (parse-blog source 'lists.blog))
(define run (make-blog-runner 'lists.blog))

(module+ test
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

  (test-case "list elements can be arbitrary expressions"
    (check-equal?
     (parse "query [f(1, 2), if p then [3] else [], x + 1];")
     '((query (blog-list (call f 1 2)
                        (if p (blog-list 3) (blog-list)) (+ x 1))))))

  (test-case "semicolons within lists do not end statements"
    (check-equal?
     (parse "fixed RealMatrix a = [1, 2; 3, 4]; query a;")
     '((fixed RealMatrix a ()
              (blog-list (blog-list 1 2) (blog-list 3 4)))
       (query a))))

  (test-case "postfix indexing accepts calls, literals and arbitrary grouped expressions"
    (check-equal?
     (parse "query transpose(a)[0]; query (a + b)[i][j]; query [10, 20][1];")
     '((query (index (call transpose a) 0))
       (query (index (index (+ a b) i) j))
       (query (index (blog-list 10 20) 1))))
    (check-equal? (parse "query (if p then a else b)[i];")
                  '((query (index (if p a b) i)))))

  (test-case "indexing binds tightly and associates to the left"
    (check-equal? (parse "query -a[i + 1][j] * b[k] + c;")
                  '((query (+ (* (- (index (index a (+ i 1)) j))
                                 (index b k)) c))))
    (check-equal? (parse "query a[b[i]];")
                  '((query (index a (index b i))))))

  (test-case "indexed declarations retain their existing representation"
    (check-equal? (parse "type Person; distinct Person P[2]; query P[0];")
                  '((type Person) (distinct Person (indexed P 2))
                    (query (index P 0)))))

  (test-case "lists compose with maps and function arguments"
    (check-equal?
     (parse "query f([1; 2], {[0, 1] -> [3, 4]});")
     '((query (call f (blog-list (blog-list 1) (blog-list 2))
                    (map (entry (blog-list 0 1) (blog-list 3 4))))))))

  (test-case "malformed literals and indexes produce located read errors"
    (for ([source '("query [1,];" "query [,1];" "query [1;];"
                    "query [;1];" "query [1;;2];" "query [1,2;"
                    "query a[];" "query a[1,2];" "query a[1;")])
      (check-exn
       (lambda (exn)
         (and (exn:fail:read? exn)
              (equal? (srcloc-source (car (exn:fail:read-srclocs exn))) 'lists.blog)))
       (lambda () (parse source)) source)))

  (test-case "parsed lists evaluate and preserve symbolic correlations"
    (check-equal? (run "query [1, 2; 3, 4] == [[1, 2], [3, 4]];")
                  (hash #t 1))
    (check-equal? (run "random Integer x ~ UniformInt(0, 1);
                       query [x, x] == [0, 0];")
                  (hash #t 1/2 #f 1/2))))
