#lang roulette/example/disrupt

(require rackunit
         (submod "../example/blog.rkt" test-support)
         (only-in roulette/example/disrupt [query inference-query]))

(module+ test
  (test-case "numeric lists normalize to row-major matrices"
    (define row (make-blog-matrix (blog-list 1 2 3)))
    (check-equal? (blog-matrix-value-rows row) 1)
    (check-equal? (blog-matrix-value-columns row) 3)
    (check-equal? (blog-matrix-value-elements row) '(1 2 3))
    (define matrix
      (make-blog-matrix (blog-list (blog-list 1 2) (blog-list 3 4))))
    (check-equal? (blog-matrix-value-rows matrix) 2)
    (check-equal? (blog-matrix-value-columns matrix) 2)
    (check-equal? (blog-matrix-value-elements matrix) '(1 2 3 4))
    (check-equal? (make-blog-matrix (blog-list (blog-list 1 2 3))) row)
    (check-eq? (make-blog-matrix matrix) matrix)
    (check-false (equal? (blog-list 1 2 3) row))
    (check-equal? (blog-matrix-value-elements
                   (make-blog-matrix (blog-list 1/3 2.5)))
                  '(1/3 2.5)))

  (test-case "empty numeric lists retain their specified shape"
    (check-equal? (make-blog-matrix (blog-list))
                  (blog-matrix-value 0 0 '()))
    (check-equal? (make-blog-matrix (blog-list (blog-list) (blog-list)))
                  (blog-matrix-value 2 0 '())))

  (test-case "invalid shapes and nonnumeric contents are rejected"
    (for ([source (in-list
                   (list (blog-list (blog-list 1) (blog-list 2 3))
                         (blog-list 1 (blog-list 2))
                         (blog-list (blog-list (blog-list 1)))
                         (blog-list #t) (blog-list "one")
                         (blog-list blog-null) (blog-list 1+2i)
                         '(1 2) 1))])
      (check-exn exn:fail:contract? (lambda () (make-blog-matrix source)))))

  (test-case "matrix inference preserves correlated cells and value equality"
    (define zero (make-blog-matrix (blog-list 0 0)))
    (define one (make-blog-matrix (blog-list 1 1)))
    (check-equal?
     (pmf-hash
      (inference-query
       (define x (if (flip 1/2) 0 1))
       (make-blog-matrix (blog-list x x))))
     (hash zero 1/2 one 1/2))
    (check-equal?
     (pmf-hash
      (inference-query
       (define x (if (flip 1/2) 0 1))
       (equal? (make-blog-matrix (blog-list x x))
               (if (= x 0) zero one))))
     (hash #t 1)))

  (test-case "finite alternatives containing nonnumeric cells are rejected"
    (check-exn exn:fail:contract?
               (lambda ()
                 (make-blog-matrix (blog-list (if (flip 1/2) 1 #f)))))))
