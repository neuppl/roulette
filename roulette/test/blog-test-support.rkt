#lang racket/base

(require (only-in "../example/blog.rkt" blog-parse blog-run blog-query-result-probabilities)
         (submod "../example/blog.rkt" test-support))

(provide parse-blog make-blog-runner make-blog-multi-runner matrix)

(define (parse-blog source [source-name 'example.blog])
  (blog-parse source-name (open-input-string source)))

;; Attach the same module instance used to build expected matrix/list values.
;; Otherwise transparent structs from separate namespaces still have different
;; types and two identical-looking results can compare unequal.
(define-namespace-anchor anchor)

(define (make-blog-runner [source-name 'example.blog])
  (define ns (make-base-namespace))
  (namespace-attach-module (namespace-anchor->namespace anchor)
                           'roulette/example/blog ns)
  (parameterize ([current-namespace ns])
    (namespace-require '(prefix inference: roulette/example/disrupt))
    (namespace-require 'roulette/example/blog))
  (lambda (source)
    (eval `(inference:pmf-hash
            (inference:query ,@(parse-blog source source-name))) ns)))

;; Multi-query programs need a snapshot at every statement, rather than the
;; last-expression convention retained by the single-query helper above.
(define (make-blog-multi-runner [source-name 'example.blog])
  (lambda (source)
    (map blog-query-result-probabilities
         (blog-run source-name (open-input-string source)))))

;; Expected matrix values use the same checked constructor as declarations.
;; Pass rows as ordinary Racket lists; explicit empty shapes use the struct
;; constructor in tests where a row list cannot express those dimensions.
(define (matrix . rows)
  (make-blog-matrix
   (apply blog-list (map (lambda (row) (apply blog-list row)) rows))))
