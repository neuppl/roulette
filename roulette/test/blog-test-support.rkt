#lang racket/base

(require rackunit
         (only-in "../example/blog.rkt" blog-parse)
         (submod "../example/blog.rkt" test-support)
         (only-in (submod "../example/blog.rkt" reader)
                  [read-syntax blog-read-syntax])
         (only-in "../example/disrupt/core.rkt" [query inference-query]))

(provide parse-blog
         make-blog-runner
         matrix
         check-run
         check-invalid
         module-results)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared parsing, inference and expected-value helpers

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

;; Expected matrix values use the same checked constructor as declarations.
;; Pass rows as ordinary Racket lists; explicit empty shapes use the struct
;; constructor in tests where a row list cannot express those dimensions.
(define (matrix . rows)
  (make-blog-matrix
   (apply blog-list (map (lambda (row) (apply blog-list row)) rows))))

(define run (make-blog-runner))

(define (check-run source expected)
  (check-equal? (run source) (hash expected 1)))

(define (check-invalid . sources)
  (for ([source (in-list sources)])
    (check-exn exn:fail:contract? (lambda () (run source)) source)))

;; Exercise reader-produced modules and their automatic query printer.
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
    ;; An ordinary Disrupt query scopes the module's choices and observations.
    ;; Its top-level expressions still use Disrupt's automatic query printer.
    (void
     (inference-query
      (eval `(module ,name ,(caddr module-datum) ,@(cdddr module-datum)))
      (dynamic-require `(quote ,name) #f))))
  (values (reverse results) (get-output-string output)))
