#lang roulette/example/disrupt

(provide (except-out (all-from-out roulette/example/disrupt) #%module-begin)
         (rename-out [#%mb #%module-begin])
         scale)

(require (for-syntax syntax/parse)
         json
         syntax/wrap-modbeg
         syntax/location
         racket/path
         roulette/private/log
         make-log-interceptor)

;; runtime helpers shared by the `scale` and `make-timing-expr` expansions.
;; not provided: the macros below reach them via hygiene, so the user's
;; namespace stays clean.

(define (tap x)
  (displayln x)
  x)

(define (jsonify-res r)
  (cond
    [(jsexpr? r) r]
    [(pmf? r) (for/list ([(val prob) (in-pmf r)])
                (list (~s val) prob))]
    [else (~s r)]))

;; path is computed in each macro's template so (quote-module-name) resolves
;; in the *user's* module, naming the output file after their source.
;; `scaling` is #f for a non-scaling run, or a list of x-axis label strings
;; (one per scaled expression) for a scaling run.
(define (write-benchmark path scaling result real cpu gc rec-calls total-size)
  (call-with-output-file path
    (lambda (out)
      (write-json
        (hash
          'scaling scaling
          'result result
          'real_time_ms real
          'cpu_time_ms cpu
          'gc_time_ms gc
          'recursive-calls rec-calls
          'total-size total-size)
        out))
    #:exists 'replace))

;;record benchmarking information for multiple expressions together, to see how performance scales
(define-syntax (scale stx)
  (syntax-parse stx
    [(_ ?e ...)
     ;; use each expression's source syntax as its x-axis label, e.g. "(main 2)"
     (with-syntax ([(?label ...)
                    (map (lambda (e) (format "~s" (syntax->datum e)))
                         (syntax->list #'(?e ...)))])
     #'(let ()
         ;; one (result real cpu gc recursive-calls size) entry per expression
         (define entries
           (list
            (let-values ([(res real cpu gc)
                          (time-apply (lambda () (tap (query ?e))) (list))])
              (list (map jsonify-res res) real cpu gc (recursive-calls) (size ?e)))
            ...))
         (write-benchmark
           (path-replace-suffix (file-name-from-path (quote-module-name)) ".json")
           (list ?label ...)
           (map (lambda (e) (list-ref e 0)) entries)
           (map (lambda (e) (list-ref e 1)) entries)
           (map (lambda (e) (list-ref e 2)) entries)
           (map (lambda (e) (list-ref e 3)) entries)
           (map (lambda (e) (list-ref e 4)) entries)
           (map (lambda (e) (list-ref e 5)) entries))))]))

(begin-for-syntax
  (define top-level-exprs '()))

(define-syntax #%mb
  (make-wrapping-module-begin
   #'register-top-level
   #'#%mb-make-timing))

(define-syntax register-top-level
  (syntax-parser
    [(_ ?e)
     (set! top-level-exprs (cons #'?e top-level-exprs))
     #'(void)]))

(define-syntax #%mb-make-timing
  (syntax-parser
    [(_ ?e ...)
     #'(#%plain-module-begin
        ?e ...
        (make-timing-expr))]))

(define-syntax (make-timing-expr _)
  (with-syntax ([(?r ...) (reverse top-level-exprs)])
    (if (empty? top-level-exprs)
      #'(void)
      #'(begin
          (define-values (res real cpu gc) (time-apply (lambda () (values (tap (query ?r)) ...)) (list)))
          (write-benchmark
            (path-replace-suffix (file-name-from-path (quote-module-name)) ".json")
            #f
            (map jsonify-res res)
            real cpu gc
            (recursive-calls)
            (+ (size ?r) ...))
          (apply values res)))))



(module reader syntax/module-reader
  #:language 'benchmark_ci)
