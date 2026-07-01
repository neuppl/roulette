#lang roulette/example/disrupt

;; Version 2
(provide (except-out (all-from-out roulette/example/disrupt) #%module-begin)
         (rename-out [#%mb #%module-begin]))

(require (for-syntax syntax/parse)
         json
         syntax/wrap-modbeg
         syntax/location
         racket/path
         roulette/private/log
         make-log-interceptor)

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
    #'(begin
        (define roulette-interceptor
          (make-log-interceptor roulette-logger))
        (define (tap x)
          (displayln x)
          x)
        (define-values (res real cpu gc) (time-apply (lambda () (values (tap (query ?r)) ...)) (list)))

        (define jsonified-res (map (lambda (r) 
                                      (cond 
                                        [(jsexpr? r) r]
                                        [(pmf? r) (for/list ([(val prob) (in-pmf r)])
                                                    (list (~s val) prob))]
                                        [else (~s r)])) 
                                    res))
        (call-with-output-file (path-replace-suffix (file-name-from-path (quote-module-name)) ".json")
          (lambda (out)
            (write-json 
              (hash
                'result jsonified-res
                'real_time_ms real
                'cpu_time_ms cpu
                'gc_time_ms gc
                'recursive-calls (recursive-calls)
                'total-size (+ (size ?r) ...))
              out))
          #:exists 'replace)
          (apply values res))))



(module reader syntax/module-reader
  #:language 'benchmark_ci)
