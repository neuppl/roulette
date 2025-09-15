#lang roulette/example/disrupt

;; Version 2
(provide (except-out (all-from-out roulette/example/disrupt) #%module-begin)
         (rename-out [#%mb #%module-begin]))

(require (for-syntax syntax/parse)
         json
         syntax/wrap-modbeg
         syntax/location
         racket/path
         relation/type)

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
      (define-values (res real cpu gc)
        (time-apply (lambda () ?r ...) (list)))

      (call-with-output-file (path-replace-suffix (file-name-from-path (quote-module-name)) ".json")
        (lambda (out)
          (write-json (hasheq
                        'result (if (jsexpr? (car res))
                                    (car res)
                                    (if (pmf? (car res))
                                      (for/hash ([(val prob) (in-pmf (car res))])
                                            (values (->symbol val) prob))
                                      "some non-jsonable result"))
                        'real_time_ms real
                        'cpu_time_ms cpu
                        'gc_time_ms gc)
                      out))
        #:exists 'replace))))



(module reader syntax/module-reader
  #:language 'benchmark_ci)