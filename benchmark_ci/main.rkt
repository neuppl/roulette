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
        
        (define-values (result logs)
          (roulette-interceptor
            (λ () (define-values (res real cpu gc)
                            (time-apply (lambda () ?r ...) (list)))
                  (hash 'res res 
                        'real real 
                        'cpu cpu 
                        'gc gc))))

        (define res (hash-ref result 'res))
        (define real (hash-ref result 'real))
        (define cpu (hash-ref result 'cpu))
        (define gc (hash-ref result 'gc))
        (define log-list (filter (lambda (x) (regexp-match? #rx"^roulette:" x))
                         (hash-ref logs 'info)))
        (call-with-output-file (path-replace-suffix (file-name-from-path (quote-module-name)) ".json")
          (lambda (out)
            (write-json 
              (hash
                'result (if (jsexpr? (car res))
                            (car res)
                            (if (pmf? (car res))
                              (for/list ([(val prob) (in-pmf (car res))])
                                    (list (~s val) prob))
                              (~s (car res))))
                'real_time_ms real
                'cpu_time_ms cpu
                'gc_time_ms gc
                'recursive-calls (string->number
                                    (second 
                                      (regexp-match
                                        #rx"roulette: ([0-9]+) recursive calls" 
                                        (last log-list)))) 
                'total-size (foldr  +
                                    0
                                    (map 
                                      (lambda (s) 
                                        (define match (regexp-match 
                                          #rx"roulette: ([0-9]+) total size" s))
                                        (if match
                                            (string->number (second match))
                                            #f))
                                      (filter  
                                        (lambda (x) 
                                          (regexp-match? #rx"roulette: ([0-9]+) total size" x))
                                          log-list))))
              out))
          #:exists 'replace)
        (display (car res)))))



(module reader syntax/module-reader
  #:language 'benchmark_ci)