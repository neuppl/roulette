#lang racket

(require (only-in roulette/example/disrupt pmf? in-pmf query recursive-calls size clear-cache!)
         (for-syntax syntax/parse)
         (only-in rosette concrete?)
         json
				 syntax/location)


(provide with-benchmarking-results-dir
				 benchmark
				 scale
				 max-arg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
(define (tap x)
  (println x)
  x)

(define (wrap-query e)
  (if (concrete? e)
      e
      (query e)))
      
(define (jsonify-res r)
  (cond
    [(jsexpr? r) r]
    [(pmf? r) (for/list ([(val prob) (in-pmf r)])
                (list (~s val) prob))]
    [else (~s r)]))

(define-syntax (module-name stx)
  (syntax-parse stx
    [(_) #'(file-name-from-path (quote-module-name))]))

(define benchmarking-results-dir (make-parameter "test"))


; Run before every benchmark
(define (setup-benchmark-run)
  (clear-cache!)
  (collect-garbage 'major))

(define-syntax (with-benchmarking-results-dir stx)
  (syntax-parse stx
    [(_ ?dir ?e ...) #'(begin
                          (make-directory* ?dir) 
                          (parameterize ([benchmarking-results-dir ?dir])
                              ?e ...))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; individual benchmark runs and writing results

;;There are 2 types of benchmark results: normal, and scaling.

;; `scaling` is #f for a non-scaling run, or a list of x-axis label strings
;; (one per scaled expression) for a scaling run. all parameters are lists of values for scaling runs. 
(define (write-benchmark path scaling result real cpu gc rec-calls total-size)
  (call-with-output-file (build-path (benchmarking-results-dir) path)
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

;; the metrics collected for a single benchmarked call
(struct bench-run (result real cpu gc rec-calls size))

(define (run-benchmark make-e)
  (setup-benchmark-run)
  (define e #f) ; to contain result of running expression _before_ querying 
								; (to avoid duplicate computation when calling size)
  (define-values (res real cpu gc)
    (time-apply (lambda ()
                  (set! e (make-e))
                  (tap (wrap-query e)))
                (list)))
  (values res (bench-run (map jsonify-res res) real cpu gc (recursive-calls) (size e))))


(define-syntax (format-benchmark stx)
	(syntax-parse stx
		[(_ ?type ?name ?e ...)
		#'(begin 
				(displayln "---------------------------------------------------------------")
		 		(printf "Running ~a benchmark: ~a\n\n" ?type ?name)
				?e ...
				(displayln "---------------------------------------------------------------")
				)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level benchmarking utilities

;; 1) basic timing, recursive calls and bdd size information while running an expression
(define-syntax (benchmark stx)
  (syntax-parse stx
    [(_ ?e)
     #'(format-benchmark "timing" (module-name)
				(let-values ([(res run) (run-benchmark (lambda () ?e))])
								(write-benchmark
									(path-replace-extension (module-name) ".json")
									#f
									(bench-run-result run)
									(bench-run-real run) (bench-run-cpu run) (bench-run-gc run)
									(bench-run-rec-calls run)
									(bench-run-size run))
								(apply values res)))]))

;; 2) record benchmarking information for calling `?fn` on each of `?arg ...`
;; together, to see how performance scales
(define-syntax (scale stx)
  (syntax-parse stx
    [(_ ?fn (?arg ...))
     ;; label each run with the equivalent call syntax, e.g. "(scaling-hardware 2)"
     (with-syntax ([(?label ...)
                    (map (lambda (a) (format "(~s ~s)"
                                             (syntax->datum #'?fn)
                                             (syntax->datum a)))
                         (syntax->list #'(?arg ...)))])
       #'(format-benchmark "scaling" (module-name)
           (let ([fn ?fn])
             (define runs
               (list (let-values ([(_ run) (run-benchmark (lambda () (fn ?arg)))]) run)
                     ...))
             (write-benchmark
               (path-replace-extension (module-name) ".json")
               (list ?label ...)
               (map bench-run-result runs)
               (map bench-run-real runs)
               (map bench-run-cpu runs)
               (map bench-run-gc runs)
               (map bench-run-rec-calls runs)
               (map bench-run-size runs)))))]))


;; 3) Okay I lied, there is a third benchmark type that records the maximum value of an argument to a
;; function. 
;; Find the largest argument n that computes within a fixed recursive call limit, by incrementing arg 
;; by `step` each iteration
(define-syntax (max-arg stx)
  (syntax-parse stx
    [(_ ?fn (~alt (~optional (~seq #:start ?start) #:defaults ([?start #'0]))
                  (~optional (~seq #:step ?step) #:defaults ([?step #'10]))
                  (~optional (~seq #:rec-limit ?limit) #:defaults ([?limit #'1000000])))
        ...)
     #'(format-benchmark "max-arg" (module-name)
         (let ([fn ?fn]
               [name (module-name)]
               [start ?start]
               [step ?step]
               [limit ?limit])
           (define (exceeds-limit? n)
             (define rec-calls
               (begin (setup-benchmark-run)
                      (wrap-query (fn n))
                      (recursive-calls)))
             (printf "n=~a:" n)
             (cond
               [(>= rec-calls limit)
                (printf "Ran out of recursive calls: ~a\n" rec-calls)
                #t]
               [else
                (printf "Ran in fewer than ~a recursive calls: ~a \n" limit rec-calls)
                #f]))

           (define max
             (let loop ([i start])
               (if (exceeds-limit? i)
                   (- i step)  ; _previous_ iteration is the last one within limit
                   (loop (+ i step)))))

           (printf "maximum argument value is ~a\n" max)

           (call-with-output-file (build-path (benchmarking-results-dir) (path-replace-extension name ".json"))
             (lambda (out)
               (write-json
                (hash
                 'max-arg #t
                 'arg-value max
                 'start start
                 'step step
                 'rec-limit limit)
                out))
             #:exists 'replace)))]))



