#lang racket
(require (only-in roulette/example/disrupt pmf? in-pmf query recursive-calls size clear-cache!)
         (for-syntax syntax/parse)
         (only-in rosette concrete?)
         json
				 racket/date
				 "small/alarm.rkt"
				 "small/evidence1.rkt"
				 "small/evidence2.rkt"
				 "small/grass.rkt"
				 "small/murder-mystery.rkt"
				 "small/n-grid.rkt"
				 "small/noisy-or.rkt"
				 "small/two-coins.rkt"
				 "scaling/hardware.rkt"
				 "scaling/network.rkt"
				 "bayesian-networks/alarm.bif.rkt"
				 "bayesian-networks/cancer.bif.rkt"
				 "bayesian-networks/hailfinder.bif.rkt"
				 "bayesian-networks/hepar2.bif.rkt"
				 "bayesian-networks/insurance.bif.rkt"
				 "bayesian-networks/munin.bif.rkt"
				 "bayesian-networks/pigs.bif.rkt"
				 "bayesian-networks/survey.bif.rkt"
				 "bayesian-networks/water.bif.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
(define (tap x)
  (displayln x)
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


(define current-benchmarking-dir #f)
(define benchmarking-dirs (list))


(define-syntax (with-benchmarking-results-dir stx)
  (syntax-parse stx
    [(_ ?dir ?e ...) #'(begin (make-directory ?dir)
															(set! benchmarking-dirs (cons ?dir benchmarking-dirs))
															(set! current-benchmarking-dir ?dir)
															?e ...
															(set! current-benchmarking-dir #f))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; individual benchmark runs and writing results

;;There are 2 types of benchmark results: normal, and scaling.

;; `scaling` is #f for a non-scaling run, or a list of x-axis label strings
;; (one per scaled expression) for a scaling run. all parameters are lists of values for scaling runs. 
(define (write-benchmark path scaling result real cpu gc rec-calls total-size)
  (call-with-output-file (build-path current-benchmarking-dir path)
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
  (clear-cache!)
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
    [(_ ?e ?name)
     #'(format-benchmark "timing" ?name
				(let-values ([(res run) (run-benchmark (lambda () ?e))])
								(write-benchmark
									(path-replace-extension ?name ".json")
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
    [(_ ?fn (?arg ...) ?name)
     ;; label each run with the equivalent call syntax, e.g. "(scaling-hardware 2)"
     (with-syntax ([(?label ...)
                    (map (lambda (a) (format "(~s ~s)"
                                             (syntax->datum #'?fn)
                                             (syntax->datum a)))
                         (syntax->list #'(?arg ...)))])
       #'(format-benchmark "scaling" ?name
           (let ([fn ?fn])
             (define runs
               (list (let-values ([(_ run) (run-benchmark (lambda () (fn ?arg)))]) run)
                     ...))
             (write-benchmark
               (path-replace-extension ?name ".json")
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
(define (max-arg fn name #:start [start 0] #:step [step 10] #:rec-limit [limit 1000000])
  (format-benchmark "max-arg" name
		(define (exceeds-limit? n)
			(define rec-calls
				(begin (clear-cache!)
							(fn n)
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

		(call-with-output-file (build-path current-benchmarking-dir (path-replace-extension name ".json"))
			(lambda (out)
				(write-json
				(hash
					'max-arg #t
					'arg-value max
					'start start
					'step step
					'rec-limit limit)
				out))
			#:exists 'replace)))


(define (run-small-benchmarks)
	(displayln "\033[1mSmall benchmarks\033[0m")
	(with-benchmarking-results-dir "small-results"
		(benchmark (small-alarm) "alarm.rkt")
		(benchmark (small-evidence1) "evidence1.rkt")
		(benchmark (small-evidence2) "evidence2.rkt")
		(benchmark (small-grass) "grass.rkt")
		(benchmark (small-murder-mystery) "murder-mystery.rkt")
		(benchmark (small-n-grid 10) "n-grid.rkt")
		(benchmark (small-noisy-or) "noisy-or.rkt")
		(benchmark (small-two-coins) "two-coins.rkt")))

(define (run-scaling-benchmarks)
	(displayln "\033[1mScaling benchmarks\033[0m")
	(with-benchmarking-results-dir "scaling-results"
		(scale scaling-hardware (2 3 4 5 6 7) "hardware.rkt")
		(scale scaling-network (1 2 3 4 5 6 7 8) "network.rkt")))

(define (run-max-arg-benchmarks)
	(displayln "\033[1mMax-arg benchmarks\033[0m")
	(with-benchmarking-results-dir "max-arg-results"
		(max-arg small-n-grid "max_n-grid.rkt" #:start 1 #:step 5)
		(max-arg scaling-hardware "max_hardware.rkt" #:start 1 #:step 1)
		(max-arg scaling-network "max_network.rkt" #:start 1 #:step 2)))

(define (run-bayesian-networks-benchmarks)
	(displayln "\033[1mBayesian network benchmarks\033[0m")
	(with-benchmarking-results-dir "bayesian-networks-results"
		(benchmark (bn-alarm) "alarm.bif")
		(benchmark (bn-cancer) "cancer.bif")
		(benchmark (bn-hailfinder) "hailfinder.bif")
		(benchmark (bn-hepar2) "hepar2.bif")
		(benchmark (bn-insurance) "insurance.bif")
		(benchmark (bn-munin) "munin.bif")
		(benchmark (bn-pigs) "pigs.bif")
		(benchmark (bn-survey) "survey.bif")
		(benchmark (bn-water) "water.bif")))

(define (run-all-benchmarks)
	(run-small-benchmarks)
	(run-scaling-benchmarks)
	(run-max-arg-benchmarks)
	(run-bayesian-networks-benchmarks))


(define (save-benchmarking-results #:commit-msg [msg #f] #:commit-hash [hash #f])
	(date-display-format 'iso-8601) 
	(define results-dir (build-path "data" 
																	(string-append "run_" (date->string (current-date) 
																																			(current-milliseconds)))))
	(make-directory* results-dir)
	(for ([dir benchmarking-dirs])
		(copy-directory/files dir (build-path results-dir (string-trim dir "-results")))
		(delete-directory/files dir))
	;; record the commit hash and message for site.scribl. site.scribl currently
	;; reads only COMMIT_HASH.txt; COMMIT_MSG.txt is written for potential future use.
	(when hash
		(call-with-output-file (build-path results-dir "COMMIT_HASH.txt")
			(lambda (out) (display hash out))
			#:exists 'replace))
	(when msg
		(call-with-output-file (build-path results-dir "COMMIT_MSG.txt")
			(lambda (out) (display msg out))
			#:exists 'replace))
	(set! benchmarking-dirs (list)))

(module+ main
	(run-all-benchmarks)
	(define args (current-command-line-arguments))
	(save-benchmarking-results #:commit-hash (and (>= (vector-length args) 1) (vector-ref args 0))
														 #:commit-msg (and (>= (vector-length args) 2) (vector-ref args 1))))