#lang racket
(require racket/date 
				 (only-in "benchmarking.rkt" with-benchmarking-results-dir))


(define result-extension "-results")
(define (make-results-dir bench-dir) (string-append bench-dir result-extension))


(define benchmarking-dirs (list "small" "scaling" "max-arg" "bayesian-networks" "sandia/fast/"))

(define (run-benchmarks dir)
	(printf "\033[1mRunning benchmarks in ~a\n\033[0m" dir)

	(with-benchmarking-results-dir (make-results-dir dir)
		(for ([path (in-directory dir)]
					#:when (path-has-extension? (path->string path) ".rkt"))
			((dynamic-require path 'main)))))

(define (save-benchmarking-results #:commit-msg [msg #f] #:commit-hash [hash #f])
	(date-display-format 'iso-8601) 
	(define saved-results-dir (build-path "data" 
																	(string-append "run_" (date->string (current-date) 
																																			(current-milliseconds)))))
	(make-directory* saved-results-dir)
	(for ([dir (map make-results-dir benchmarking-dirs)])
		(define dest (build-path saved-results-dir (string-trim dir "-results")))
		(make-parent-directory* dest)
		(copy-directory/files dir dest)
		(delete-directory/files dir))
	(when hash
		(call-with-output-file (build-path saved-results-dir "COMMIT_HASH.txt")
			(lambda (out) (display hash out))
			#:exists 'replace))
	(when msg
		(call-with-output-file (build-path saved-results-dir "COMMIT_MSG.txt")
			(lambda (out) (display msg out))
			#:exists 'replace))
	(set! benchmarking-dirs (list)))


(module+ main
	(for ([dir benchmarking-dirs])
			 (run-benchmarks dir))
	(define args (current-command-line-arguments))
	(save-benchmarking-results #:commit-hash (and (>= (vector-length args) 1) (vector-ref args 0))
															#:commit-msg (and (>= (vector-length args) 2) (vector-ref args 1))))