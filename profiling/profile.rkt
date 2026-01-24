#lang racket
(require racket/function)
(require json)
(require relation/type)
(require racket/serialize)
;; this file is run with an argument "file_name.rkt" that contains an interrupt program that is to be profiled.


;;A State is a (List (Pair Number Boolean)) representing that the n-indexed symbolic variable should be
;; assigned a particular boolean value. 
;; An empty list means no assignments (all variables are unknown, we start at this state)

(define (die msg)
  (eprintf "Error: ~a\n" msg)
  (exit 1))

(define (write-now data [port (current-output-port)])
	(write data port)
	(flush-output port))


(define (terminate-subprocess proc stdout stdin stderr)
	(subprocess-kill proc #t)
	(close-output-port stdin)
  (close-input-port stdout)
  (when (input-port? stderr) 
		(close-input-port stderr)))

(define (random-specialize state num-vars)
	(define current-assignments (map (lambda (asgn) (car asgn)) state))
  (define unknown-positions
    (for/list ([pos (in-range num-vars)]
               #:unless (member pos current-assignments))
      pos))
  
  (if (empty? unknown-positions)
      (die "out of states to specialize")
      (let* ([random-pos (list-ref unknown-positions (random (length unknown-positions)))]
             [asgn (= 1 (random 2))])
				(cons (cons random-pos asgn)
							state))))



;; Produces a hash from each variable to (Listof number-of-successful-samples number-of-total-assignments)
;; If provided a non-false stream (in+out ports), then provides the stream with freq-map on every update/sample collected

(define (make-heuristics stream? #:resume [resumption-data #f])
  (let ([freq-map (if resumption-data
											(make-hash (hash->list resumption-data)) ; This is to make the hash mutable 
											(make-hash))])
    (cons 
			(lambda (state+result)
				(if state+result 
					(let ([state (car state+result)]
								[timed-out? (cdr state+result)])
						(for ([idx+asgn state])
							(match-define (cons idx asgn) idx+asgn)
							(hash-update! freq-map 
														idx
														(lambda (x)
															(list (if timed-out?
																				(first x)
																				(add1 (first x))) 
																		(add1 (second x))))  
														(list 0 0))) ; list of number of samples, number of assignments
						(hash-update! freq-map "Total-runs" add1 1) ;starts with one, we assume there was an initial test 
						                                            ; without any specialization which failed to execute in time
						(unless timed-out? 
							(hash-update! freq-map "Total-samples" add1 0)
							(when stream? 
								(displayln "here")
								(write-now freq-map (car stream?)))))
					(if (empty? (hash->list freq-map))
							"No heuristics collected, file runs within time limit"
							(begin 
								(when stream? (write-now 'stop (car stream?)))
								freq-map))))
			(lambda () 
				(for/hash ([(key value) freq-map])
					(values 
						(->symbol key)
						value))))))


(define (random-specialization-transition initial-state
																				  specialization-rate 
																					#:resume [resumption-data #f])
	(let ([attempts (if resumption-data 
											(hash-ref resumption-data 'attempts)
											1)]
				[samples (if resumption-data 
											(hash-ref resumption-data 'samples)
											1)]
				[specialization-rate (if resumption-data 
																(hash-ref resumption-data 'specialization-rate)
																specialization-rate)])
		(cons
			(lambda (num-vars timed-out?)
				(if timed-out?
						(begin
							(printf "~v: Attempt ~v: failed\n\n\n" samples attempts)
							(set! attempts (+ attempts 1)))
						(begin
							(printf "~v: Found a working sample in ~v attempt(s)\n\n\n" samples attempts)
							(set! attempts 1)
							(set! samples (+ samples 1))))

				(printf "Specializing ~v variables\n" specialization-rate)
				(for/fold ([acc initial-state])
									([x (in-range specialization-rate)])
					(random-specialize acc num-vars)))
				(lambda () (hash 'attempts attempts
												 'samples samples
												 'specialization-rate specialization-rate)))))


(define (pause file-path resumption-data)
	(define resumption-path (string-append "temp-"
																				 (path->string (path-replace-extension file-path ".json"))))
	(call-with-output-file 
		#:exists 'replace
		resumption-path
		(curry write-json resumption-data #:indent #\tab))
	(displayln (string-append "Pausing program, resumption file can be found in " resumption-path)) 
  (exit 0))


(define (search initial-state
								transition-pair
								samples
								heuristics-pair
								file-path
								timeout-duration
								#:stream-results [stream? #f]
								#:resume  [resume? #f]
								#:pause-in [pause? #f]) ; pause? is the number of samples after which to save in-progress results, and restart the program.
	(match-define (cons transition transition-pause) transition-pair)
	(match-define (cons heuristics heuristics-pause) heuristics-pair)
	(define (subsample samples ch)
		(define (subsample/acc remaining-samples ch timed-out?)
			(when (and pause? (not timed-out?))
				(set! pause? (- pause? 1))
				(when (= pause? 0)
					(pause file-path (hash 'transition (transition-pause)
																 'heuristics (heuristics-pause)
																 'initial-state initial-state
																 'samples samples
																 'timeout-duration timeout-duration
																 'stream-results? (if stream? #t #f)))))
			(if (= 0 remaining-samples)
					(channel-put ch 'done)
					(let*-values  ([(proc stdout stdin stderr)
  													(subprocess #f #f (current-error-port) "/usr/local/bin/racket" file-path)])
						(write-now "profiler-run" stdin)

						(define num-vars (read stdout))
						(define new-state (transition num-vars timed-out?))

						(write-now timeout-duration stdin)
						(write-now new-state stdin)

						(define result (read stdout))
						(define new-timed-out? (equal? result 'timed-out))
						(channel-put ch (cons new-state new-timed-out?))
						(terminate-subprocess proc stdout stdin stderr)
						(define new-samples (if (not new-timed-out?) 
																		(- remaining-samples 1) 
																		remaining-samples))
						(subsample/acc new-samples ch new-timed-out?))))
		(if resume?
			(subsample/acc samples ch #f) ; resume without first pass, assume no timeout, since resumptions should be at a successful sample
			(begin ; First pass, with initial-state, to see if it runs without any subsampling
				(let*-values ([(proc stdout stdin stderr)
												(subprocess #f #f (current-error-port) "/usr/local/bin/racket" file-path)])
					(write-now "profiler-run" stdin)

					(define num-vars (read stdout))
					(displayln num-vars)

					(write-now timeout-duration stdin)
					(write-now initial-state stdin)
					(define result (read stdout))
					(define timed-out? (equal? result 'timed-out))
					(terminate-subprocess proc stdout stdin stderr)
					(if timed-out?
						(subsample/acc samples ch #t)
						#;(begin
							(displayln "ran within time limit")
							(subsample/acc samples ch #t))
						(begin 
							(displayln "No subsampling needed, program executed within time limit. No heuristics collected.")
							(channel-put ch 'done)
							#t))))))
	
	(define producer->consumer-ch (make-channel))
	(define out-ch (make-channel))
	(define producer (thread (lambda () (subsample samples producer->consumer-ch))))
	(define consumer (thread (lambda () 
										(let loop () 
											(define item (channel-get producer->consumer-ch))
											(cond [(eq? item 'done) (channel-put out-ch (heuristics #f))]
														[else (heuristics item) (loop)])))))

	(define out (channel-get out-ch))
	(displayln out)
	out)


;; Returns the file path of the json file with profiling results

(define (make-profiling-json-results file-path stream-results? #:resume [resumption-data #f])
	(define program-text
		(call-with-input-file file-path
			(lambda (in) (port->string in))))

	(define-values (proc stdout stdin stderr)
  	(subprocess #f #f (current-error-port) "/usr/local/bin/racket" file-path))
	(write-now "generate-json" stdin)

	(write-now file-path stdin)
	(write-now program-text stdin)
	(define should-stream? (if resumption-data
													 (hash-ref resumption-data 'stream-results?)
													 stream-results?))
	(write-now (if should-stream? 'stream 'single) stdin)

	(define ports (cons stdin stdout))

	(define stream? (if should-stream? 
											ports
											#f))
	(define results 
		(if resumption-data
			(let* ([samples (hash-ref resumption-data 'samples)]
						 [initial-state (hash-ref resumption-data 'initial-state)]
					   [transition-resumption-data (hash-ref resumption-data 'transition)]
				  	 [heuristics-resumption-data (hash-ref resumption-data 'heuristics)]
						 [transition-fn (random-specialization-transition initial-state 
																															#f
																															#:resume transition-resumption-data)]
						 [heuristics-fn (make-heuristics stream? #:resume heuristics-resumption-data)]
						 [timeout (hash-ref resumption-data 'timeout-duration)])
				(search initial-state
								transition-fn
								samples
								heuristics-fn
								file-path 
								timeout
								#:stream-results stream?
								#:resume #t
								#:pause-in (get-pause-arg)))
			(search (list)
							(random-specialization-transition (list) 
																								10)
							200
							(make-heuristics stream?)
							file-path 
							2
							#:stream-results stream?
							#:pause-in (get-pause-arg))))

	(write-now results stdin)
	(define json-path (read stdout))

	(terminate-subprocess proc stdout stdin stderr)
	json-path)



(define args (current-command-line-arguments))


(define (get-file-path-argument)
	(when (< (vector-length args) 1)
		(error "Expected atleast one argument with path of file to profile, got none."))
	(define file-path (vector-ref args 0))
	file-path)


; provides the json path for the file with information to resume a previously paused search
; Argument should be provided as "--resume xyz.json"
(define (get-resumption-json-path-argument)
	(let ([resume-index (index-of (vector->list args) "--resume")])
    (if (and resume-index 
             (< (add1 resume-index) (vector-length args)))
        (vector-ref args (add1 resume-index))
        #f)))

;Provides the number of samples after which you pause the search algorithm
; Argument should be provided as "--pause n"
(define (get-pause-arg)
  (let ([pause-index (index-of (vector->list args) "--pause")])
    (if (and pause-index 
             (< (add1 pause-index) (vector-length args)))
        (string->number (vector-ref args (add1 pause-index)))
        #f)))

(define (run-profiler file-path #:resume [resumption-path #f])
	(make-profiling-json-results file-path 
															 #f
															 #:resume (if resumption-path
															 							(call-with-input-file resumption-path read-json)
																						#f))
	(displayln "Profiler completed running"))
 
(run-profiler (get-file-path-argument) 
							#:resume (get-resumption-json-path-argument))
