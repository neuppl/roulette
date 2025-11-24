#lang racket
(require racket/function)
;; this file is run with an argument "file_name.rkt" that contains an interrupt program that is to be profiled.


;;A State is a (List (Pair Number Boolean)) representing that the n-indexed symbolic variable should be
;; assigned a particular boolean value. 
;; An empty list means no assignments (all variables are unknown, we start at this state)

(define (die msg)
  (eprintf "Error: ~a\n" msg)
  (exit 1))

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


;; Produces a hash of number of variable assignments for each idx accumulated from the provided state+results
;; If provided a non-false stream (place channel), then provides the stream with freq-map on every update

; Hash from each possible variable assignment to % of runs in which that assignment was included in a 
; successful sample 
(define (make-heuristics stream?)
  (let ([freq-map (make-hash)])
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
					(hash-update! freq-map "Total-runs" add1 0)
					(unless timed-out? 
						(hash-update! freq-map "Total-samples" add1 0)
						(when stream? (place-channel-put stream? freq-map))))
				(if (empty? (hash->list freq-map))
						"No heuristics collected, file runs within time limit"
						(begin 
							(when stream? (place-channel-put stream? 'stop))
							freq-map))))))


;; Given the file path, runs subsampling search algorithm and provides heuristics. file-path is 
;; expected to be a file in #lang roulette/example/interrupt
(define (search file-path 
								samples 
								specialization-rate
								initial-state
								timeout-duration
								#:stream-results [stream? #f])
	(define random-specialization-transition 
		(let ([attempts 1]
					[samples 1]
					[transition-rate specialization-rate])
			(lambda (num-vars timed-out?)
				(if timed-out?
						(begin
							(printf "~v: Attempt ~v: failed\n\n\n" samples attempts)
							(set! attempts (+ attempts 1))
							(set! transition-rate (+ 0 transition-rate)))
						(begin
							(printf "~v: Found a working sample in ~v attempt(s)\n\n\n" samples attempts)
							(set! attempts 1)
							(set! transition-rate specialization-rate)
							(set! samples (+ samples 1))))
				(printf "Specializing ~v variables\n" transition-rate)
				(for/fold ([acc initial-state])
									([x (in-range transition-rate)])
					(random-specialize acc num-vars)))))

	(define (subsample samples ch)
		(define (subsample/acc remaining-samples ch timed-out?)
			(if (= 0 remaining-samples)
			(channel-put ch 'done)
			(let*  ([pch       (dynamic-place file-path 'place-main)]
						  [num-vars (place-channel-get pch)]
					    [new-state (random-specialization-transition num-vars timed-out?)])
				(place-channel-put pch timeout-duration)
				(place-channel-put pch new-state)
				(define result (place-channel-get pch))
				(define timed-out? (equal? result "timed-out"))
				(channel-put ch (cons new-state timed-out?))
				(place-kill pch)
				(define new-samples (if (not timed-out?) 
																(- remaining-samples 1) 
																remaining-samples))
				(subsample/acc new-samples ch timed-out?))))
		

		; First pass, with no assignments, to see if it runs without any subsampling
		(define pch (dynamic-place file-path 'place-main))
		(define num-vars (place-channel-get pch))
		(displayln num-vars)
		(place-channel-put pch timeout-duration)
		(place-channel-put pch initial-state)
		(define result (place-channel-get pch))
		(define timed-out? (equal? result "timed-out"))
		(place-kill pch)
		(if timed-out?
				(subsample/acc samples ch #t)
				(begin 
					(displayln "No subsampling needed, program executed within time limit. No heuristics collected.")
					(channel-put ch 'done)
					#t)))
					
	(define producer->consumer-ch (make-channel))
	(define out-ch (make-channel))
	(define heuristics (make-heuristics stream?))
	(define producer (thread (lambda () (subsample samples producer->consumer-ch))))
	(define consumer (thread (lambda () 
										(let loop () 
											(define item (channel-get producer->consumer-ch))
											(cond [(eq? item 'done) (channel-put out-ch (heuristics #f))]
														[else (heuristics item) (loop)])))))

	
	(channel-get out-ch))


;; Returns the file path of the json file with profiling results
(define (make-profiling-json-results file-path stream-results?)
	(define program-text
		(call-with-input-file file-path
			(lambda (in) (port->string in))))

	
	(define pch (dynamic-place file-path 'generate-json))
	(place-channel-put pch file-path)
	(place-channel-put pch program-text)
	(place-channel-put pch (if stream-results? 'stream 'single))

	(define stream? (if stream-results? 
											pch
											#f))

	(define results (search 
										file-path 
										5000 
										100 
										(list) 
										5
										#:stream-results stream?))


	(place-channel-put pch results)	
	(define json-path (place-channel-get pch))
	(place-kill pch)
	json-path)




(define (get-file-path-argument)
	(define args (current-command-line-arguments))
	(when (< (vector-length args) 1)
		(error "Expected atleast one argument with path of file to profile, got none."))
		(define file-path (vector-ref args 0))
		file-path)



(define (run-profiler file-path)
	(make-profiling-json-results file-path #t)
	(displayln "My work here is done"))




(run-profiler (get-file-path-argument))
