#lang errortrace racket

;; this file is run with an argument "file_name.rkt" that contains an interrupt program that is to be profiled.


;;A State is a (List (Pair Number Boolean)) representing that the n-indexed symbolic variable should be
;; assigned a particular boolean value. 
;; An empty list means no assignments (all variables are unknown, we start at this state)

(define file-path (vector-ref (current-command-line-arguments) 0))

(define program-text
	(call-with-input-file file-path
		(lambda (in) (port->string in))))

(define (die msg)
  (eprintf "Error: ~a\n" msg)
  (exit 1))


;; A State is a (List (Pair Number Boolean)) representing that the n-indexed symbolic variable should be
;; assigned a particular boolean value. 
;; An empty list means no assignments (all variables are unknown, we start at this state)


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
(define (make-heuristics) 
  (let ([freq-map (make-hash)])
    (lambda (state+result)
      (if state+result 
          (let ([state (car state+result)]
                [timed-out? (cdr state+result)])
            (unless timed-out?
							(for/list ([idx+asgn state])
								(match-define (cons idx asgn) idx+asgn)
								(hash-update! freq-map idx add1 0))
							(hash-update! freq-map 'Total-runs add1 0)))
          freq-map))))


;; Given the file path, runs subsampling search algorithm and provides heuristics. file-path is 
;; expected to be a file in #lang roulette/example/interrupt
(define (search file-path samples specialization-rate initial-state timeout-duration)
	(define random-specialization-transition 
		(let ([current-state initial-state]
					[attempts 1]
					[samples 1])
			(lambda (num-vars timed-out?)
				(if timed-out?
						(begin
							(printf "~v: Attempt ~v: failed\n\n\n" samples attempts)
							(set! attempts (+ attempts 1))
							
							(let ([new-state 
											(for/fold ([acc current-state])
																([x (in-range specialization-rate)])
												(random-specialize acc num-vars))])
								(set! current-state new-state)
								new-state))
						(begin
							(printf "~v: Found a working sample in ~v attempt(s)\n\n\n" samples attempts)
							(set! attempts 1)
							(set! samples (+ samples 1))
							(set! current-state initial-state)
							initial-state)))))

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
		(place-channel-put pch timeout-duration)
		(place-channel-put pch initial-state)
		(define result (place-channel-get pch))
		(define timed-out? (equal? result "timed-out"))
		(place-kill pch)
		(if timed-out?
				(subsample/acc (- samples 1) ch #t)
				(begin 
					(displayln "No subsampling needed, program executed within time limit. No heuristics collected.")
					(channel-put ch 'done)
					#t)))
					
	(define producer->consumer-ch (make-channel))
	(define out-ch (make-channel))
	(define heuristics (make-heuristics))
	(define producer (thread (lambda () (subsample samples producer->consumer-ch))))
	(define consumer (thread (lambda () 
										(let loop () 
											(define item (channel-get producer->consumer-ch))
											(cond [(eq? item 'done) (channel-put out-ch (heuristics #f))]
														[else (heuristics item) (loop)])))))

	
	(channel-get out-ch))





(define pch (dynamic-place file-path 'generate-json))
(place-channel-put pch program-text)
(displayln (place-channel-get pch))