#lang racket
;; This file is run with an argument "file_name.rkt" containing an interrupt program to be profiled.

;; A State is a (List (Pair Number Boolean)) representing that the n-indexed symbolic variable
;; should be assigned a particular boolean value.
;; An empty list means no assignments (all variables unknown — the initial state).

(define (die msg)
  (eprintf "Error: ~a\n" msg)
  (exit 1))

(define (write-now data [port (current-output-port)])
  (write data port)
  (newline port)
  (flush-output port))

(define (terminate-subprocess proc stdout stdin stderr)
  (subprocess-kill proc #t)
  (close-output-port stdin)
  (close-input-port stdout)
  (when (input-port? stderr)
    (close-input-port stderr)))

(define (random-specialize state num-vars)
  (define current-assignments (map car state))
  (define unknown-positions
    (for/list ([pos (in-range num-vars)]
               #:unless (member pos current-assignments))
      pos))
  (if (empty? unknown-positions)
      (die "out of states to specialize")
      (let ([random-pos (list-ref unknown-positions (random (length unknown-positions)))]
            [asgn (= 1 (random 2))])
        (cons (cons random-pos asgn) state))))


;; Accumulates a hash from variable index -> (list sum-of-eliminations total-assignments).
;; Called with (cons state result) per run, or #f to finalize and return the avg-map.
(define (make-heuristics)
  (let ([avg-map (make-hash)])
    (cons
      (lambda (state+result)
        (if state+result
            (let ([state (car state+result)]
                  [result (cdr state+result)])
              (for ([idx+asgn state])
                (match-define (cons idx _) idx+asgn)
                (hash-update! avg-map idx
                              (lambda (x) (list (+ (first x) result) (add1 (second x))))
                              (list 0 0)))
              (hash-update! avg-map "Total-runs" add1 1))
            (if (hash-empty? avg-map)
                "No heuristics collected, file runs within time limit"
                avg-map)))
      (lambda () #f))))


;; Returns a transition function that randomly specializes `specialization-rate` variables per run.
(define (random-specialization-transition specialization-rate)
  (let ([runs 0])
    (lambda (num-vars)
      (set! runs (add1 runs))
      (printf "Run ~v\n" runs)
      (printf "Specializing ~v variables\n" specialization-rate)
      (for/fold ([acc '()])
                ([_ (in-range specialization-rate)])
        (random-specialize acc num-vars)))))


;; Runs `num-runs` profiler evaluations against `file-path` using a persistent subprocess.
;; Returns the accumulated heuristics avg-map.
(define (search transition num-runs heuristics-pair file-path)
  (match-define (cons heuristics _) heuristics-pair)
  (define-values (proc stdout stdin stderr)
    (subprocess #f #f (current-error-port) "/usr/local/bin/racket" file-path))
  (write-now "profiler-run" stdin)
  (define num-vars (read stdout))

  (define (do-runs remaining ch)
    (if (zero? remaining)
        (begin
          (write-now 'done stdin)
          (terminate-subprocess proc stdout stdin stderr)
          (channel-put ch 'done))
        (let* ([new-state (transition num-vars)]
               [_ (write-now new-state stdin)]
               [result (read stdout)])
          (displayln result)
          (channel-put ch (cons new-state result))
          (do-runs (sub1 remaining) ch))))

  (define ch (make-channel))
  (define out-ch (make-channel))
  (thread (lambda () (do-runs num-runs ch)))
  (thread (lambda ()
            (let loop ()
              (define item (channel-get ch))
              (if (eq? item 'done)
                  (channel-put out-ch (heuristics #f))
                  (begin (heuristics item) (loop))))))

  (channel-get out-ch))


;; Spawns the target file in generate-json mode, runs the search, and writes results to JSON/HTML.
(define (make-profiling-json-results file-path)
  (define program-text (call-with-input-file file-path port->string))

  (define results
    (search (random-specialization-transition (get-specialize-arg))
            1000
            (make-heuristics)
            file-path))

  (define-values (proc _stdout stdin _stderr)
    (subprocess #f #f (current-error-port) "/usr/local/bin/racket" file-path))
  (write-now "generate-json" stdin)
  (write-now file-path stdin)
  (write-now program-text stdin)
  (write-now results stdin)
  (close-output-port stdin)

  (subprocess-wait proc))


(define args (current-command-line-arguments))

(define (get-file-path-argument)
  (when (< (vector-length args) 1)
    (error "Expected at least one argument with path of file to profile, got none."))
  (vector-ref args 0))

;; --n N  sets the number of variables specialized per run (default 50)
(define (get-specialize-arg)
  (let ([idx (index-of (vector->list args) "--n")])
    (if (and idx (< (add1 idx) (vector-length args)))
        (string->number (vector-ref args (add1 idx)))
        50)))

(make-profiling-json-results (get-file-path-argument))
