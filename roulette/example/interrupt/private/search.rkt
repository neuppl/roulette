;; File contains utilities for search algorithm to find the most expensive symbolic variables when 
;; querying an expression

#lang roulette

(provide (all-defined-out))
(require rosette/base/core/bool)
;; A State is a (Listof (Pair SymbolicVariable (|| Boolean Unknown)))
;; A Transition is a (ScoreMap State -> State)
;; A ScoreMap is a (Hashof State (|| Number False))
;; A Score is a (State -> Number)

;; Terminated the program with the provided error message
;; Needed because raise/error only terminates execution in the current context/thread. 
(define (die msg)
  (eprintf "Error: ~a\n" msg)  ; eprintf writes to stderr
  (exit 1))

;; ScoreMap -> Boolean
;; Returns if all states have been assigned a numerical score or 0 samples left
(define (search-complete? score-map samples state-space)
  (if state-space
      (for/and ([state state-space])
                      (and (hash-has-key? score-map state)
                           (hash-ref score-map state)))
      (equal? samples 0)))

;; ScoreMap -> State
;; Returns the State with the highest associated score
(define (highest-scoring score-map)
  (car (argmax cdr (hash->list score-map))))

;; Number (-> Any) (-> Any)
;; Runs thnk with a time limit and returns the value of running thnk, or running default after timing out
(define (with-timeout timeout-duration thnk default)
  (let* ([ch (make-channel)]
         [th (thread (lambda () 
                       (channel-put ch (thnk))))]
         [out (sync/timeout timeout-duration ch)])
    (if out 
        out
        (begin
          (kill-thread th)  ; Kill the thread if timeout occurred
          (default)))))

;; -> (|| (Pairof State Number) False) -> (|| void (Hashof Symbolic Variable Number))
;; Produces a hash of variable assignment accumulated from the provided state+scores
(define (make-heuristics) 
  (let ([freq-map (make-hash)])
    (lambda (state+score)
      (if state+score 
          (let ([state (car state+score)]
                [score (cdr state+score)])
            (when (>= score 0)
                  (begin 
                    (for/list ([var+asgn state])
                      (match-define (cons var asgn) var+asgn)
                      (when (not (unknown? asgn))
                        (hash-set! freq-map var (+ 1 (hash-ref freq-map var 0)))))
                    (hash-set! freq-map 'Total-runs (+ 1 (hash-ref freq-map 'Total-runs 0))))))
          freq-map))))


;; Performs the search for the highest scoring state based on the score-func.
;; Returns the State with highest score
;; Exactly one of the keyword arguments #:samples or #:state-space must be provided
;; Use state-space for exhaustive search and samples for sampling-based search algorithms.

;; State Number Transition Score [#:samples Number] [#:state-space (ListOf State)] -> State
(define (search start-state timeout-duration transition score-func #:samples [samples #f]
                                                                   #:state-space [state-space #f])
    ;; State ScoreMap Number Channel -> State
    (define (search/acc start-state score-map samples ch)
        (if (search-complete? score-map samples state-space)
            (channel-put ch 'done)
            (let* ([start-state-score
                    (score-func start-state)]
                    [new-score-map (hash-set score-map start-state start-state-score)]
                    [new-samples (if (and samples (>= start-state-score 0)) 
                                     (- samples 1) 
                                     samples)]
                    [next-state (if (not (search-complete? new-score-map new-samples state-space))
                                    (transition new-score-map start-state)
                                    start-state)])
                (channel-put ch (cons start-state start-state-score)) ; every time a state is explored, update in channel
                (search/acc next-state new-score-map new-samples ch))))

    (unless (xor samples state-space)
      (die "exactly one of state-space or samples must be provided to perform search"))
    (define score-map (hash))

    (define ch (make-channel))
    (define out-ch (make-channel))
    (define heuristics (make-heuristics))
    (define producer (thread (lambda () (search/acc start-state score-map samples ch))))
    (define consumer (thread (lambda () 
                       (let loop () 
                         (define item (channel-get ch))
                         (cond [(eq? item 'done) (channel-put out-ch (heuristics #f))]
                               [else (heuristics item) (loop)])))))

    
    (channel-get out-ch))




(struct unknown ())

(define UNKNOWN (unknown))

;; enumerate all possible combinations of assignments of booleans and unknowns to the provided variables
;; (Listof SymbolicVariables) -> (Listof State
(define (enumerate-state-space vars)
  (apply cartesian-product
         (for/list [(v vars)]
           (list (cons v UNKNOWN) (cons v #t) (cons v #f)))))
 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEARCH ALGORITHM IMPLEMENTATIONS (EXHAUSTIVE, SAMPLING, ETC.)

;; (Listof State) -> (ScoreMap State -> State)
;; Given the list of all states (state space), transitions iteratively through all states,
;; and dies when there are no more states to transition to
;; this is probably not the most efficient implementation, but oh well ... 
(define exhaustive-transition-func
  (lambda (state-space)
    (let ([remaining state-space]) 
      (lambda (scoremap state) 
        (begin
          (if (empty? remaining)
            (die "All possible states exhausted, no remaining states to transition to... ")
            (let ([ret (first remaining)])
              (set! remaining (rest remaining))
              ret)))))))

(define (random-set-element s)
  (let ([lst (set->list s)])
    (if (empty? lst)
        (die "no more elements in provided set")
        (list-ref lst (random (length lst))))))

;; Produce a list of all the states reachable by specialization (ie. setting one unknown)
;; State -> (Listof State)
(define (reachable-by-specialization state)
  (define unknown-positions
    (for/list ([val (in-list state)]
               [pos (in-naturals)]
               #:when (unknown? (cdr val)))
      pos))
  
  (if (empty? unknown-positions)
      (mutable-set)
      (for*/mutable-set ([random-pos unknown-positions]
                  [asgn (in-list (list #t #f))])
        (let* ([old (list-ref state random-pos)])
              (for/list ([val (in-list state)]
                         [pos (in-naturals)])
                (if (= pos random-pos)
                    (cons (car old) asgn)
                    val))))))

#;(reachable-by-specialization (list (cons 'a UNKNOWN) (cons 'b UNKNOWN)))

;; Randomly chooses of the states reachable by specialization
;; State -> State
#;(define (random-specialize state)
  (random-set-element (reachable-by-specialization state)))
(define (random-specialize state)
  (define unknown-positions
    (for/list ([val (in-list state)]
               [pos (in-naturals)]
               #:when (unknown? (cdr val)))
      pos))
  
  (if (empty? unknown-positions)
      (die "out of states to specialize")
      (let* ([random-pos (list-ref unknown-positions (random (length unknown-positions)))]
             [asgn (= 1 (random 2))]
             [old (list-ref state random-pos)])
        (for/list ([val (in-list state)]
                   [pos (in-naturals)])
          (if (= pos random-pos)
              (cons (car old) asgn)
              val)))))

#;(random-specialize (list (cons 'a #f) (cons 'b UNKNOWN) (cons 'c #f) (cons 'd UNKNOWN) (cons 'e UNKNOWN)))



(define (list-union lst1 lst2)
  (for/fold ([acc lst1])
            ([elm lst2])
    (if (member elm acc)
        acc
        (cons elm acc))))


(define random-specialization-transition 
  (lambda (initial-state)
    (let ([attempts 1]
          [samples 1]) 
      (lambda (scoremap state)
        (if (>= (hash-ref scoremap state) 0) ; => prev state ran successfully 
                (begin
                  (printf "~v: Found a working sample in ~v attempt(s)\n\n\n" samples attempts)
                  (set! attempts 1)
                  (set! samples (+ samples 1))
                  initial-state)
                (begin
                  (printf "~v: Attempt ~v: failed\n\n\n" samples attempts)
                  (set! attempts (+ attempts 1))
                  
                  (for/fold ([acc state])
                                        ([x (in-range 1 21 1)])
                                (random-specialize acc))))))))


#;(define random-specialization-transition 
  (lambda (initial-state) 
    (let ([frontier (reachable-by-specialization initial-state)]
          [attempts 1]
          [samples 1])
      (lambda (scoremap state)
        ; The next state is always from the frontier. To pick the next state, we have 2 options:

        ; If the previous state ran successfully within the time limit, then we choose randomly from 
        ; the frontier. 

        ; If the previous state did not run within time, then we specialize the previous state to get 
        ; the next state. This new state also must be in the frontier, if we maintain frontier correctly

        (when (set-empty? frontier) (die "Ran out of states to sample from")) 
        (define next-state
          (if (>= (hash-ref scoremap state) 0) ; => prev state ran successfully 
              (begin
                (printf "~v: Found a working sample in ~v attempt(s)\n\n\n" samples attempts)
                (set! attempts 1)
                (set! samples (+ samples 1))
                (random-set-element frontier))
              (begin
                (printf "~v: Attempt ~v: failed\n\n\n" samples attempts)
                (set! attempts (+ attempts 1))
                (random-specialize state))))

        ; Remove next state from frontier
        (set-remove! frontier next-state)

        ;Add new reachable states to the frontier
        (time (set-union! frontier (reachable-by-specialization next-state)))
        
        next-state))))