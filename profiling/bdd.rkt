#lang racket

;; A tiny BDD implementation

(provide (all-defined-out))

;; BDD pointer type (just an integer)
(define bdd-ptr? exact-nonnegative-integer?)

;; BDD data structure
(struct bdd-true () #:transparent)
(struct bdd-false () #:transparent)
(struct bdd-node (topvar low high) #:transparent)

;; BDD table structure
(struct bdd-table (backing-table     ; vector of BDDs
                   compute-table     ; hash: bdd -> int (for uniqueness)
                   next-free         ; box containing next free index
                   num-vars          ; box containing number of allocated vars
                   memo-table        ; hash: (ptr . ptr) -> ptr (for AND)
                   recursive-calls)  ; box containing count of recursive calls
  #:transparent
  #:mutable)

(define true-ptr 0)
(define false-ptr 1)

;; Get the number of recursive calls
(define (bdd-num-recursive-calls)
  (unbox (bdd-table-recursive-calls global-bdd-table)))

;; Reset the recursive call counter
(define (bdd-reset-recursive-calls)
  (set-box! (bdd-table-recursive-calls global-bdd-table) 0))

;; Create a fresh BDD table
(define (fresh-bdd-table)
  (let ([arr (make-vector 10000000 (bdd-true))]
        [compute-tbl (make-hash)]
        [memo-tbl (make-hash)])
    (hash-set! compute-tbl (bdd-true) true-ptr)
    (hash-set! compute-tbl (bdd-false) false-ptr)
    (vector-set! arr 0 (bdd-true))
    (vector-set! arr 1 (bdd-false))
    (bdd-table arr compute-tbl (box 2) (box 0) memo-tbl (box 0))))

;; Global BDD table
(define global-bdd-table (fresh-bdd-table))

;; Reset the global BDD table to a fresh state
(define (reset-bdd-table!)
  (set! global-bdd-table (fresh-bdd-table))
  (set! global-wmc-params (wmc-params (make-hash) 1.0 0.0)))

;; Dereference a BDD pointer
(define (deref-bdd ptr)
  (vector-ref (bdd-table-backing-table global-bdd-table) ptr))

;; Get or insert a fresh BDD into the table
(define (get-or-insert bdd)
  (define cached (hash-ref (bdd-table-compute-table global-bdd-table) bdd #f))
  (if cached
      cached
      (let ([new-idx (unbox (bdd-table-next-free global-bdd-table))])
        (set-box! (bdd-table-next-free global-bdd-table) (add1 new-idx))
        (vector-set! (bdd-table-backing-table global-bdd-table) new-idx bdd)
        (hash-set! (bdd-table-compute-table global-bdd-table) bdd new-idx)
        new-idx)))

;; Convert BDD to string for debugging
(define (string-of-bdd f)
  (match (deref-bdd f)
    [(bdd-true) "T"]
    [(bdd-false) "F"]
    [(bdd-node topvar low high)
     (format "(~a ~a ~a)" topvar (string-of-bdd low) (string-of-bdd high))]))


;; Get a pointer to a fresh BDD variable with the given value
(define (fresh-var value)
  (let ([fresh-var-num (unbox (bdd-table-num-vars global-bdd-table))])
    (set-box! (bdd-table-num-vars global-bdd-table) (add1 fresh-var-num))
    (if value
        (get-or-insert (bdd-node fresh-var-num false-ptr true-ptr))
        (get-or-insert (bdd-node fresh-var-num true-ptr false-ptr)))))

;; Get a pointer to a BDD with topvariable `topvar`
(define (bdd-var topvar)
  ;; Assert that this variable has been allocated via fresh-var
  (unless (< topvar (unbox (bdd-table-num-vars global-bdd-table)))
    (error "Variable not allocated via fresh-var"))
  (get-or-insert (bdd-node topvar false-ptr true-ptr)))

;; Get the top variable of a BDD node
(define (topvar f)
  (match (deref-bdd f)
    [(bdd-node topvar _ _) topvar]
    [_ (error "Tried to call topvar on non-node")]))

;; Negate a BDD
(define (bdd-not f)
  (define memo (make-hash))
  (define (neg-h f)
    ;; Increment recursive call counter
    (set-box! (bdd-table-recursive-calls global-bdd-table)
              (add1 (unbox (bdd-table-recursive-calls global-bdd-table))))
    (define cached (hash-ref memo f #f))
    (if cached
        cached
        (let ([result
               (match (deref-bdd f)
                 [(bdd-true) false-ptr]
                 [(bdd-false) true-ptr]
                 [(bdd-node topvar low high)
                  (get-or-insert (bdd-node topvar (neg-h low) (neg-h high)))])])
          (hash-set! memo f result)
          result)))
  (neg-h f))

;; Conjoin two BDDs
(define (bdd-and f g)
  ;; Increment recursive call counter
  (set-box! (bdd-table-recursive-calls global-bdd-table)
            (add1 (unbox (bdd-table-recursive-calls global-bdd-table))))
  ;; Check for cached BDD
  (define cached (hash-ref (bdd-table-memo-table global-bdd-table) (cons f g) #f))
  (if cached
      cached
      ;; No cached BDD, compute conjunction
      (let ([result
             (match* ((deref-bdd f) (deref-bdd g))
               ;; False cases
               [(_ (bdd-false)) false-ptr]
               [((bdd-false) _) false-ptr]
               ;; True cases
               [((bdd-true) (bdd-true)) true-ptr]
               [((bdd-true) (bdd-node _ _ _)) g]
               [((bdd-node _ _ _) (bdd-true)) f]
               ;; Node cases - same topvar
               [((bdd-node f-topvar f-low f-high) (bdd-node g-topvar g-low g-high))
                #:when (= f-topvar g-topvar)
                (let ([l (bdd-and f-low g-low)]
                      [h (bdd-and f-high g-high)])
                  (if (= l h)
                      l
                      (get-or-insert (bdd-node f-topvar l h))))]
               ;; Node cases - different topvar
               [((bdd-node f-topvar f-low f-high) (bdd-node g-topvar g-low g-high))
                #:when (< f-topvar g-topvar)
                (let ([l (bdd-and f-low g)]
                      [h (bdd-and f-high g)])
                  (if (= l h)
                      l
                      (get-or-insert (bdd-node f-topvar l h))))]
               [((bdd-node f-topvar f-low f-high) (bdd-node g-topvar g-low g-high))
                (let ([l (bdd-and f g-low)]
                      [h (bdd-and f g-high)])
                  (if (= l h)
                      l
                      (get-or-insert (bdd-node g-topvar l h))))])])
        ;; Cache the conjunction
        (hash-set! (bdd-table-memo-table global-bdd-table) (cons f g) result)
        result)))

;; Disjoin two BDDs (via De Morgan's law)
(define (bdd-or f g)
  (let ([negf (bdd-not f)]
        [negg (bdd-not g)])
    (bdd-not (bdd-and negf negg))))

;; Implication: f → g = ¬f ∨ g
(define (bdd-implies f g)
  (bdd-or (bdd-not f) g))

;; Biconditional (if and only if): f ↔ g = (f ∧ g) ∨ (¬f ∧ ¬g)
(define (bdd-iff f g)
  (let ([f-and-g (bdd-and f g)]
        [neg-f (bdd-not f)]
        [neg-g (bdd-not g)])
    (bdd-or f-and-g (bdd-and neg-f neg-g))))

;; If-then-else operation: ite(guard, then, else) = (guard ∧ then) ∨ (¬guard ∧ else)
(define (bdd-ite guard then else)
  (let* ([guard-and-then (bdd-and guard then)]
         [neg-guard (bdd-not guard)]
         [neg-guard-and-else (bdd-and neg-guard else)])
    (bdd-or guard-and-then neg-guard-and-else)))

;; Basic tests
(define (test-canonicity-neg)
  (reset-bdd-table!)
  (define b1 (fresh-var #t))
  (define b1n (bdd-not b1))
  (define b2n (bdd-not b1))
  (unless (= b1n b2n)
    (error "Canonicity test for negation failed")))

(define (test-canonicity-and)
  (reset-bdd-table!)
  (define a (fresh-var #t))
  (define b (fresh-var #t))
  (define a2 (bdd-var 0))
  (define b2 (bdd-var 1))
  (unless (= (bdd-and a b) (bdd-and b2 a2))
    (error "Canonicity test for conjunction failed")))

(define (test-unsat)
  (reset-bdd-table!)
  (define a (fresh-var #t))
  (unless (= (bdd-and a (bdd-not a)) false-ptr)
    (error "UNSAT test failed")))

(define (test-valid)
  (reset-bdd-table!)
  (define a (fresh-var #t))
  (unless (= (bdd-or a (bdd-not a)) true-ptr)
    (error "Valid test failed")))

;; Weight structure for weighted model counting
(struct weight (low-w high-w) #:transparent)

;; WMC parameters
(struct wmc-params (weights one zero) #:transparent #:mutable)

;; Global WMC parameters
(define global-wmc-params (wmc-params (make-hash) 1.0 0.0))

;; Set the weight for a variable in the global wmc-params
;; var-ptr: pointer to a BDD variable
;; false-weight: weight when the variable is false (low branch)
;; true-weight: weight when the variable is true (high branch)
(define (set-var-weight! var-ptr false-weight true-weight)
  (let ([var-num (topvar var-ptr)])
    (hash-set! (wmc-params-weights global-wmc-params)
               var-num
               (weight false-weight true-weight))))

;; Perform unsmoothed weighted model counting
(define (wmc f)
  (define memo (make-hash))
  (define (wmc-h f)
    (define cached (hash-ref memo f #f))
    (if cached
        cached
        (let ([result
               (match (deref-bdd f)
                 [(bdd-true) (wmc-params-one global-wmc-params)]
                 [(bdd-false) (wmc-params-zero global-wmc-params)]
                 [(bdd-node topvar low high)
                  (let* ([wt (hash-ref (wmc-params-weights global-wmc-params) topvar
                                       (λ () (error 'wmc "no weight found for variable ~a (ptr=~a)" topvar f)))]
                         [low-wmc (wmc-h low)]
                         [high-wmc (wmc-h high)])
                    (+ (* (weight-low-w wt) low-wmc)
                       (* (weight-high-w wt) high-wmc)))])])
          (hash-set! memo f result)
          result)))
  (wmc-h f))


;; Helper for floating point comparison
(define (within-epsilon a b)
  (< (abs (- a b)) 0.0001))

;; Test WMC
(define (test-wmc)
  (reset-bdd-table!)
  (define a (fresh-var #t))
  (define b (fresh-var #t))
  (define c (fresh-var #t))
  (define disj (bdd-or a (bdd-or b c)))
  ;; Set weights using the global params
  (set-var-weight! a 0.5 0.5)
  (set-var-weight! b 0.5 0.5)
  (set-var-weight! c 0.5 0.5)
  (unless (within-epsilon (wmc disj) 0.875)
    (error "WMC test failed")))

;; Run all tests
(define (run-all-tests)
  (test-canonicity-neg)
  (test-canonicity-and)
  (test-unsat)
  (test-valid)
  (test-wmc)
  (displayln "All BDD tests passed!"))

;; Uncomment to run tests
;; (run-all-tests)
