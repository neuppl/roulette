#lang roulette/example/disrupt
(require "hash-set.rkt"
         "guards.rkt")
(provide (all-defined-out)
         (all-from-out "hash-set.rkt")
         (all-from-out "guards.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program representation

;; name is the predicate identifier, and
;; args is a list of any (concrete) arguments supplied to the fact.

;; args can be symbols, which represent variables 
(struct fact (name args)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (write-string (symbol->string (fact-name self)) port)
     (write-string "(" port)
     (for ([a (fact-args self)] [i (in-naturals)])
       (when (> i 0) (write-string ", " port))
       (if (string? a)
           (write-string (format "~s" a) port)
           (write a port)))
     (write-string ")" port))])

;; head is the derived fact, and body is a list of
;; facts (clauses) that must be satisfied. 
(struct rule (head body) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching

;; Match a fact pattern against f, given existing bindings. Returns
;; extended bindings, or #f if the match fails.
(define (match-fact pattern f bindings)
  (and (equal? (fact-name pattern) (fact-name f))
       (= (length (fact-args pattern)) (length (fact-args f)))
       (match-args (fact-args pattern) (fact-args f) bindings)))

(define (match-args pattern-args fact-args bindings)
  (cond
    [(null? pattern-args) bindings]
    [else
     (define p (car pattern-args))
     (define a (car fact-args))
     (cond
       [(symbol? p)
        (cond
          [(hash-has-key? bindings p)
           (and (equal? (hash-ref bindings p) a)
                (match-args (cdr pattern-args) (cdr fact-args) bindings))]
          [else
           (match-args (cdr pattern-args) (cdr fact-args)
                       (hash-set bindings p a))])]
       [else
        (and (equal? p a)
             (match-args (cdr pattern-args) (cdr fact-args) bindings))])]))

(define (substitute head bindings)
  (fact (fact-name head)
        (map (lambda (a) (if (symbol? a) (hash-ref bindings a) a))
             (fact-args head))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Probabilistic fact database

;; base-fact-probs : list of (cons fact probability)
;;
;; The only place a base fact's guard is created, which makes it the one
;; place that decides how the guards are represented and -- for a backend
;; where it matters, such as BDDs -- what order the variables come in.
(define (make-base-set base-fact-probs)
  (for/sym-set ([fp base-fact-probs])
    (values (car fp) (guard-var (cdr fp)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immediate consequence operator (semi-naive)

;; A per-predicate index: all is a fallback list of every (fact .
;; guard) pair for this predicate; by-pos maps an argument position to
;; a hash from the value seen at that position to the (fact . guard)
;; pairs that have it, so a clause with an already-known argument
;; (a literal constant, or an already-bound variable) can look up only
;; the facts that could possibly match, instead of scanning every fact
;; of that predicate.
(struct pred-index (all by-pos) #:transparent)
(define (empty-pred-index) (pred-index '() (hash)))

(define (pred-index-add pi f g)
  (define new-by-pos
    (for/fold ([bp (pred-index-by-pos pi)]) ([a (fact-args f)] [i (in-naturals)])
      (hash-update bp i
                   (lambda (inner) (hash-update inner a (lambda (l) (cons (cons f g) l)) '()))
                   (hash))))
  (pred-index (cons (cons f g) (pred-index-all pi)) new-by-pos))

;; predicate name -> pred-index
(define (index-by-name st)
  (for/fold ([idx (hash)]) ([(k g) st])
    (hash-update idx (fact-name k)
                 (lambda (pi) (pred-index-add pi k g))
                 (lambda () (pred-index-add (empty-pred-index) k g)))))

;; First argument position in clause whose value is already known
;; (a literal constant, or a variable already in bindings), as
;; (cons position value) — or #f if every argument is still unbound.
(define (known-arg clause bindings)
  (let loop ([args (fact-args clause)] [i 0])
    (cond
      [(null? args) #f]
      [else
       (define a (car args))
       (cond
         [(symbol? a)
          (if (hash-has-key? bindings a)
              (cons i (hash-ref bindings a))
              (loop (cdr args) (add1 i)))]
         [else (cons i a)])])))

;; Candidates for clause: value-narrowed via known-arg when possible,
;; else the full (unnarrowed) list for that predicate.
(define (candidates-for clause bindings idx)
  (define pi (hash-ref idx (fact-name clause) (empty-pred-index)))
  (define ka (known-arg clause bindings))
  (if ka
      (hash-ref (hash-ref (pred-index-by-pos pi) (car ka) (hash)) (cdr ka) '())
      (pred-index-all pi)))

;; Matches body left to right; the clause at delta-pos draws from
;; delta-idx, every other clause draws from full-idx.
;; Produces a list of (bindings . guard) pairs, or "world"s 
(define (find-bindings-prob/at body full-idx delta-idx delta-pos)
  (for/fold ([worlds (list (cons (hash) (guard-true)))])
            ([clause body] [i (in-naturals)])
    (define idx (if (= i delta-pos) delta-idx full-idx))
    (for*/list ([w worlds]
                [fg (candidates-for clause (car w) idx)]
                [b (in-value (match-fact clause (car fg) (car w)))]
                #:when b)
      ;; A world holds when every clause matched so far does, so the
      ;; guards conjoin. This has to be `guard-and` rather than `and`:
      ;; Rosette lifts `and` over symbolic booleans, which happens to do
      ;; the right thing for term guards, but on any other representation
      ;; it just returns the last operand and quietly drops the rest.
      (cons b (guard-and (cdr w) (cdr fg))))))


;; `delta` is what was freshly derived in the most recent iteration.
;; Only derivations using delta in at least one clause position are
;; computed, since anything using only older facts was already found.

;; try every clause position as the required-delta position.
;; produces a list of worlds (bindings . guard) pairs
(define (find-bindings-prob/delta body full delta)
  (define full-idx (time-it! add-index-time! (lambda () (index-by-name full))))
  (define delta-idx (time-it! add-index-time! (lambda () (index-by-name delta))))
  (define n (length body))
  (for*/list ([delta-pos (in-range n)]
              [w (find-bindings-prob/at body full-idx delta-idx delta-pos)])
    w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timing instrumentation

(define total-find-bindings-time 0.0)
(define total-guard-build-time 0.0)
(define total-set-union-time 0.0)
(define total-index-time 0.0)

(define (time-it! updater! thunk)
  (define start (current-inexact-monotonic-milliseconds))
  (define result (thunk))
  (updater! (- (current-inexact-monotonic-milliseconds) start))
  result)

(define (add-find-bindings-time! dt) (set! total-find-bindings-time (+ total-find-bindings-time dt)))
(define (add-guard-build-time! dt) (set! total-guard-build-time (+ total-guard-build-time dt)))
(define (add-set-union-time! dt) (set! total-set-union-time (+ total-set-union-time dt)))
(define (add-index-time! dt) (set! total-index-time (+ total-index-time dt)))

(define (rule-apply-prob/delta r full delta)
  (define bindings (time-it! add-find-bindings-time!
                              (lambda () (find-bindings-prob/delta (rule-body r) full delta))))
  ;; for/sym-set/fast (rather than for/sym-set) since the same head
  ;; fact is typically derived from many different bindings.
  (time-it! add-guard-build-time!
            (lambda ()
              (for/sym-set/fast ([w bindings])
                (values (substitute (rule-head r) (car w)) (cdr w))))))

;; Returns (values new-full new-delta): new-delta is exactly what's
;; fresh this round, new-full is full ∪ new-delta.
(define (immediate-prob full delta rules)
  (for/fold ([full-acc full] [new-acc (set)])
            ([r rules])
    (define delta-pool (time-it! add-set-union-time! (lambda () (set-union delta new-acc))))
    (define fresh (rule-apply-prob/delta r full-acc delta-pool))
    (define new-full-acc (time-it! add-set-union-time! (lambda () (set-union full-acc fresh))))
    (define new-new-acc (time-it! add-set-union-time! (lambda () (set-union new-acc fresh))))
    (values new-full-acc new-new-acc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immediate consequence operators for canonical guards
;;
;; `immediate-prob` above is semi-naive in the textbook sense: it
;; derives from `delta`, and the guard it builds therefore says "newly
;; derivable *this round*". That is a strictly more complicated
;; condition than "derivable" -- it has to encode the absence of the
;; shorter derivations as well as the presence of a new one -- and with
;; a canonical representation the intermediate diagrams dwarf the
;; answer: on a friends-and-smokers ring of 14 it costs 231 million BDD
;; operations where the operators below cost under 180 thousand, for
;; the same result.
;;
;; The mistake is transplanting a rule about *set*-valued Datalog onto
;; guard-valued Datalog. Classically a derivation from facts that all
;; existed before was already performed, so it can be skipped. Here a
;; fact can exist from round one while its guard keeps weakening for
;; many rounds, so the condition to test is whether a guard *changed*,
;; and the re-derivation must use current guards rather than deltas of
;; guards.
;;
;; `immediate-semi` does that: the delta selects which facts to match,
;; and every guard is read out of the accumulated set. `immediate-naive`
;; drops the selection too, and is kept as the simplest thing that
;; works and as a baseline to measure against.
(define (rule-apply-prob/full r full)
  (define idx (time-it! add-index-time! (lambda () (index-by-name full))))
  ;; -1 is not a clause position, so no clause is restricted to a delta.
  (define bindings (time-it! add-find-bindings-time!
                             (lambda () (find-bindings-prob/at (rule-body r) idx idx -1))))
  (time-it! add-guard-build-time!
            (lambda ()
              (for/sym-set/fast ([w bindings])
                (values (substitute (rule-head r) (car w)) (cdr w))))))

(define (immediate-naive full rules)
  (for/fold ([acc full]) ([r rules])
    (time-it! add-set-union-time!
              (lambda () (set-union acc (rule-apply-prob/full r acc))))))

;; The given keys, carrying the guards they have in `full` rather than
;; any partial ones. This is what keeps a delta from ever becoming a
;; "newly derivable this round" condition.
(define (restrict-to full keys)
  (for/sym-set/fast ([k keys])
    (values k (set-member? full k))))

;; The keys whose guard is not the one they had before. Cheap exactly
;; where this operator is used, since comparing two canonical guards is
;; a pointer comparison.
(define (changed-keys old new)
  (for/list ([(k g) new] #:unless (guard-equiv? g (set-member? old k))) k))

;; One round of semi-naive evaluation over accumulated guards: a
;; derivation must use at least one changed fact somewhere, but every
;; clause's guard comes from `full`. Skipping the rest is sound because
;; if no body guard changed then neither did their conjunction, so the
;; contribution is already present and re-adding it would be a no-op.
(define (immediate-semi full delta-keys rules)
  (define full-idx (time-it! add-index-time! (lambda () (index-by-name full))))
  (define delta-idx
    (time-it! add-index-time!
              (lambda () (index-by-name (restrict-to full delta-keys)))))
  (for/fold ([acc full]) ([r rules])
    (define n (length (rule-body r)))
    ;; Each clause position in turn is the one required to draw from the
    ;; delta. A derivation with two changed facts is therefore built
    ;; twice; with canonical guards the second is free.
    (define bindings
      (time-it! add-find-bindings-time!
                (lambda ()
                  (for*/list ([pos (in-range n)]
                              [w (find-bindings-prob/at (rule-body r) full-idx delta-idx pos)])
                    w))))
    (define fresh
      (time-it! add-guard-build-time!
                (lambda ()
                  (for/sym-set/fast ([w bindings])
                    (values (substitute (rule-head r) (car w)) (cdr w))))))
    (time-it! add-set-union-time! (lambda () (set-union acc fresh)))))

;; Reports a runtime failure the way the parser reports a syntax one:
;; prefixed with the source location of the statement responsible.
;; `where` is that location, already formatted, and is #f when these
;; are called directly from Racket rather than from a statement.
(define (probalog-error who where fmt . args)
  (raise (make-exn:fail (format "~a: ~a" (or where who) (apply format fmt args))
                        (current-continuation-marks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weighing and conditioning guards
;;
;; Under the term backend a guard is a Rosette term, so Roulette's own
;; `query` and `observe!` do this work. Under the BDD backend a guard is
;; an opaque handle that Roulette cannot see into, so the same semantics
;; are computed directly: conditioning conjoins evidence, and a marginal
;; is P(fact and evidence) / P(evidence) -- which is what Roulette's
;; `query` computes too, by normalising over `(if evidence e ⊥)`.

;; The accumulated evidence, as a guard. #f before anything has been
;; observed, since `(guard-true)` cannot be evaluated until a backend has
;; been chosen.
(define bdd-evidence (box #f))
(define (current-evidence) (or (unbox bdd-evidence) (guard-true)))

;; A pmf over #t/#f for a guard, conditioned on the evidence so far, or
;; #f when no world satisfies the evidence at all -- matching what
;; Roulette's `query` returns in that case.
(define (guard->pmf g)
  (cond
    [(bdd-guards?)
     (define ev (current-evidence))
     (define denominator (guard-prob ev))
     (and (positive? denominator)
          (let ([p (/ (guard-prob (guard-and g ev)) denominator)])
            ;; Dropping zero-probability outcomes is what leaves a certain
            ;; answer as a single-outcome pmf, which `query-result->string`
            ;; then prints as #t or #f rather than as a table.
            (for/pmf ([value (in-list (list #t #f))]
                      [prob (in-list (list p (- 1 p)))]
                      #:unless (zero? prob))
              (values value prob))))]
    [else (query g)]))

;; Condition all later queries on `g` holding.
(define (add-evidence! g)
  (if (bdd-guards?)
      (set-box! bdd-evidence (guard-and (current-evidence) g))
      (observe! g)))

;; re-exporting query from roulette/example/disrupt as query-fact
(define (query-fact result f #:where [where #f])
  (define pmf (guard->pmf (set-member? result f)))
  (unless pmf
    (probalog-error
     'query-fact where
     (string-append
      "no possible world remains, so ~a has no probability\n"
      "  the observations made so far cannot all hold at once")
     f))
  pmf)

;; How a query result is shown. A distribution over a single outcome
;; carries no uncertainty, so it prints as that value rather than as a
;; one-row table — the same choice Disrupt makes when printing a query
;; whose result turned out to be concrete.
(define (query-result->string pmf)
  (define outcomes (for/list ([(value prob) (in-pmf pmf)]) value))
  (if (= (length outcomes) 1)
      (format "~a" (car outcomes))
      (format "~a" pmf)))

;; Whether `guard` can still take the given value in some world that
;; satisfies every observation made so far. `query` returns #f when no
;; world satisfies the observations at all.
(define (possible? guard value)
  (define pmf (guard->pmf guard))
  (and pmf
       (for/or ([(v p) (in-pmf pmf)])
         (and (equal? v value) (positive? p)))))

;; Conditioning on something impossible divides by zero, and every
;; later query would silently report nothing rather than fail. So an
;; observation is checked before it is made, while it can still be
;; blamed on the statement that caused it.
(define (check-observable! who where guard value what)
  (unless (possible? guard value)
    (probalog-error
     who where
     (string-append
      "cannot observe ~a: it has probability 0\n"
      "  given the base facts, the rules, and any earlier observations,\n"
      "  there is no possible world in which this holds")
     what)))

;; Condition the current probability distribution on the given fact
;; being present (or absent) in the result set. All subsequent calls
;; to query-fact (or query) are automatically conditioned on this
;; observation. Observing something impossible is rejected rather than
;; dividing by zero; see check-observable! above.
(define (observe-fact result f #:where [where #f])
  (define guard (set-member? result f))
  (check-observable! 'observe-fact where guard #t f)
  (add-evidence! guard))

(define (observe-not-fact result f #:where [where #f])
  (define guard (set-member? result f))
  (check-observable! 'observe-not-fact where guard #f
                     (format "the absence of ~a" f))
  (add-evidence! (guard-not guard)))

;; Lower-level: condition on an arbitrary guard formula, e.g. a
;; disjunction of several facts being present.
(define (observe-guard g #:where [where #f])
  (check-observable! 'observe-guard where g #t "this formula")
  (add-evidence! g))