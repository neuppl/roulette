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

;; Put base facts sharing a constant next to each other: this is the BDD
;; variable order, and facts that can meet in a derivation are the ones
;; that want to be adjacent. Breadth-first over constants (Cuthill-McKee),
;; so it is invariant under renaming the data.
(define (order-base-facts base-fact-probs)
  ;; constants that share a fact are adjacent
  (define adj (make-hash))
  (for ([fp base-fact-probs])
    (define as (fact-args (car fp)))
    (for ([a as])
      (unless (hash-has-key? adj a) (hash-set! adj a (make-hash))))
    (for* ([a as] [b as] #:unless (equal? a b))
      (hash-set! (hash-ref adj a) b #t)))
  ;; breadth-first visit order, restarting for each component
  (define idx (make-hash))
  (define counter 0)
  (define (visit! c)
    (unless (hash-has-key? idx c)
      (hash-set! idx c counter)
      (set! counter (add1 counter))))
  (for* ([fp base-fact-probs] [start (fact-args (car fp))])
    (unless (hash-has-key? idx start)
      (visit! start)
      (let loop ([queue (list start)])
        (unless (null? queue)
          (define fresh
            (for/fold ([acc '()]) ([(nb _) (in-hash (hash-ref adj (car queue)))])
              (cond
                [(hash-has-key? idx nb) acc]
                [else (visit! nb) (cons nb acc)])))
          (loop (append (cdr queue) (reverse fresh)))))))
  ;; a fact sits at its earliest-visited argument; nullary facts, which
  ;; share a constant with nothing, keep their declared order in front
  (define (rank fp)
    (for/fold ([m -1]) ([a (fact-args (car fp))])
      (define i (hash-ref idx a 0))
      (if (or (= m -1) (< i m)) i m)))
  ;; `sort` is stable, so declaration order breaks ties
  (sort base-fact-probs < #:key rank))

;; base-fact-probs : list of (cons fact probability). The only place a
;; base fact's guard is made, hence the only place the variable order is
;; decided.
(define (make-base-set base-fact-probs)
  (for/sym-set ([fp (order-base-facts base-fact-probs)])
    (values (car fp) (guard-var (cdr fp)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immediate consequence operator (semi-naive)


;; all is a fallback for by-pos, which indexes arguments at every position.
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

(define (candidates-for clause bindings idx)
  (define pi (hash-ref idx (fact-name clause) (empty-pred-index)))
  (define ka (known-arg clause bindings))
  (if ka
      (hash-ref (hash-ref (pred-index-by-pos pi) (car ka) (hash)) (cdr ka) '())
      (pred-index-all pi)))

(define (find-bindings-prob/at body full-idx delta-idx delta-pos)
  (for/fold ([worlds (list (cons (hash) (guard-true)))])
            ([clause body] [i (in-naturals)])
    (define idx (if (= i delta-pos) delta-idx full-idx))
    (for*/list ([w worlds]
                [fg (candidates-for clause (car w) idx)]
                [b (in-value (match-fact clause (car fg) (car w)))]
                #:when b)
      (cons b (guard-and (cdr w) (cdr fg))))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immediate consequence operator
;;
;; The delta is keys whose guard *changed*, not facts newly derived, and
;; it only selects which facts to match -- every guard in a derivation is
;; read from the accumulated set. A fact can exist from round one while
;; its guard keeps weakening, so a delta of guards would build "newly
;; derivable this round", a far larger condition than "derivable".

;; The given keys, carrying their guards from `full`.
(define (restrict-to full keys)
  (for/sym-set ([k keys])
    (values k (set-member? full k))))

;; Keys whose guard changed; a pointer comparison each.
(define (changed-keys old new)
  (for/list ([(k g) new] #:unless (guard-equiv? g (set-member? old k))) k))

(define (immediate-semi full delta-keys rules)
  (define full-idx (time-it! add-index-time! (lambda () (index-by-name full))))
  (define delta-idx
    (time-it! add-index-time!
              (lambda () (index-by-name (restrict-to full delta-keys)))))
  (for/fold ([acc full]) ([r rules])
    (define n (length (rule-body r)))
    ;; Each clause position in turn draws from the delta; a derivation
    ;; with two changed facts is built twice, and the second is free.
    (define bindings
      (time-it! add-find-bindings-time!
                (lambda ()
                  (for*/list ([pos (in-range n)]
                              [w (find-bindings-prob/at (rule-body r) full-idx delta-idx pos)])
                    w))))
    (define fresh
      (time-it! add-guard-build-time!
                (lambda ()
                  (for/sym-set ([w bindings])
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
;; Roulette cannot see into a BDD, so its `query`/`observe!` are bypassed
;; and the same semantics computed directly: conditioning conjoins
;; evidence, a marginal is P(fact and evidence) / P(evidence).

(define evidence (box (guard-true)))
(define (current-evidence) (unbox evidence))

;; Guards do not survive a reset of the manager, so neither can the
;; evidence built out of them.
(add-reset-hook! (lambda () (set-box! evidence (guard-true))))

;; Whether a guard can hold in some world satisfying the evidence.
;;
;; `guard-var` turns probability 0 and 1 into the constants, so every
;; variable has probability strictly between them, and every satisfying
;; assignment therefore carries positive weight. A guard has probability
;; zero exactly when it is the false diagram. That makes this structural
;; test *exact* where comparing a weighted count against zero is not: a
;; probability of 1 - 2^-60 rounds to 1.0 in a double, which would make
;; a possible outcome look impossible.
(define (satisfiable? g) (not (guard-known-false? g)))

;; A pmf over #t/#f for a guard, conditioned on the evidence so far, or
;; #f when no world satisfies the evidence at all -- matching what
;; Roulette's `query` returns in that case.
;;
;; An outcome is dropped only when it is impossible, which is what
;; leaves a certain answer as a single-outcome pmf for
;; `query-result->string` to print as #t or #f. An outcome that is
;; merely too unlikely for a double to distinguish from 0 is kept.
(define (guard->pmf g)
  (define ev (current-evidence))
  (define yes (guard-and g ev))
  (define no (guard-and (guard-not g) ev))
  (cond
    [(not (satisfiable? ev)) #f]
    [(not (satisfiable? no)) (for/pmf ([v (in-list '(#t))] [pr (in-list '(1))])
                               (values v pr))]
    [(not (satisfiable? yes)) (for/pmf ([v (in-list '(#f))] [pr (in-list '(1))])
                                (values v pr))]
    [else
     (define p (/ (guard-prob yes) (guard-prob ev)))
     (for/pmf ([value (in-list (list #t #f))]
               [prob (in-list (list p (- 1 p)))])
       (values value prob))]))

;; Condition all later queries on `g` holding.
(define (add-evidence! g)
  (set-box! evidence (guard-and (current-evidence) g)))

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
;; satisfies every observation made so far.
(define (possible? guard value)
  (define ev (current-evidence))
  (and (satisfiable? ev)
       (satisfiable? (guard-and (if value guard (guard-not guard)) ev))))

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

;; Condition on the fact being present (or absent); all later queries
;; see the posterior.
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