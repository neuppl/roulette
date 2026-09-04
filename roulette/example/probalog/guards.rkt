#lang rosette

;; The guard operations that `hash-set.rkt` builds sym-sets out of, behind
;; an interface with swappable implementations.
;;
;; A guard says under what conditions an element is present in a set. The
;; original implementation represents one as a Rosette boolean term, which
;; is what the `term` backend below preserves. The trouble with terms is
;; that they are not canonical: going around a recursive rule keeps adding
;; syntactically new disjuncts to a guard long after it has stopped meaning
;; anything new, so guards grow without bound, fixpoint detection needs a
;; solver call to see through the redundancy, and the whole accumulated
;; formula has to be compiled at query time. On a cyclic program that is
;; exponential -- 77 million BDD operations to produce a 560-node BDD, for
;; a friends-and-smokers ring of 14 people.
;;
;; The `bdd` backend represents a guard as a BDD instead. BDDs are
;; canonical, so a redundant disjunct collapses the moment it is added
;; rather than accumulating, fixpoint detection is pointer equality, and a
;; marginal is a weighted count over a structure that is already built.
;;
;; Not every sym-set operation can be served by BDDs: `set-count` and
;; friends build Rosette symbolic *values* out of guards (a distribution
;; over how many elements a set has, say), which a BDD handle cannot
;; represent. Those stay on the term backend; see `guard-term` below.

(provide (struct-out guard-backend)
         current-guard-backend
         term-backend make-bdd-backend
         use-term-guards! use-bdd-guards! bdd-guards?
         guard-true guard-false
         guard-var guard-and guard-or guard-not
         guard-implies guard-iff
         guard-equiv? guard-prob
         guard-known-true? guard-known-false?
         guard-term guard-from-rosette
         guard-backend-name
         guard-stats)

(require (only-in roulette/example/disrupt/core flip)
         roulette/engine/rsdd
         data/gvector
         ffi/unsafe/custodian)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The interface
;;
;; `var` takes a probability and returns a fresh independent guard that
;; holds with that probability. `equiv?` decides logical equivalence --
;; the operation the fixpoint loop is built on, and the one where the two
;; backends differ most: a solver call versus a pointer comparison.
;;
;; `prob` is the unnormalised probability that a guard holds, and is only
;; meaningful on a backend that can count models directly; the term
;; backend answers such questions through Roulette's `query` instead.
;;
;; `term` converts back to a Rosette boolean, for the operations that have
;; to hand a guard to Rosette. It is the identity on the term backend and
;; unavailable on BDDs.

(struct guard-backend
  (name true false var and or not implies iff equiv? prob term
   known-true? known-false? from-rosette stats)
  #:transparent)

(define current-guard-backend (make-parameter #f))

(define (backend) (current-guard-backend))

(define (guard-true)  (guard-backend-true  (backend)))
(define (guard-false) (guard-backend-false (backend)))

(define (guard-var p)        ((guard-backend-var (backend)) p))
(define (guard-and a b)      ((guard-backend-and (backend)) a b))
(define (guard-or a b)       ((guard-backend-or (backend)) a b))
(define (guard-not a)        ((guard-backend-not (backend)) a))
(define (guard-implies a b)  ((guard-backend-implies (backend)) a b))
(define (guard-iff a b)      ((guard-backend-iff (backend)) a b))
(define (guard-equiv? a b)   ((guard-backend-equiv? (backend)) a b))
(define (guard-prob a)       ((guard-backend-prob (backend)) a))
(define (guard-term a)       ((guard-backend-term (backend)) a))

;; Lift a Rosette boolean into the current representation. `flatten-symbolic`
;; reports the branch conditions it splits a symbolic value under as Rosette
;; booleans, so any guard derived from one has to come through here before
;; being combined with the rest.
(define (guard-from-rosette t) ((guard-backend-from-rosette (backend)) t))

;; Backend-specific counters, for tuning. An assoc list so each backend
;; can report whatever it has.
(define (guard-stats) ((guard-backend-stats (backend))))

;; Whether a guard is known, without a solver, to be the constant true or
;; false. Used as a fast path; a guard that is semantically constant but
;; not recognised as such simply misses it.
;;
;; These must be backend operations rather than a generic comparison
;; against `(guard-true)`. This module is `#lang rosette`, where `eq?` is
;; lifted: applying it to a symbolic guard yields a *symbolic* answer, and
;; a caller that branches on that forks its accumulator into a symbolic
;; union of two hashes. Every implementation here has to return a genuine
;; Racket boolean.
(define (guard-known-true? g)  ((guard-backend-known-true? (backend)) g))
(define (guard-known-false? g) ((guard-backend-known-false? (backend)) g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The term backend: guards are Rosette booleans
;;
;; This is the original behaviour, unchanged. `equiv?` keeps the concrete
;; fast path and falls back to asking the solver whether the two guards
;; can ever disagree.

(define (term-equiv? a b)
  (if (and (concrete? a) (concrete? b))
      (eq? a b)
      (unsat? (verify (assert (<=> a b))))))

(define (term-prob a)
  (error 'guard-prob
         (string-append
          "the term backend cannot weigh a guard directly\n"
          "  probabilities under this backend come from Roulette's `query`")))

;; `concrete?` first: it is a real Racket predicate, where the `eq?` that
;; follows would otherwise be lifted and answer symbolically.
(define (term-known-true? g)  (and (concrete? g) (eq? g #t)))
(define (term-known-false? g) (and (concrete? g) (eq? g #f)))

(define term-backend
  (guard-backend 'term
                 #t #f
                 flip
                 && || ! => <=>
                 term-equiv?
                 term-prob
                 values
                 term-known-true?
                 term-known-false?
                 values
                 (λ () '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The BDD backend: guards are BDDs
;;
;; Everything here is canonical. `or`-ing a disjunct that is already
;; implied returns the identical BDD, so a guard stops growing the moment
;; it stops meaning anything new; `equiv?` is then a pointer comparison
;; rather than a solver call; and `prob` is a weighted count over a
;; structure that was built incrementally rather than compiled in one go
;; at the end.
;;
;; State lives in the backend instance -- one BDD manager, one weight map
;; -- so a fresh instance is a fresh universe of variables.

(define (make-bdd-backend)
  (define b (mk-bdd-manager-default-order 0))
  (register-finalizer-and-custodian-shutdown b free-bdd-manager)
  ;; Indexed by variable label, holding (cons false-weight true-weight).
  ;; `rsdd-label` hands out labels sequentially from 0, so appending as
  ;; each variable is made keeps index and label in step.
  (define weights (make-gvector))
  (define cache (box '()))
  (register-finalizer-and-custodian-shutdown cache free-weight-cache)

  (define (bdd-var p)
    (cond
      ;; Matching `flip`: a certainty is a constant, not a variable. This
      ;; is what lets a program with no `::` annotations run without
      ;; allocating anything, and keeps guards concrete throughout.
      [(= p 0) (make-rsdd-false b)]
      [(= p 1) (make-rsdd-true b)]
      [else
       (define l (rsdd-label b))
       (gvector-add! weights (cons (- 1 p) p))
       (rsdd-var b l)]))

  ;; Every guard operation passes through here, so counting is exact.
  (define ops 0)
  (define (op!) (set! ops (add1 ops)))

  (define (bdd-term g)
    (error 'guard-term
           (string-append
            "the bdd backend cannot turn a guard back into a Rosette term\n"
            "  this operation needs the term backend; see guards.rkt")))

  ;; Only the two constants can cross over; anything genuinely symbolic
  ;; means a symbolic *element*, which this backend does not handle.
  (define (bdd-from-rosette t)
    (cond
      [(and (concrete? t) (eq? t #t)) (make-rsdd-true b)]
      [(and (concrete? t) (eq? t #f)) (make-rsdd-false b)]
      [else
       (error 'guard-from-rosette
              (string-append
               "the bdd backend cannot represent a symbolic set element\n"
               "  probalog facts are always ground, so this should not arise"))]))

  (guard-backend 'bdd
                 (make-rsdd-true b) (make-rsdd-false b)
                 bdd-var
                 (λ (x y) (op!) (rsdd-and b x y))
                 (λ (x y) (op!) (rsdd-or b x y))
                 (λ (x) (op!) (rsdd-not b x))
                 ;; x => y  is  (not x) or y;  x <=> y  is  both ways
                 (λ (x y) (rsdd-or b (rsdd-not b x) y))
                 (λ (x y) (rsdd-and b (rsdd-or b (rsdd-not b x) y)
                                      (rsdd-or b (rsdd-not b y) x)))
                 (λ (x y) (op!) (rsdd-equal? b x y))
                 (λ (g) (wmc g weights cache real-semiring))
                 bdd-term
                 rsdd-true?
                 rsdd-false?
                 bdd-from-rosette
                 (λ () (list (cons 'guard-ops ops)
                             (cons 'apply-calls (rsdd-num-recursive-calls b))
                             (cons 'variables (gvector-count weights))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selecting a backend

(define (use-term-guards!) (current-guard-backend term-backend))
(define (use-bdd-guards!)  (current-guard-backend (make-bdd-backend)))
(define (bdd-guards?) (eq? (guard-backend-name (current-guard-backend)) 'bdd))

;; The term backend is the default. `PROBALOG_GUARDS=bdd` selects the
;; other one for a whole process, which is how the test suite and the
;; benchmarks run the same programs both ways without every program
;; having to say so itself.
(if (equal? (getenv "PROBALOG_GUARDS") "bdd")
    (use-bdd-guards!)
    (use-term-guards!))
