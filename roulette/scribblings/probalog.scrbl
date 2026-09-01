#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@(require (for-label racket/base
		     (only-in roulette/example/disrupt
			      flip
			      query
			      observe!
			      pmf?)
		     (only-in roulette/example/probalog/probalog-core
			      fact
			      fact?
			      fact-name
			      fact-args
			      rule
			      rule?
			      rule-head
			      rule-body
			      make-base-set
			      query-fact
			      observe-fact
			      observe-not-fact
			      observe-guard
			      sym-set?
			      set
			      set-member?
			      set-add
			      set-remove
			      set-count
			      set-empty?
			      set-union
			      set-intersect
			      set-subtract
			      subset?
			      set-equal?
			      for/sym-set
			      for*/sym-set
			      for/sym-set/fast)
		     (only-in roulette/example/probalog/probalog-set-equal
			      run-datalog))
	  racket/sandbox
	  scribble/example)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; constants

@(define evaluator (make-base-eval #:lang 'roulette/example/disrupt))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Probalog}
@defmodule*[(roulette/example/probalog) #:lang]

Probalog is a probabilistic @hyperlink[DATALOG]{Datalog}:
a Datalog whose base facts may be annotated with probabilities.
Like an ordinary Datalog engine,
it derives every fact entailed by the base facts and the rules;
unlike an ordinary Datalog engine,
it also computes the probability of each derived fact
from the probabilities of the base facts it depends on.

@filebox["reachability.rkt"]{@verbatim[#<<END
#lang roulette/example/probalog

Edge("a", "b") :: 0.5.
Edge("b", "c") :: 0.6.

Path(x, y) :- Edge(x, y).
Path(x, z) :- Path(x, y), Edge(y, z).

? Path("a", "c").
END
]}

Running this program prints
@verbatim{Path("a", "c"): #<pmf: [#t 0.3] [#f 0.7]>}
since @tt{Path("a", "c")} is derivable exactly when both edges are present,
which happens with probability @racket[(* 0.5 0.6)].

Derived facts are not independent,
so their probabilities cannot be combined pairwise.
Probalog handles this by evaluating the whole program symbolically:
each fact in the database is paired with a Boolean formula
over the base facts' @racket[flip]s
describing exactly the worlds in which that fact is derivable.
Querying a fact hands that formula to Roulette's inference engine.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Syntax}

A Probalog program is a sequence of statements,
each terminated by a period.
Comments start with @tt{%} and run to the end of the line.
Predicate names must begin with an uppercase letter
and variables with a lowercase letter;
constants are double-quoted strings or numbers.

@tabular[#:sep @hspace[2]
	 #:row-properties '(bottom-border ())
  (list (list @bold{Statement} @bold{Meaning})
	(list @tt{Foo("a") :: 0.5.} "a base fact with probability 0.5")
	(list @tt{Foo("a").} "a base fact with probability 1")
	(list @tt{Foo(x) :- Bar(x), Baz(x).} "a rule")
	(list @tt{? Foo("a").} "query the probability of a fact")
	(list @tt{! Foo("a").} "observe that a fact is true")
	(list @tt{! ~Foo("a").} "observe that a fact is false"))]

Facts and rules may appear in any order,
and are collected before anything runs:
the database is fully saturated
before the first query or observation is evaluated.
Queries and observations, on the other hand,
run in source order relative to each other,
so a query before the first observation reports a prior
and a query after it reports a posterior
conditioned on every preceding observation.

@filebox["observation.rkt"]{@verbatim[#<<END
#lang roulette/example/probalog

Edge("a", "b") :: 0.5.
Edge("b", "c") :: 0.6.

Path(x, y) :- Edge(x, y).
Path(x, z) :- Path(x, y), Edge(y, z).

? Edge("a", "b").   % prior:     #<pmf: [#t 0.5] [#f 0.5]>
! Path("a", "c").
? Edge("a", "b").   % posterior: #<pmf: [#t 1.0]>
END
]}

Observing @tt{Path("a", "c")} forces both edges to be present,
so the posterior probability of @tt{Edge("a", "b")} is 1.
Observations are implemented with @racket[observe!],
so observing a fact whose prior probability is zero is undefined.

The parser rejects several classes of program statically:

@itemlist[
@item{A probability annotation must lie in @racket[(real-in 0 1)].}
@item{Facts, queries, and observations must be @emph{ground} ---
      every argument must be a constant.
      A ``fact'' containing a variable is really a universally quantified rule,
      which breaks the finiteness assumptions bottom-up evaluation relies on;
      a query containing a variable has no body to bind it against.}
@item{Every variable in a rule's head must appear somewhere in its body
      (range restriction), since otherwise the rule could never fire.}
@item{Every use of a predicate name across the file must agree on arity.
      A mismatch is almost always a typo,
      and would otherwise fail silently:
      a clause with the wrong arity simply never unifies,
      giving no hint as to why a rule never fires.}
@item{An uppercase identifier in argument position is rejected,
      since it is nearly always a constant missing its quotes.}]

@margin-note{
 The reader and parser for @racketmodname[roulette/example/probalog]
 were written as a convenience for testing programs,
 and are not robust:
 syntactically invalid programs may produce confusing errors
 rather than good diagnostics.
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Engine}
@defmodule[roulette/example/probalog/probalog-core]

The engine is also usable directly from
@racketmodname[roulette/example/disrupt],
without going through the surface syntax.
A program is a list of base facts paired with probabilities
and a list of rules.

@examples[#:eval evaluator #:hidden
  (require roulette/example/probalog/probalog-core
	   roulette/example/probalog/probalog-set-equal)]
@examples[#:eval evaluator #:label #f
  (define db
    (run-datalog
     (list (cons (fact 'Edge (list "a" "b")) 0.5)
	   (cons (fact 'Edge (list "b" "c")) 0.6))
     (list (rule (fact 'Path (list 'x 'y))
		 (list (fact 'Edge (list 'x 'y))))
	   (rule (fact 'Path (list 'x 'z))
		 (list (fact 'Path (list 'x 'y))
		       (fact 'Edge (list 'y 'z)))))))
  (query-fact db (fact 'Path (list "a" "c")))
  (observe-fact db (fact 'Path (list "a" "c")))
  (query-fact db (fact 'Edge (list "a" "b")))]

@defstruct*[fact ([name symbol?] [args list?])]{
  A predicate applied to arguments.
  An argument that is a symbol is a variable;
  any other argument is a constant.
  Facts appearing in a base-fact list or in a query must be ground.
}

@defstruct*[rule ([head fact?] [body (listof fact?)])]{
  A Horn clause:
  @racket[head] is derivable for any binding of its variables
  under which every clause of @racket[body] holds.
}

@defproc[(run-datalog [base-fact-probs (listof (cons/c fact? (real-in 0 1)))]
		      [rules (listof rule?)])
	 sym-set?]{
  Saturates the database,
  returning the symbolic set of all derivable facts.
  Each base fact is admitted under an independent @racket[flip]
  of the given probability,
  and the immediate-consequence operator is iterated to a fixpoint.
}

@defproc[(query-fact [result sym-set?] [f fact?]) pmf?]{
  Returns the probability distribution of @racket[f]'s membership
  in @racket[result],
  conditioned on all observations made so far.
  Equivalent to @racket[(query (set-member? result f))].
}

@defproc[(observe-fact [result sym-set?] [f fact?]) void?]{
  Conditions the current distribution on @racket[f] being derivable,
  so that all subsequent queries report posteriors.
  Observing a fact of zero prior probability is undefined.
}

@defproc[(observe-not-fact [result sym-set?] [f fact?]) void?]{
  Like @racket[observe-fact],
  but conditions on @racket[f] @emph{not} being derivable.
}

@defproc[(observe-guard [g boolean?]) void?]{
  Conditions on an arbitrary formula,
  such as a disjunction of several facts' membership tests.
}

@defproc[(make-base-set [base-fact-probs (listof (cons/c fact? (real-in 0 1)))])
	 sym-set?]{
  Builds the initial database:
  each fact guarded by an independent @racket[flip] of its probability.
  Called by @racket[run-datalog].
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Symbolic sets}
@defmodule[roulette/example/probalog/hash-set]

The natural representation of a Datalog database
whose facts exist only in some worlds
is a set whose membership is symbolic.
Rosette provides no such structure,
so Probalog supplies one:
a @deftech{symbolic set} maps each element
to the Boolean formula --- its @deftech{guard} ---
under which the element is a member.
All bindings of this module
are re-exported by @racketmodname[roulette/example/probalog/probalog-core].

@examples[#:eval evaluator #:hidden
  (require roulette/example/probalog/hash-set)]
@examples[#:eval evaluator #:label #f
  (define x (flip 1/2))
  (set 1 2 (if x 3 4))]

Elements with a concrete guard of @racket[#t] print bare;
the rest print as @tt{[guard element]} pairs.
Here @racket[3] and @racket[4] are each present in half the worlds.
The set operations lift to guards:
a union disjoins the guards of a shared element,
an intersection conjoins them.

@examples[#:eval evaluator #:label #f
  (define y (flip 1/2))
  (set-union (set-add (set) 1 x) (set-add (set) 1 y))
  (set-intersect (set-add (set) 1 x) (set-add (set) 1 y))]

@defproc[(sym-set? [v any/c]) boolean?]{
  Predicate for @tech{symbolic sets}.
  A symbolic set is also a sequence
  of two values, the element and its @tech{guard}.
}

@defform[(set elem ...)]{
  Constructs a @tech{symbolic set} containing each @racket[elem].
  An element that is itself a symbolic value
  is decomposed into the concrete values it can take,
  each guarded by the condition under which it takes that value.
}

@deftogether[(@defproc[(set-member? [st sym-set?] [v any/c]) boolean?]
	      @defproc[(set-add [st sym-set?] [v any/c] [guard boolean? #t]) sym-set?]
	      @defproc[(set-remove [st sym-set?] [v any/c]) sym-set?]
	      @defproc[(set-count [st sym-set?]) integer?]
	      @defproc[(set-empty? [st sym-set?]) boolean?]
	      @defproc[(set-union [st sym-set?] [more sym-set?] ...) sym-set?]
	      @defproc[(set-intersect [st sym-set?] [more sym-set?] ...) sym-set?]
	      @defproc[(set-subtract [st sym-set?] [more sym-set?] ...) sym-set?]
	      @defproc[(subset? [st1 sym-set?] [st2 sym-set?]) boolean?])]{
  The usual set operations,
  lifted to @tech{symbolic sets}.
  Results are symbolic:
  @racket[set-member?] returns the element's @tech{guard},
  @racket[set-count] a number that depends on which guards hold,
  and @racket[set-empty?] and @racket[subset?] formulas
  rather than concrete Booleans.
  The optional @racket[guard] argument to @racket[set-add]
  adds @racket[v] only in the worlds where @racket[guard] holds;
  it is passed explicitly rather than written as
  @racket[(when guard (set-add st v))]
  because a conditional whose branches disagree on whether a key exists
  cannot be expressed as a Rosette union over hashes.
}

@defproc[(set-equal? [st1 sym-set?] [st2 sym-set?] [keys (or/c list? #f) #f])
	 boolean?]{
  Whether the two sets are @emph{semantically} equal:
  every element's guard in @racket[st1]
  is logically equivalent to its guard in @racket[st2].
  Structurally different formulas may be equivalent,
  so this is decided with Rosette's solver rather than by comparison.
  When @racket[keys] is a list,
  only those elements are compared ---
  which is what makes fixpoint detection affordable,
  since elements untouched by the latest round
  are guaranteed to be unchanged.
}

@deftogether[(@defform[(for/sym-set (for-clause ...) body ...+)]
	      @defform[(for*/sym-set (for-clause ...) body ...+)])]{
  Iteration forms that accumulate a @tech{symbolic set}.
  Each iteration's @racket[body] may return either an element,
  or an element and a @tech{guard} as two values.
}

@defform[(for/sym-set/fast (for-clause ...) body ...+)]{
  Like @racket[for/sym-set],
  but accumulates into a transient mutable hash
  and converts to a @tech{symbolic set} only at the end.
  When the same element is produced by many iterations ---
  as when one rule derives the same conclusion along many paths ---
  this pays the cost of building the merged guard
  once per distinct element rather than once per iteration.
  The mutable hash never escapes,
  so the form is observationally pure.
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Evaluation strategy}

Saturation is bottom-up and semi-naive.
A naive round re-joins the entire database against every rule body,
rediscovering everything found in earlier rounds;
semi-naive evaluation instead tracks the facts
newly derived in the previous round
and computes only derivations that use one of them
in at least one clause position,
trying each position in turn.
Any derivation this skips
used only older facts,
and so was already found.

Fixpoint detection compares the guards of the new database
against the old with @racket[set-equal?],
restricted to the facts that actually changed in the last round.
Solver calls dominate the cost of a round,
and this makes their number proportional
to the size of the change rather than the size of the database ---
a ratio that shrinks as saturation approaches its fixpoint.

Matching is driven by an index.
Before each round the facts are grouped by predicate name,
and within a predicate by the value at each argument position.
When a clause is matched,
any argument whose value is already known ---
a constant, or a variable bound by an earlier clause of the same body ---
selects the facts that agree at that position,
rather than scanning every fact of the predicate.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; links

@(define DATALOG "https://en.wikipedia.org/wiki/Datalog")
