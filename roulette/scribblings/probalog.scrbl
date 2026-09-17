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
			      query-result->string
			      observe-fact
			      observe-not-fact
			      observe-guard
			      sym-set?
			      set
			      set-member?
			      set-add
			      set-remove
			      set-empty?
			      set-union
			      set-intersect
			      set-subtract
			      subset?
			      set-equal?
			      for/sym-set
			      for*/sym-set)
		     (only-in roulette/example/probalog/probalog-set-equal
			      run-datalog
			      saturate-semi)
		     (only-in roulette/example/probalog/guards
			      guard-true
			      guard-false
			      guard-var
			      guard-and
			      guard-or
			      guard-not
			      guard-implies
			      guard-iff
			      guard-equiv?
			      guard-prob
			      guard-known-true?
			      guard-known-false?
			      reset-guards!
			      add-reset-hook!
			      guard-stats))
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
each fact in the database is paired with a Boolean condition
over the base facts
describing exactly the worlds in which that fact is derivable.
That condition is a binary decision diagram,
built as the derivation proceeds rather than compiled at the end,
and a query is a weighted model count over it.

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
? Edge("a", "b").   % posterior: #t
END
]}

Observing @tt{Path("a", "c")} forces both edges to be present,
so the posterior probability of @tt{Edge("a", "b")} is 1.
A query with only one possible outcome prints as that outcome
rather than as a distribution over it.
Observations are implemented with @racket[observe!].
Observing something impossible --- a fact that no combination of base
facts can derive, or one ruled out by an earlier observation ---
is reported as an error against the statement that did it,
rather than silently leaving every later query with nothing to report.

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

Every one of these is reported at the location of the offending text,
so an editor can highlight it.
Parsing stops at the first error, however:
a program with several mistakes reveals them one at a time.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Editor support}

Probalog programs can be written in a @filepath{.rkt} file like any
other Racket module, but the language also claims the extension
@filepath{.pdl}, which is what an editor keys off of when it has no
other way to know what it is looking at.

@bold{DrRacket} needs nothing beyond the language itself.
It reads the coloring, indentation, and interaction behavior from
the @tt{#lang} line, so a Probalog file opened in DrRacket gets:

@itemlist[
@item{Syntax coloring in Probalog's own terms:
      predicate names as keywords,
      variables as symbols,
      quoted strings and probabilities as constants,
      and @tt{%} comments as comments.}
@item{Indentation that understands statements.
      A rule broken across lines aligns under its first body clause,
      and a line following a completed statement returns to the margin.}
@item{Check Syntax arrows for both variables and predicate names,
      so @onscreen{Rename} and @onscreen{Jump to Binding} work on either.
      A variable is bound by its first occurrence in its rule's body ---
      the one that actually ranges over the database ---
      and used by the others, including the ones in the head.
      A predicate is bound file-wide by the first statement that defines it,
      a fact declaration or a rule head,
      and used by every body clause, query, and observation that names it,
      so selecting one occurrence highlights the rest.}
@item{An interactions area that reads Probalog statements,
      submitting on a period rather than on a balanced parenthesis.
      Racket expressions are accepted there too:
      the saturated database is bound to @racket[probalog-result],
      and the whole engine interface is in scope.}
@item{Errors highlighted where they occur,
      rather than reported against the parser's own source.}]

@bold{VS Code} needs two separate things,
because nothing there reads a @tt{#lang} line the way DrRacket does.
Diagnostics, hover, and jump-to-binding come from
@hyperlink[LANGSERVER]{racket-langserver},
which runs the same Check Syntax pass documented above
and so reports the same information;
it works on @filepath{.rkt} files with no additional setup.
Syntax coloring comes from the extension in
@filepath{roulette/example/probalog/vscode},
which is installed by linking it into the extensions directory:

@verbatim|{
$ ln -s "$(pwd)/roulette/example/probalog/vscode" ~/.vscode/extensions/probalog
}|

and restarting VS Code.
It colors @filepath{.pdl} files.
A Probalog program saved as @filepath{.rkt} will still be colored as
Racket, since the Racket extension claims that extension and grammars
are selected by file type rather than by language line.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Engine}

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

@subsection{Saturation}
@defmodule[roulette/example/probalog/probalog-set-equal]

@defproc[(run-datalog [base-fact-probs (listof (cons/c fact? (real-in 0 1)))]
		      [rules (listof rule?)])
	 sym-set?]{
  Saturates the database,
  returning the symbolic set of all derivable facts.
  Each base fact is admitted under an independent @tech{guard}
  variable of the given probability,
  and the immediate-consequence operator is iterated to a fixpoint.
  Equivalent to @racket[(saturate-semi (make-base-set base-fact-probs) rules)].
}

@defproc[(saturate-semi [base sym-set?] [rules (listof rule?)]) sym-set?]{
  Iterates the immediate-consequence operator over @racket[base]
  until no fact's @tech{guard} changes.
  Takes the database rather than building it,
  so a caller that wants control over the variable order
  can construct it with @racket[make-base-set] itself.
}

@subsection{Facts, rules, and queries}
@defmodule[roulette/example/probalog/probalog-core]

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

@defproc[(query-fact [result sym-set?] [f fact?]
		     [#:where where (or/c string? #f) #f]) pmf?]{
  Returns the probability distribution of @racket[f]'s membership
  in @racket[result],
  conditioned on all observations made so far ---
  that is, @math{P(f ∧ evidence) / P(evidence)}.

  This is what @racket[query] computes for
  @racket[(set-member? result f)], but it is computed directly:
  a @tech{guard} is a diagram Rosette cannot see into, so the engine
  does its own conditioning rather than going through Roulette's
  @racket[query] and @racket[observe!].
  Where @racket[query] would return @racket[#f] because no possible
  world remains, this raises an error instead.

  Every one of these procedures takes an optional @racket[where],
  a source location like @tt{"reachability.pdl:5:0"}
  used to attribute a failure to the statement responsible.
  The language supplies it;
  code calling the engine directly has no reason to.
}

@defproc[(query-result->string [p pmf?]) string?]{
  How the language displays a query result:
  a distribution with a single possible outcome
  renders as that outcome, and anything else as the distribution.
  @racket[query-fact] itself always returns a @racket[pmf?];
  this only affects what a @tt{?} statement prints.

  So a printed @racket[#t] or @racket[#f] means the answer is
  @emph{provably} certain, not that its probability rounded to
  @racket[1] or @racket[0]. An outcome is dropped only when it is
  impossible, which @racket[query-fact] decides with
  @racket[guard-known-false?] rather than by weighing --- see there for
  why the two differ.
}

@defproc[(observe-fact [result sym-set?] [f fact?]
		       [#:where where (or/c string? #f) #f]) void?]{
  Conditions the current distribution on @racket[f] being derivable,
  so that all subsequent queries report posteriors.

  Observing something of probability zero would divide by zero
  and leave every later query reporting nothing,
  so it is rejected instead:
  @racket[f] must be derivable in at least one world
  that satisfies the observations already made.
}

@defproc[(observe-not-fact [result sym-set?] [f fact?]
			   [#:where where (or/c string? #f) #f]) void?]{
  Like @racket[observe-fact],
  but conditions on @racket[f] @emph{not} being derivable,
  and rejects the observation when @racket[f] is certain.
}

@defproc[(observe-guard [g any/c]
			[#:where where (or/c string? #f) #f]) void?]{
  Conditions on an arbitrary formula,
  such as a disjunction of several facts' membership tests,
  subject to the same check.
}

@defproc[(make-base-set [base-fact-probs (listof (cons/c fact? (real-in 0 1)))])
	 sym-set?]{
  Builds the initial database:
  each fact guarded by an independent variable of its probability.
  It is the only place a base fact's @tech{guard} is created,
  which makes it the one place that decides
  what order the variables come in.
  Called by @racket[run-datalog].
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Guards}
@defmodule[roulette/example/probalog/guards]

A @deftech{guard} is the condition under which something holds --- for a
@tech{symbolic set}, the condition under which an element is a member.
Guards are binary decision diagrams, built through this module on top of
the primitives @racketmodname[roulette/engine/rsdd] exports.

The representation is chosen for canonicality: two guards that mean the
same thing are the same object. Or-ing in a disjunct already implied
gives back the identical diagram, so a redundant derivation collapses
rather than accumulating; equivalence is a comparison rather than a
solver call; and a marginal is a weighted count over a diagram that
already exists. Building the diagram as the derivation proceeds, rather
than compiling an accumulated formula at the end, is the idea of
TP-compilation (Vlasselaer, Van den Broeck, Kimmig, Meert & De Raedt,
IJAR 2016).

A guard is not a Racket boolean and cannot be branched on. Ask
@racket[guard-known-true?] or @racket[guard-known-false?] instead, or
weigh it with @racket[guard-prob].

@deftogether[(@defproc[(guard-true) any/c]
	      @defproc[(guard-false) any/c])]{
  The guards that hold in every world and in none.
}

@defproc[(guard-var [p (real-in 0 1)]) any/c]{
  A fresh variable holding with probability @racket[p], independent of
  every other. A @racket[p] of @racket[0] or @racket[1] gives a constant
  rather than a variable, so a program with no @tt{::} annotations
  allocates none and every guard in it stays constant.

  This is the only way new randomness enters a guard, which is what
  makes @racket[make-base-set] the one place the variable order is
  decided.
}

@deftogether[(@defproc[(guard-and [a any/c] [b any/c]) any/c]
	      @defproc[(guard-or [a any/c] [b any/c]) any/c]
	      @defproc[(guard-not [a any/c]) any/c]
	      @defproc[(guard-implies [a any/c] [b any/c]) any/c]
	      @defproc[(guard-iff [a any/c] [b any/c]) any/c])]{
  The propositional connectives.
  @racket[guard-implies] is @racket[(guard-or (guard-not a) b)],
  and @racket[guard-iff] is that both ways round.
}

@defproc[(guard-equiv? [a any/c] [b any/c]) boolean?]{
  Whether two guards hold in exactly the same worlds.
  Canonicality makes this a comparison rather than a search,
  which is what the fixpoint loop is built on.
}

@deftogether[(@defproc[(guard-known-true? [g any/c]) boolean?]
	      @defproc[(guard-known-false? [g any/c]) boolean?])]{
  Whether a guard holds in every world, or in none.

  These are exact, and are the right way to ask whether an outcome is
  possible. Because @racket[guard-var] turns probability @racket[0] and
  @racket[1] into constants, every variable has probability strictly
  between them and every satisfying assignment carries positive weight
  --- so a guard has probability zero @emph{exactly} when
  @racket[guard-known-false?] holds of it. Comparing
  @racket[guard-prob] against zero is not equivalent: a probability of
  @racket[(- 1 (expt 2 -60))] rounds to @racket[1.0] in a flonum, which
  would make a possible outcome look impossible.
}

@defproc[(guard-prob [g any/c]) real?]{
  The unnormalised probability that @racket[g] holds.

  @bold{Sharp edge:} this memoises in each diagram node's scratch cell
  and never invalidates, so results are valid only while the weights are
  unchanged. That holds here --- a fact's probability never changes, and
  conditioning conjoins evidence rather than reweighting --- and it is
  what makes repeated queries over overlapping guards nearly free.
}

@defproc[(reset-guards!) void?]{
  Discards the diagram manager and starts over, which is how a fresh
  program gets a fresh variable space.

  This @emph{invalidates every existing guard}. A guard made before a
  reset names variables in a manager that is gone, and nothing
  afterwards can detect that: an operation on one may fault or may
  quietly return a wrong answer. Callers holding guards across a reset
  must drop them.
}

@defproc[(add-reset-hook! [thunk (-> any)]) void?]{
  Registers @racket[thunk] to run after each @racket[reset-guards!],
  once the new manager is in place so the thunk may build guards. This
  is how state holding guards drops what a reset invalidated; the
  engine uses it to clear accumulated evidence.
}

@defproc[(guard-stats) (listof (cons/c symbol? exact-nonnegative-integer?))]{
  Counters for tuning: guard operations performed, diagram apply calls,
  and variables allocated.
}

@section{Symbolic sets}
@defmodule[roulette/example/probalog/hash-set]

The natural representation of a Datalog database
whose facts exist only in some worlds
is a set whose membership is symbolic.
Rosette provides no such structure,
so Probalog supplies one:
a @deftech{symbolic set} maps each element
to its @tech{guard}: the condition
under which the element is a member.
Elements are concrete; only membership is uncertain.
All bindings of this module
are re-exported by @racketmodname[roulette/example/probalog/probalog-core].

@examples[#:eval evaluator #:hidden
  (require roulette/example/probalog/hash-set
	   roulette/example/probalog/guards)]
@examples[#:eval evaluator #:label #f
  (define x (guard-var 1/2))
  (set-add (set 1 2) 3 x)]

Elements present in every world print bare;
the rest print as @tt{[guard element]} pairs.
Here @racket[3] is present in half the worlds.
The set operations lift to guards:
a union disjoins the guards of a shared element,
an intersection conjoins them.

@examples[#:eval evaluator #:label #f
  (define y (guard-var 1/2))
  (guard-equiv? (set-member? (set-union (set-add (set) 1 x)
					(set-add (set) 1 y))
			     1)
		(guard-or x y))
  (guard-equiv? (set-member? (set-intersect (set-add (set) 1 x)
					    (set-add (set) 1 y))
			     1)
		(guard-and x y))]

@defproc[(sym-set? [v any/c]) boolean?]{
  Predicate for @tech{symbolic sets}.
  A symbolic set is also a sequence
  of two values, the element and its @tech{guard}.
}

@defform[(set elem ...)]{
  Constructs a @tech{symbolic set} containing each @racket[elem]
  in every world.
  Elements are concrete;
  to add one under a condition, pass a @tech{guard}
  to @racket[set-add].
}

@deftogether[(@defproc[(set-member? [st sym-set?] [v any/c]) any/c]
	      @defproc[(set-add [st sym-set?] [v any/c] [guard any/c (guard-true)]) sym-set?]
	      @defproc[(set-remove [st sym-set?] [v any/c]) sym-set?]
	      @defproc[(set-empty? [st sym-set?]) any/c]
	      @defproc[(set-union [st sym-set?] [more sym-set?] ...) sym-set?]
	      @defproc[(set-intersect [st sym-set?] [more sym-set?] ...) sym-set?]
	      @defproc[(set-subtract [st sym-set?] [more sym-set?] ...) sym-set?]
	      @defproc[(subset? [st1 sym-set?] [st2 sym-set?]) any/c])]{
  The usual set operations,
  lifted to @tech{symbolic sets}.
  Results are guards rather than Booleans:
  @racket[set-member?] returns the element's @tech{guard},
  and @racket[set-empty?] and @racket[subset?]
  the condition under which they hold.
  The optional @racket[guard] argument to @racket[set-add]
  adds @racket[v] only in the worlds where that guard holds;
  it is passed explicitly rather than written as
  @racket[(when guard (set-add st v))]
  because a guard is not a Racket Boolean
  and so cannot be branched on.
}

@defproc[(set-equal? [st1 sym-set?] [st2 sym-set?] [keys (or/c list? #f) #f])
	 boolean?]{
  Whether the two sets are @emph{semantically} equal:
  every element's guard in @racket[st1]
  is logically equivalent to its guard in @racket[st2].
  Because guards are canonical
  this is decided by comparison rather than by a solver.
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
  which is added in every world,
  or an element and a @tech{guard} as two values.

  Both accumulate into a transient mutable hash
  and convert to a @tech{symbolic set} only at the end.
  When the same element is produced by many iterations ---
  as when one rule derives the same conclusion along many paths ---
  this merges the duplicates in place,
  so the cost of building the element's guard
  is paid once per distinct element rather than once per iteration.
  The mutable hash never escapes,
  so the forms are observationally pure.
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

There is a wrinkle in transplanting that rule
onto a database whose facts carry guards.
Classically a derivation from facts that all existed before
was already performed, so it can be skipped;
here a fact can exist from round one
while its guard keeps weakening for many rounds.
So the delta selects only @emph{which} facts to match,
and every guard in a derivation
is read out of the accumulated database
rather than out of the delta.
Deriving from a delta @emph{of guards} instead
builds the condition ``newly derivable this round'',
which has to encode the absence of the shorter derivations
as well as the presence of a new one;
on a friends-and-smokers ring of 14
that cost 231 million diagram operations
where this operator costs under 180 thousand,
for the same result.

Fixpoint detection compares the guards of the new database
against the old with @racket[set-equal?],
restricted to the facts that actually changed in the last round.
Comparing canonical guards is a pointer comparison,
so a round's fixpoint test is proportional
to the size of the change rather than the size of the database.

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
@(define LANGSERVER "https://github.com/jeapostrophe/racket-langserver")
