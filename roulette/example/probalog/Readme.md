# Probalog: A Probabilistic Datalog implementation built with Roulette

This is an implementation of an engine that runs Datalog programs where the facts can be annotated with probabilities.

An example of such a program is the following:

```
Edge("a", "b") :: 0.5.
Edge("b", "c") :: 0.6.

Path(x, y) :- Edge(x, y).
Path(x, z) :- Path(x, y), Edge(y, z).
```

In the above program, we have the facts `Edge("a", "b")` and `Edge("b", "c")` in our initial fact set. These facts have probabilities 0.5 and 0.6.

Similar to a standard Datalog engine, the goal is to find all derivable facts from the initial factset. However, we must also calculate the probabilities of the derived facts using the probabilities in the initial fact set.

After all facts (along with their probabilities) have been derived, you can query the presence of facts. For example, querying `Path("a", "c")` gives us the probability distribution `#<pmf: [#t 0.3] [#f 0.7]>` since `Path("a", "c")` only exists when both `Path("a", "b")` and `Path("b", "c")` exist, which has a probability of `0.6*0.5 = 0.3`

## Factset representation and fixpoint detection

An ideal datastructure to represent the factset in this setting is a symbolic set. However, Rosette (the symbolic evaluation engine Roulette uses) doesn't have support for symbolic hashes/sets. So, we created our own implementation in `hash-set.rkt` that works by associating each set element with a **guard**: the condition under which that element is present. Elements themselves are concrete — the parser requires facts and queries to be ground, so it is only membership that is uncertain.

```
(define x (guard-var 1/2))
(set-add (set 1 2) 3 x)
```

produces

```
(sym-set
  1
  2
  [#<cpointer> 3])
```

where `1` and `2` are present in every world and `3` is present under the guard `x`. Elements present everywhere print bare; the rest print as `[guard element]` pairs. The guard prints as a pointer because it is not a Racket value — see below.

The utility comes from standard set operations being re-implemented to work with sym-sets. For example, set-union computes the union of 2 sym-sets by using the disjunction of the guards of overlapping keys in both sets: unioning `(sym-set [x 1])` with `(sym-set [y 1])` gives a set whose guard for `1` is `x || y` — element `1` is present whenever either set said it was present.

Fixpoint detection then uses the semantic equality of these sym-sets: two sets are equal when every element's guard in one is *logically equivalent* to its guard in the other, not merely syntactically identical.

## Guards are BDDs, compiled as they are built

A guard is a binary decision diagram, built through `guards.rkt` on top of the BDD primitives that `roulette-lib/engine/rsdd.rkt` exports. The representation is what makes the three operations saturation leans on cheap, and all three for the same reason — BDDs are canonical, so two guards that mean the same thing *are* the same object:

| operation | cost |
| ----------- | ------ |
| building | or-ing in a disjunct that is already implied hands back the identical diagram, so a redundant derivation collapses on contact instead of accumulating |
| comparing | deciding whether two guards are equivalent is a pointer comparison, not a solver call |
| weighing | a marginal is a weighted model count over a diagram that already exists, rather than a compilation of an accumulated formula |

This is the same idea as TP-compilation (Vlasselaer, Van den Broeck, Kimmig, Meert & De Raedt, IJAR 2016), which interleaves knowledge compilation with forward reasoning for exactly this reason.

An earlier version of this engine represented guards as Rosette boolean terms instead, and compiled them only at query time. Terms are not canonical, so going around a recursive rule kept adding syntactically new disjuncts long after the guard had stopped meaning anything new: guards grew without bound, and fixpoint detection needed a Z3 call per changed fact per round to see through the redundancy. On a friends-and-smokers ring of 14 that came to 77 million BDD operations at query time to produce a 560-node BDD. That representation is gone, and `guards.rkt` no longer depends on Rosette at all.

### Variable ordering

Because a BDD has a total variable order, and `make-base-set` is the single place a base fact's guard is created, the order in which base facts are visited *is* the variable order. It matters: a diagram has to keep alive every decision still relevant below the current level, so the cost depends on how far apart related variables sit.

Rule bodies join on shared variables, which bind to shared constants, so two facts naming the same constant are exactly the ones that can meet in a derivation — adjacent is where they want to be. `order-base-facts` therefore visits constants breadth-first, the Cuthill-McKee family of linear-layout heuristics. It looks only at which facts share a constant and never at what a constant *is*, so renaming the data does not change the result.

Note the target is pathwidth rather than treewidth: a total variable order is a path decomposition. Elimination orders such as min-fill and min-degree minimise treewidth instead, and measured 2-3x worse here; they would be the right choice for a vtree-structured representation like an SDD, not for this one.

## Performance optimizations

So far, the following optimizations are being used to improve performance.

### Semi-naive evaluation, over accumulated guards

Naively, each round re-joins the entire factset against every rule body, including facts that were already fully processed in earlier rounds. Semi-naive evaluation instead tracks a `delta` and only computes derivations that use it in at least one clause position, trying each clause position in turn.

There is a wrinkle in transplanting that rule from set-valued Datalog onto guard-valued Datalog, and getting it wrong is expensive. Classically, a derivation from facts that all existed before was already performed, so it can be skipped. Here a fact can exist from round one while its guard keeps *weakening* for many rounds. So the condition to test is whether a guard changed, not whether a fact appeared.

The delta is therefore a plain list of the keys whose guard changed, and it selects only *which* facts to match — every guard entering a derivation is read back out of the accumulated factset. Deriving from a delta *of guards* instead builds the condition "newly derivable this round", which has to encode the absence of the shorter derivations as well as the presence of a new one. That is a strictly more complicated condition than "derivable", and with canonical guards the intermediate diagrams dwarf the answer: on a friends-and-smokers ring of 14 it cost 231 million BDD operations where the operator described here costs under 180 thousand, for the same result.

### Incremental fixpoint detection

The delta doubles as the fixpoint test: nothing changed means nothing left to do. Comparing two canonical guards is a pointer comparison, and only the facts touched in the last round need comparing at all, so a round's fixpoint test costs in proportion to the size of the change rather than the size of the factset — a ratio that shrinks as saturation approaches its fixpoint.

### Predicate indexing

Facts are indexed by predicate name before each round of matching, so that a clause like `Edge(x, y)` only scans facts actually named `Edge`, rather than every fact in the factset. Facts of different predicates could never unify with a given clause anyway, so this purely removes wasted match attempts.

### Value-indexing

Building on predicate indexing, facts are further indexed per predicate by the value at each argument position. When matching a clause, if any of its arguments is already known — either a literal constant, or a variable already bound by an earlier clause in the same rule body — that known value is used to look up only the facts that could possibly match at that position, instead of scanning every fact of the predicate.

### Transient mutable hashes in sym-set iteration

Applying a rule derives the same fact many different ways — on a densely connected graph, many paths converge on the same `Path(x, y)` conclusion. Inserting each derivation into a sym-set one at a time is expensive: every insertion copies part of the underlying immutable hash and re-runs the guard merging machinery. `for/sym-set` and `for*/sym-set` instead accumulate into a _transient_ mutable hash, merging duplicate derivations in place, and convert to a real sym-set once at the end — so the expensive guarded insertion is paid once per distinct element rather than once per derivation. This is safe because the mutable hash never escapes the macro that creates it, so callers still see purely functional behaviour.

## Bayesian observations

After the factset has been saturated, you can condition the probability distribution on observed evidence. This updates all subsequent queries to reflect the posterior `P(fact | evidence)` rather than the prior `P(fact)`.

A guard is opaque to Rosette, so rather than going through Roulette's `query` and `observe!` the engine computes the same semantics directly: conditioning conjoins the evidence onto an accumulated evidence guard, and a marginal is `P(fact and evidence) / P(evidence)` — which is what Roulette's `query` computes too, by normalising over `(if evidence e ⊥)`. An observation that no world satisfies is reported as an error rather than dividing by zero.

In `#lang roulette/example/probalog`, observations use the `!` prefix:

```
! Path("a", "c").    % observe that Path("a","c") is definitely true
! ~Path("a", "c").   % observe that Path("a","c") is definitely false
```

A query whose result is certain prints as `#t` or `#f` rather than as a one-outcome distribution. That means *provably* certain: whether an outcome is possible is decided by asking whether its guard is the false diagram, not by comparing a probability against zero. The two are not the same — a probability of `1 - 2^-60` rounds to `1.0` in a float, and deciding it that way would report a possible outcome as impossible, and reject a legal observation of its absence. Since `guard-var` maps probability 0 and 1 to constants, every variable sits strictly between them, so a guard has probability zero exactly when it is structurally false; the structural test is the exact one.

The database is saturated once, and then every `?` and `!` statement runs in source order. So a query before an observation reports a prior, and the same query after it reports a posterior conditioned on every observation preceding it.

## Examples and #lang roulette/example/probalog

Probalog is implemented as an example language inside Roulette, alongside `roulette/example/disrupt`.

To install this branch of roulette (that has probalog) locally, clone this repository on the branch carrying probalog and, from the root, run `raco pkg install --auto roulette/ roulette-lib/` or `./update.sh` to install roulette locally. You should have racket installed already.

Probalog can then be run by using the hashlang declaration at the top of rkt files:

```
#lang roulette/example/probalog
```

Some example programs, including the graph-reachability example in this file and some others can be found at [github.com/Smaran-Teja/probalog](https://github.com/Smaran-Teja/probalog).

The language also claims the file extension `.pdl`, which is what editors key off of when they can't read the `#lang` line. A `.rkt` file works exactly the same way when run.

Note: parsing stops at the first error, so a program with several mistakes reveals them one at a time.

## Editor support

**DrRacket** needs no setup — it reads everything from the `#lang` line:

- syntax coloring in Probalog's own terms (predicates as keywords, variables as symbols, strings and probabilities as constants, `%` comments as comments)
- indentation that understands statements: a rule broken across lines aligns under its first body clause, and a line after a completed statement returns to the margin
- Check Syntax arrows between a rule's variables. A variable is bound by its first occurrence in the body — the one that actually ranges over the database — and used by the rest, including the ones in the head, so Rename and Jump to Binding work
- an interactions area that reads statements, submitting on a period rather than on a balanced parenthesis. Racket expressions work there too: the saturated database is bound to `probalog-result` and the whole engine interface is in scope
- parse errors highlighted where they occur

**VS Code** needs two separate pieces, since nothing there reads a `#lang` line. Diagnostics, hover, and jump-to-binding come from [racket-langserver](https://github.com/jeapostrophe/racket-langserver), which runs the same Check Syntax pass and works on `.rkt` files with no setup. Coloring comes from the extension in [`vscode/`](vscode/), installed by linking it in:

```
ln -s "$(pwd)/vscode" ~/.vscode/extensions/probalog
```

See [`vscode/README.md`](vscode/README.md) for details, including why a `.rkt` file still gets Racket's coloring there.

### How it's put together

| file                                               | role                                                                                                                                 |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| `lang/reader.rkt`                                  | the `#lang` entry point; answers the editor info keys below                                                                          |
| `lang/lang-info.rkt`, `lang/configure-runtime.rkt` | installs the interactive reader for the REPL                                                                                         |
| `lexer.rkt`                                        | the scanner, shared by the parser, the colorer, the submit predicate, and the indenter, so they can't disagree about what a token is |
| `parser.rkt`                                       | recursive descent; attaches source locations and emits the Check Syntax binding scaffold                                             |
| `expander.rkt`                                     | `#%module-begin`, plus macros that make the marker forms work at the REPL                                                            |
| `tool/syntax-color.rkt`                            | `color-lexer`                                                                                                                        |
| `tool/submit.rkt`                                  | `drracket:submit-predicate`                                                                                                          |
| `tool/indentation.rkt`                             | `drracket:indentation`                                                                                                               |
| `vscode/`                                          | TextMate grammar for VS Code                                                                                                         |

The scaffold is worth a note: Datalog variables live inside quoted data at runtime, where Check Syntax can't see them as identifiers. So each rule also emits a `(when #f (lambda (x ...) (void y ...)))` carrying the real source locations of the variable occurrences — dead code that never runs but gives the variables genuine binding structure for the IDE to annotate.
