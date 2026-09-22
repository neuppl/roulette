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

Originally, the guards were represented as Rosette terms (formulas), but for the following reasons, it has been changed to BDDs (Binary Decision Diagrams), following the idea of [Tp compilation](https://www.sciencedirect.com/science/article/pii/S0888613X16300949), for the following reasons:

1. Fixpoint detection becomes pointer equality over BDDs: Checking equality of guards is constant time with BDDs, whereas with Rosette terms, fixpoint detection was the dominant cost of running a probalog program.

2. Querying is fast: Since the BDDs are already compiled, querying for probability only requires WMC (weighted model counting), as opposed to compiling the bdd on-demand for every query.

3. Canonical representation: There are programs that repeatedly disjoin redundant formulas onto guards, which can make the guards very large, making the program very slow. BDDs collapse redundant disjunctions to a canonical form, keeping guards small.

### How sym-sets work

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

Fixpoint detection then uses the semantic equality of these sym-sets: two sets are equal when every element's guard in one is _logically equivalent_ to its guard in the other, not merely syntactically identical.

### Variable ordering

Because a BDD has a total variable order, and `make-base-set` is the single place a base fact's guard is created, the order in which base facts are visited _is_ the variable order. It matters: a diagram has to keep alive every decision still relevant below the current level, so the cost depends on how far apart related variables sit.

Rule bodies join on shared variables, which bind to shared constants, so two facts naming the same constant are exactly the ones that can meet in a derivation — adjacent is where they want to be. `order-base-facts` therefore visits constants breadth-first, the Cuthill-McKee family of linear-layout heuristics. It looks only at which facts share a constant and never at what a constant _is_, so renaming the data does not change the result.

Note the target is pathwidth rather than treewidth: a total variable order is a path decomposition. Elimination orders such as min-fill and min-degree minimise treewidth instead, and measured 2-3x worse here; they would be the right choice for a vtree-structured representation like an SDD, not for this one.

## Performance optimizations

So far, the following optimizations are being used to improve performance.

### Semi-naive evaluation, over accumulated guards

In a naive implementation of the immediate consequence operator, we match the entire existing factset against the rule bodies to derive new facts on every round. This is inefficient since we keep rederiving pre-existing facts unnecessarily.

Instead, semi-naive evaluation only considers matching rule bodies when atleast 1 of the clauses is matched against a fact in the delta from the previous round. We track the delta as a list of _new_ facts derived on every application of immediate consequence. This is also a standard optimization in classic datalog, but there is a subtlety for the probabilistic implementation that significantly affects performance.

When we apply immediate-consequence, we (potentially) not only get entirely new facts, but also updated guards (or probabilities) associated with existing facts (ie. new _unique_ derivations of the same fact). This is tracked in the delta, but the new guards themselved are not actually included in the delta, as doing so is inefficient.

For example, consider a fact set F, with one of the facts `f_0` with guard `g_0`. After an application of immediate-consequence, assume we find a way to derive `f_0` under a new guard `g_1`.

The delta then, could track the difference for this fact as `f_0` **with** the new guard `g_1 ∧ ¬g_0`, and subsequent rounds would only derive new facts using the facts **and** guards from the delta.

However, this turns out to be less performant than just using the accumulated `g_0 ∨ g_1` guard for future BDD operations, since we don't need to additionally compile to BDDs **only** the derived guard, particularly when the guards overlap.

### Incremental fixpoint detection

The delta doubles as the fixpoint test: nothing changed means nothing left to do. We record during unification which facts are newly derived, and, since they are canonical BDDs, comparing two guards is just pointer comparison to see if they changed _logically_.

### Predicate indexing

Facts are indexed by predicate name before each round of matching, so that a clause like `Edge(x, y)` only scans facts actually named `Edge`, rather than every fact in the factset. This reduces wasted fact-matching attempts that are obviously incorrect since they are different predicate names.

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

A query whose result is certain prints as `#t` or `#f` rather than as a one-outcome distribution. That means _provably_ certain: whether an outcome is possible is decided by asking whether its guard is the false diagram, not by comparing a probability against zero. The two are not the same — a probability of `1 - 2^-60` rounds to `1.0` in a float, and deciding it that way would report a possible outcome as impossible, and reject a legal observation of its absence. Since `guard-var` maps probability 0 and 1 to constants, every variable sits strictly between them, so a guard has probability zero exactly when it is structurally false; the structural test is the exact one.

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
