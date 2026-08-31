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

After all facts (along with their probabilities) have been derived, you can query the presence of facts. For example, querying `Path("a", "c")` gives us the probability distribution ` #<pmf: [#t 0.3] [#f 0.7]>` since `Path("a", "c")` only exists when both `Path("a", "b")` and `Path("b", "c")` exist, which has a probability of `0.6*0.5 = 0.3`

## Factset representation and Fixpoint detection

An ideal datastructure to represent the factset in this setting is a symbolic set. However, Rosette (the symbolic evalutation engine Roulette uses) doesn't have support for symbolic hashes/sets. So, we created our own implementation in `hash-set.rkt` that works by associating each set element with a symbolic guard under which that element is present. For example, the program

```
(define-symbolic x boolean?)
(set 1 2 (if x 3 4))
```

produces the set
`(sym-set
   [#t 1]
   [#t 2]
   [x 3]
   [(! x) 4])`
or more simply
`(sym-set
   1
   2
   [x 3]
   [(! x) 4])`, where the elements 3 and 4 exist under the guards `x` and `(! x)`.

The utility comes from standard set operations being re-implemented to work with sym-sets. For example, set-union computes the union of 2 sym-sets by using the disjuntion of the guards of overlapping keys in both sets. For example, unioning `(sym-set [x 1])` with `(sym-set [y 1])` produces `(sym-set [(|| x y) 1])` — element `1` is present whenever either set said it was present.

Fixpoint detection then uses the semantic equality of these sym-sets. This is accomplished by comparing the logical equivalence of symbolic guard formulas using a SAT solver (Z3) which Rosette provides access to. This is necessary since structurally different symbolic formulas may be logically equivalent. See more below about the "Incremental fixpoint detection" optimization

## Performance optimizations

So far, the following optimizations are being used to improve performance.

### Semi-naive evaluation

Naively, each round re-joins the entire factset against every rule body, including facts that were already fully processed in earlier rounds — any derivation using only already-known facts in every clause position was necessarily already found in a previous round. Semi-naive evaluation instead tracks a `delta` (the facts newly derived in the previous round) and only computes derivations that use `delta` in at least one clause position, trying each clause position in turn. Anything missed by this restriction was already found previously, so it's a pure performance win with no effect on the final result.

### Incremental fixpoint detection

Checking fixpoint equality with a SAT solver is expensive, and re-checking every fact in the factset each round is wasteful: any fact untouched by the latest `delta` is guaranteed to have an identical guard before and after the round, since it was never unioned with anything new. Fixpoint detection is therefore restricted to only the facts that changed in the most recent round, cutting the number of solver calls roughly in proportion to how small `delta` is relative to the whole factset — which shrinks as saturation progresses.

### Predicate indexing

Facts are indexed by predicate name before each round of matching, so that a clause like `Edge(x, y)` only scans facts actually named `Edge`, rather than every fact in the factset. Facts of different predicates could never unify with a given clause anyway, so this purely removes wasted match attempts.

### Value-indexing

Building on predicate indexing, facts are further indexed per predicate by the value at each argument position. When matching a clause, if any of its arguments is already known — either a literal constant, or a variable already bound by an earlier clause in the same rule body — that known value is used to look up only the facts that could possibly match at that position, instead of scanning every fact of the predicate.

## Examples and #lang roulette/example/probalog

Probalog is implemented as an example language inside Roulette, alongside `roulette/example/disrupt`.

To install this branch of roulette (that has probalog) locally, clone this repository on the `visualizations` branch and, from the root, run `raco pkg install --auto roulette/ roulette-lib/` or `./update.sh` to install roulette locally. You should have racket installed already.

Probalog can then be run by using the hashlang declaration at the top of rkt files:

```
#lang roulette/example/probalog
```

Some example programs, including the graph-reachability example in this file and some others can be found at [github.com/Smaran-Teja/probalog](https://github.com/Smaran-Teja/probalog).

Note: the hashlang is entirely AI-generated and not tested robustly. It was mostly made as a useful tool to test programs, not for actual use.
