# BLOG lists, matrices, parsing, and random values

Worked examples of list and matrix construction, indexing, arithmetic, and
random values. The language reference is maintained in
[the BLOG manual source](../scribblings/blog.scrbl).

Runtime indexing supports indexed distinct-object families, lists, and matrices.
`transpose`, `toReal`, and `toInt` are implemented, along with the arithmetic
and matrix library described in section 10.

BLOG statements end with a semicolon. Complete BLOG files start with
`#lang roulette/example/blog`; most examples below are statement fragments.
Racket examples illustrate runtime helpers or macro expansions, rather than
BLOG source syntax.

The language behavior being targeted comes from
[the BLOG reference, §5.4 and Appendix B](https://bayesianlogic.github.io/download/blog-langref.pdf).

## 1. Lists and matrices are different values

```text
[10, 20, 30]
```

This expression constructs a BLOG list. Its bracket notation alone does not
make it a matrix. Internally, the value uses the `blog-list-value` struct.

```text
fixed RealMatrix v = [10, 20, 30];
```

Here, the `RealMatrix` return annotation converts the list into a matrix when
the declaration evaluates. The result is a 1×3 row matrix, represented by
`blog-matrix-value`. Nested lists similarly remain lists until converted.

The internal predicates distinguish these representations:

```racket
(blog-list-value? value)   ; Is value a BLOG list?
(blog-matrix-value? value) ; Is value a matrix?
```

The shape of the data does not determine its type. A list of numeric lists can
look like a matrix while still being a list value.

## 2. Equivalent spellings of nested lists

```text
[[1, 2, 3], [4, 5, 6]]
[1, 2, 3; 4, 5, 6]
```

Both spellings parse into the same internal expression:

```racket
(blog-list
 (blog-list 1 2 3)
 (blog-list 4 5 6))
```

This is what “normalize to equivalent list expressions” means: the parser
removes the spelling difference. A semicolon inside brackets starts another
row; the runtime receives the same nested list either way.

```text
[]
[1, 2, 3]
[[], [4]]
[1; 2; 3]
```

These construct, respectively, an empty list, a flat list, a nested list with
an empty first element, and three single-element rows. Semicolon shorthand
requires at least two nonempty rows; write empty rows explicitly as `[]`.

```text
fixed RealMatrix a = [1, 2; 3, 4];
```

Parsing this declaration produces:

```racket
(fixed RealMatrix a ()
  (blog-list
   (blog-list 1 2)
   (blog-list 3 4)))
```

The empty `()` means the declaration has no parameters. Parsing does not call
`make-blog-matrix`; evaluation of the `fixed` declaration now performs that
conversion through its result wrapper.

## 3. List indexing

Each of these expressions now parses and evaluates.

```text
[10, 20, 30][1]             // 20
[[10, 20], [30, 40]][1]     // [30, 40]
[[10, 20], [30, 40]][1][0]  // 30
```

Indices start at zero. A list index returns the selected stored element. If
that element is another list, another index selects within that list. An
out-of-bounds index raises an error.

## 4. Matrix indexing

Matrix indexing returns a matrix; conversion functions extract its scalar.

```text
fixed RealMatrix m = [10, 20, 30; 40, 50, 60];

m[1]               // row matrix [40, 50, 60], shape 1×3
m[1][0]            // singleton matrix [40], shape 1×1
toReal(m[1][0])     // scalar 40.0
toInt(m[1][0])      // scalar integer 40
```

Indexing a two-dimensional matrix selects a row. Indexing that row selects a
singleton matrix. The result retains the matrix type; an explicit conversion
extracts its scalar value.

```text
fixed RealMatrix row = [10, 20, 30];
fixed RealMatrix col = [10; 20; 30];

row[1]             // singleton matrix [20], shape 1×1
col[1]             // singleton matrix [20], shape 1×1
```

A row or column vector is a matrix too. Indexing either vector selects one
element as a 1×1 matrix, rather than returning a scalar.

```text
transpose(m)[0]    // first column of m, represented as a row
(a + b)[i][j]      // select a singleton from the sum of two matrices
```

The parser accepts indexing a function result or a parenthesized expression.
Both examples now evaluate. Matrix addition requires matching dimensions.

This is the end-to-end example from the original plan:

```text
fixed RealMatrix a = [1, 2; 3, 4];
random Integer i ~ UniformInt(0, 1);
random Real x = toReal(a[i][1]);
query x;
```

This example now works: `x` is 2.0 or 4.0 with probability 1/2 each. The random
row index selects either the second entry of the first row or of the second row.

## 5. What `make-blog-matrix` currently does

These examples use internal Racket helpers. The test-support submodule exposes
the constructor helpers for tests; they are not all part of the public BLOG
language API. Comments show the resulting struct fields as constructor notation:
`(blog-matrix-value rows columns row-major-elements)`.

### A flat list becomes a row vector

```racket
(make-blog-matrix (blog-list 10 20 30))
;; → (blog-matrix-value 1 3 '(10 20 30))
```

One flat list supplies one row. The dimensions are stored explicitly, along
with an immutable list of cells.

### Nested lists become rows

```racket
(make-blog-matrix
 (blog-list (blog-list 10 20)
            (blog-list 30 40)))
;; → (blog-matrix-value 2 2 '(10 20 30 40))
```

The constructor checks that the row lengths agree and flattens them in row
order: all cells from the first row, then all cells from the second row.

### Existing matrices pass through unchanged

```racket
(define m (make-blog-matrix (blog-list 10 20)))
(eq? (make-blog-matrix m) m)
;; → #t
```

Converting a matrix again returns the same value. This lets a `RealMatrix`
function return a matrix that another declaration already constructed.

### Empty inputs retain their shape

```racket
(make-blog-matrix (blog-list))
;; → (blog-matrix-value 0 0 '())

(make-blog-matrix (blog-list (blog-list) (blog-list)))
;; → (blog-matrix-value 2 0 '())
```

An empty outer list has no rows or columns. Two explicit empty rows produce a
2×0 matrix. Both have empty cell storage, but different dimensions.

### Unequal row lengths are rejected

```racket
(make-blog-matrix
 (blog-list (blog-list 1 2)
            (blog-list 3)))
;; error: matrix rows must have equal lengths
```

Lists can be ragged, but conversion to a matrix requires a rectangular shape.

### Nonnumeric cells are rejected

```racket
(make-blog-matrix (blog-list 1 "two"))
;; error: expected real numeric matrix element
```

Matrix cells must satisfy `real?`. Strings, Booleans, BLOG null, and non-real
complex numbers are rejected too:

```racket
(make-blog-matrix (blog-list #t))
(make-blog-matrix (blog-list blog-null))
(make-blog-matrix (blog-list 1+2i))
;; Each call raises an argument error.
```

### Mixing scalars and rows is rejected

```racket
(make-blog-matrix (blog-list 1 (blog-list 2 3)))
;; error: a BLOG list cannot be a numeric cell
```

The input must be either a flat numeric list or a list of numeric rows. It
cannot mix those two structures at the outer level.

### Deeper nesting is rejected

```racket
(make-blog-matrix (blog-list (blog-list (blog-list 1))))
;; error: a BLOG list cannot be a numeric cell
```

The representation supports vectors and two-dimensional matrices, not
three-dimensional arrays.

### Ordinary Racket lists and standalone scalars are rejected

```racket
(make-blog-matrix '(1 2))
;; error: expected BLOG list or numeric matrix

(make-blog-matrix 42)
;; error: expected BLOG list or numeric matrix
```

`blog-list` constructs a tagged BLOG list. A plain Racket list is a different
input type, even when it contains the same numbers.

### Exact numbers remain exact

```racket
(make-blog-matrix (blog-list 1/3 2))
;; → (blog-matrix-value 1 2 '(1/3 2))
```

The constructor preserves the exact rational `1/3` and integer `2`. It does not
convert every entry to a floating-point number. An existing inexact entry,
such as `2.5`, remains inexact.

### Symbolic cells retain correlations

```racket
(query
 (define x (if (flip 1/2) 0 1))
 (make-blog-matrix (blog-list x x)))
```

The outcomes are `(blog-matrix-value 1 2 '(0 0))` and
`(blog-matrix-value 1 2 '(1 1))`, each with probability 1/2. Both cells refer to
the same choice, so neither `(0 1)` nor `(1 0)` can occur. Here, `query` is the
inference operation from `roulette/example/disrupt`.

### Symbolic source alternatives are converted individually

```racket
(query
 (make-blog-matrix
  (if (flip 1/2)
      (blog-list 10 20)
      (blog-list (blog-list 30) (blog-list 40)))))
```

The outcomes are a 1×2 row matrix containing `(10 20)` and a 2×1 column matrix
containing `(30 40)`, each with probability 1/2. The outer `for/all` handles
alternative source values, including alternatives with different shapes.

### Reachable invalid cell alternatives are rejected

```racket
(make-blog-matrix
 (blog-list (if (flip 1/2) 1 #f)))
;; error: #f is not a real numeric matrix element
```

The cell could be numeric, but it could also be false. Validation checks its
reachable alternatives rather than accepting only the numeric possibility.

## 6. Parser rules and their examples

The rules we discussed were:

```racket
(expr
 [(atom) $1]
 [(lp expr rp) $2]
 [(postfix) $1])
```

This is an illustration of three alternatives, not the current complete
grammar. `$1` selects the first matched part and `$2` selects the second.
The current `expr` rule only needs the `postfix` alternative for these cases:
`postfix` includes atoms, and atoms include parenthesized expressions.

### Atoms

| BLOG fragment | Parsed expression |
| --- | --- |
| `42` | `42` |
| `true` | `#t` |
| `a` | `a` |
| `f(1, 2)` | `(call f 1 2)` |
| `[10, 20]` | `(blog-list 10 20)` |

The rule `[(atom) $1]` returns the atom's parsed expression unchanged. An atom
can contain structure internally, such as the arguments of a function call.

### Parenthesized expressions

| BLOG fragment | Parsed expression |
| --- | --- |
| `(42)` | `42` |
| `(a + b)` | `(+ a b)` |
| `(a[i])` | `(index a i)` |

The rule `[(lp expr rp) $2]` matches `(`, an expression, and `)`. Returning `$2`
discards the parentheses, which control grouping without becoming a runtime
operation.

### Postfix expressions

| BLOG fragment | Parsed expression |
| --- | --- |
| `a` | `a` |
| `a[i]` | `(index a i)` |
| `a[i][j]` | `(index (index a i) j)` |
| `f()[0]` | `(index (call f) 0)` |
| `transpose(a)[0]` | `(index (call transpose a) 0)` |
| `(a + b)[i]` | `(index (+ a b) i)` |
| `[10, 20][1]` | `(index (blog-list 10 20) 1)` |

A postfix expression starts with an atom and accepts zero or more index
suffixes. The recursive rule builds chained indexing from left to right.
Parsing these expressions does not establish that the values support indexing.

```text
expr → postfix → atom
               → postfix[expr]
```

This grammar already covers atoms and grouped expressions. Including all
three alternatives directly in `expr` would create duplicate parsing paths.

## 7. The result-conversion macro

The macro was initially named `blog-result`. It was renamed to
`blog-result-warpper`, preserving the requested spelling.

```racket
(define-syntax blog-result-warpper
  (syntax-parser
    [(_ (~datum RealMatrix) body:expr)
     #'(make-blog-matrix body)]
    [(_ result-type:id body:expr)
     #'body]))
```

`_` matches the macro name. `(~datum RealMatrix)` matches that specific type
name; `body:expr` captures an expression. The second rule accepts any other
type identifier. `#'` constructs the syntax that replaces the macro use.

### A matrix annotation wraps the body

```racket
(blog-result-warpper RealMatrix (blog-list 1 2))
;; expands to:
(make-blog-matrix (blog-list 1 2))
;; evaluates to:
;; (blog-matrix-value 1 2 '(1 2))
```

The specific matrix rule comes before the general rule, so it handles the
`RealMatrix` annotation and inserts conversion at runtime.

### Other annotations leave the body unchanged

```racket
(blog-result-warpper Integer (+ 1 2))
;; expands to: (+ 1 2)
;; evaluates to: 3

(blog-result-warpper Boolean 42)
;; expands to: 42
;; evaluates to: 42
```

The second example intentionally uses a mismatched annotation. Other return
types are still unchecked; this macro only adds checking for `RealMatrix`.

### A fixed declaration uses the wrapper

```racket
(fixed RealMatrix a () (blog-list 1 2))

;; expands first to:
(define a (blog-result-warpper RealMatrix (blog-list 1 2)))

;; then to:
(define a (make-blog-matrix (blog-list 1 2)))
```

These are explanatory expansion stages for internal macros. The conversion
evaluates once when the constant is defined. For a fixed function with
parameters, it evaluates when the function is called.

## 8. What `define-random` does

```racket
(define-syntax define-random
  (syntax-parser
    [(_ name:id () body:expr)
     #'(define name body)]
    [(_ name:id ((arg-type:id arg:id) ...+) body:expr)
     #'(define name
         (make-random-function 'name (list arg-type ...)
                               (lambda (arg ...) body)))]))
```

The macro determines how a random declaration stores its results. The outer
`random` macro has already inserted distribution drawing for `~` declarations
and result conversion for `RealMatrix` annotations.

### No parameters: one stored random value

```racket
(define-random X () (draw-distribution (Bernoulli 0.5)))

;; expands to:
(define X (draw-distribution (Bernoulli 0.5)))
```

The body evaluates once when `X` is defined. Referencing `X` again uses the
same random variable, rather than allocating another independent choice.

### One parameter: one value per object

```racket
(define-random Healthy ((Person p))
  (draw-distribution (BooleanDistrib 0.8)))

;; expands to:
(define Healthy
  (make-random-function
   'Healthy
   (list Person)
   (lambda (p)
     (draw-distribution (BooleanDistrib 0.8)))))
```

If the declared `Person` objects are Alice and Bob, the implementation
evaluates the body once for Alice and once for Bob. Each gets an independent
choice in this example. Repeated calls for Alice retrieve Alice's stored
choice. Grounding uses the objects declared when the function is defined.

### Multiple parameters: one value per argument combination

```racket
;; Schematic example: body stands for the prepared result expression.
(define-random Friends ((Person p) (Person q)) body)
```

With Alice and Bob, the table contains entries for `(Alice, Alice)`,
`(Alice, Bob)`, `(Bob, Alice)`, and `(Bob, Bob)`. Each ordered pair has one
stored result. In the macro, `...+` requires one or more parameter pairs;
`arg-type ...` and `arg ...` collect their types and names.

## 9. Reusing a random variable versus independent choices

```text
random Integer X ~ Bernoulli(0.5);
query X + X;
```

There is one random choice. Both occurrences of `X` refer to it:

| X | X + X | Probability |
| --- | --- | --- |
| 0 | 0 | 0.5 |
| 1 | 2 | 0.5 |

The result cannot be 1: the two occurrences always agree. Saying “every
reference uses the same random value” describes this correlation; it does
not mean inference fixes `X` to one particular outcome for all possible worlds.

```text
random Integer X ~ Bernoulli(0.5);
random Integer Y ~ Bernoulli(0.5);
query X + Y;
```

These declarations allocate two independent choices:

| X + Y | Probability |
| --- | --- |
| 0 | 0.25 |
| 1 | 0.5 |
| 2 | 0.25 |

The middle result can occur as either `(X = 0, Y = 1)` or
`(X = 1, Y = 0)`. With no parameters, a declaration stores one random value;
with parameters, it stores one per argument combination.

## 10. Matrix arithmetic and the matrix library

Matrix operations take matrix values, not raw BLOG lists. Declare a list as
`RealMatrix` or use a constructor such as `eye` before applying these operations.
Arithmetic on ordinary real numbers retains its existing behavior.

### Addition, subtraction, unary signs, and multiplication

```text
fixed RealMatrix a = [1, 2; 3, 4];
fixed RealMatrix b = [10, 20; 30, 40];

query a + b;        // [11, 22; 33, 44]
query b - a;        // [9, 18; 27, 36]
query -a;           // [-1, -2; -3, -4]
query +a;           // [1, 2; 3, 4]
query a * a;        // [7, 10; 15, 22]
```

Addition and subtraction require matching shapes. Multiplication is standard
matrix multiplication: the left matrix's column count must equal the right
matrix's row count. It computes row/column dot products rather than multiplying
corresponding cells.

### Scalar addition, subtraction, scaling, and division

```text
fixed RealMatrix a = [1, 2; 3, 4];
query a + 10;       // [11, 12; 13, 14]
query 10 + a;       // [11, 12; 13, 14]
query a - 10;       // [-9, -8; -7, -6]
query 10 - a;       // [9, 8; 7, 6]
query 2 * a;        // [2, 4; 6, 8]
query a * 2;        // [2, 4; 6, 8]
query a / 2;        // [1/2, 1; 3/2, 2] internally, with exact rational entries
```

Scalar addition and subtraction apply to every cell, preserving the matrix's
dimensions and symbolic correlations. `10 - a` computes `10 - cell` for each
entry. Adding a scalar does not mean adding a scaled identity matrix; write
`a + 10 * eye(2)` to add 10 only to this matrix's diagonal.

Scaling works on either side of multiplication. Division supports a nonzero
scalar denominator. Matrix division remains unsupported; for example,
`a * inv(b)` explicitly expresses multiplication by a matrix inverse.

### Constructors and diagonal matrices

```text
query eye(2);       // [1, 0; 0, 1]
query zeros(2, 3);  // [0, 0, 0; 0, 0, 0]
query ones(2, 1);   // [1; 1]

fixed RealMatrix column = [2; 3];
query diag(column); // [2, 0; 0, 3]
```

Dimensions must be nonnegative exact integers and may depend on finite random
choices. `diag` requires a column vector and uses its entries as the diagonal.
Zero dimensions are allowed and remain explicit in the matrix representation.

### Tiling and stacking

```text
fixed RealMatrix a = [1, 2; 3, 4];
query repmat(a, 2, 2);
// [1, 2, 1, 2; 3, 4, 3, 4; 1, 2, 1, 2; 3, 4, 3, 4]

query hstack(1, 2, 3); // [1, 2, 3]
query vstack(1, 2, 3); // [1; 2; 3]

fixed RealMatrix column = [5; 6];
query hstack(a, column); // [1, 2, 5; 3, 4, 6]
```

`repmat` repeats the complete matrix vertically and horizontally. `hstack`
requires matching row counts; `vstack` requires matching column counts. Both
accept one or more arguments and treat scalar arguments as singleton matrices,
so compatible mixtures of scalars and matrices are allowed.

### Reductions and elementwise functions

```text
fixed RealMatrix a = [1, 2; 3, 4];
query sum(a);       // row matrix [4, 6]
query trace(a);     // scalar 5
query det(a);       // scalar -2

fixed RealMatrix b = [-1, 2; -3, 4];
query abs(b);       // [1, 2; 3, 4]

fixed RealMatrix c = [0, 1];
query exp(c);       // [1, e], where e is approximately 2.71828
```

`sum` adds each column and returns a row matrix. `trace` adds the diagonal of a
square matrix. `det` computes its determinant. `abs` and `exp` apply to every
cell and also accept scalar inputs.

### Inverses and singular matrices

```text
fixed RealMatrix a = [1, 2; 3, 4];
query inv(a);       // [-2, 1; 3/2, -1/2], preserving exact entries
query inv(a) * a == eye(2); // true

fixed RealMatrix singular = [1, 2; 2, 4];
query det(singular); // 0
// inv(singular) raises an error.
```

Determinants and inverses require square matrices with finite real entries.
They use pivoted Gaussian elimination. A singular matrix has determinant zero
and no inverse. Exact inputs retain exact arithmetic; inexact computations
have ordinary floating-point rounding, so exact equality is not appropriate
for general floating-point inverse checks.

The empty 0×0 matrix has determinant 1 and an empty inverse. Its trace is 0.

### Random entries and dimensions preserve correlation

```text
random Integer x ~ UniformInt(0, 1);
random RealMatrix a = [x, 1; 0, x];
query toInt((a * a)[0][1]);
// 0 or 2, each with probability 1/2
```

The selected entry is `x + x`, so its two occurrences refer to the same choice.
Routine arithmetic and reductions retain symbolic entries rather than
enumerating every complete matrix.

```text
random Integer n ~ UniformInt(1, 2);
query eye(n) * ones(n, 1) == ones(n, 1);
// always true
```

Shape validation considers correlated dimension alternatives together. It does
not reject impossible combinations such as pairing `eye(1)` with `ones(2, 1)`
when both dimensions come from the same `n`.

Determinant and inverse currently enumerate finite matrix alternatives before
elimination. This preserves their probabilities and correlations but can be
expensive for matrices with many independent random cells. A singular inverse
alternative or another reachable argument error raises an error rather than
silently discarding that outcome.

## 11. How symbolic validation works

`flatten-symbolic` is a repository helper imported from
`roulette/private/util`, not a built-in Racket function. It maps concrete
outcomes to Boolean guards describing when those outcomes occur. The guards
are conditions, not probabilities.

```racket
(flatten-symbolic 25)
;; → (hash 25 #t)

(define b (flip 1/2))
(define x (if b 10 20))
(flatten-symbolic (+ x 1))
;; Reachable outcomes: 11 guarded by b, 21 guarded by (not b).
```

The helper traverses symbolic arithmetic and nested lists, vectors, and
structs. It preserves their structure rather than converting nested lists
into a single flat list. Multiple occurrences of the same choice retain their
relationship; impossible combinations may appear with a false guard.

For indexing, the input to validation is a list containing the exclusive
index limit and requested index:

```racket
(flatten-symbolic (list (blog-index-limit value) index))
```

With a fixed three-element list and index 1, this returns a hash mapping
`'(3 1)` to `#t`. If both values depend on the same Boolean choice, it exposes
their combinations together. For example, a singleton indexed at 0 when `b`
is true and a two-element list indexed at 1 otherwise have reachable entries
`'(1 0)` guarded by `b` and `'(2 1)` guarded by `(not b)`.

```racket
(for ([(choice guard)
       (in-hash
        (flatten-symbolic (list (blog-index-limit value) index)))]
      #:unless (base:eq? (&& guard (vc-assumes (vc))) #f))
  ;; Validate the concrete limit/index combination in choice.
  ...)
```

`in-hash` binds `choice` to the hash key and `guard` to its value. Hash
iteration order is unspecified. The `#:unless` clause skips an entry if its
guard combined with the current Rosette execution assumptions simplifies to
literal `#f`. It does not ask a solver to prove arbitrary conditions
unreachable. The loop validates remaining entries and returns `void`, or
raises an error; a separate expression in `blog-index` returns the selected
value.

## 12. Running and maintaining the tests

From this `roulette/example` directory:

```sh
raco test ../test/blog*.rkt
```

The suites separate syntax, object identities, random-function reuse, list
and matrix construction, declarations, indexing, and arithmetic. The original
`blog.rkt` suite exercises the reader and probabilistic inference together.
`blog-parser.rkt` tests the actual `read-syntax` interface and located parse
errors; the implementation does not export a `blog-parse-syntax` API or promise
individual source spans on every parsed subexpression.

`test/blog-test-support.rkt` provides the common parser helper, evaluator setup,
and expected-matrix constructor. Its `make-blog-runner` shares the BLOG module
instance with tests so actual and expected structs have matching identities.
The single-query helper evaluates a program inside an inference query and
returns its last expression's PMF hash. Multi-query tests use
`make-blog-multi-runner`, which calls `blog-run` and returns every snapshot's
PMF hash. Tests retain exact probability comparisons for exact computations.

To build the language manual into a temporary directory:

```sh
raco scribble --html --dest /private/tmp/roulette-blog-docs ../scribblings/blog.scrbl
```

Building this section by itself can report unresolved cross references to
other parts of the Roulette and Racket manuals; it still generates the HTML.

## 13. Sequential queries and joint distributions

Separate query statements use the observations preceding each statement:

```text
#lang roulette/example/blog

random Integer X ~ UniformInt(0, 1);
query X;             // {0: 1/2, 1: 1/2}
obs X = 1;
query X;             // {1: 1}
query X + X;         // {2: 1}
```

Running the file prints each result in order. The observation does not change
the already-computed first answer. All queries share the same model choices;
they do not redraw `X` or recreate grounded random functions.

Use commas in one query statement to request one joint distribution:

```text
random Integer X ~ UniformInt(0, 1);
query X, X + X;
// {(0, 0): 1/2, (1, 2): 1/2}
```

Joint outcomes are ordinary Racket lists in expression order. The two entries
share `X`, so neither `(0, 2)` nor `(1, 0)` occurs. A single-expression query
still produces its original value rather than a singleton tuple. In
particular, `query [X, X];` queries one BLOG list value, while `query X, X;`
uses ordinary Racket list keys for a joint tuple.

```text
random Integer X ~ UniformInt(0, 1);
random Integer Y ~ UniformInt(0, 1);
query X, Y;
// {(0, 0): 1/4, (0, 1): 1/4, (1, 0): 1/4, (1, 1): 1/4}
```

Independent variables can produce every tuple. Joint support can grow as the
product of the component supports; separate marginal queries can be cheaper
when an application does not need their correlation.

### Returning every query result to a Racket application

`blog-run` executes raw BLOG text without a `#lang` line and returns an ordered
list of `blog-query-result` records. It is usable from ordinary Racket:

```racket
#lang racket/base

(require (only-in roulette/example/blog
                 blog-run
                 blog-query-result-number
                 blog-query-result-expressions
                 blog-query-result-probabilities))

(define results
  (blog-run 'example.blog
            (open-input-string
             "random Integer X ~ UniformInt(0, 1);
              query X;
              query X, X + X;
              obs X = 1;
              query X;")))

(map blog-query-result-number results)
;; → '(1 2 3)

(map blog-query-result-expressions results)
;; → '((X) (X (+ X X)) (X))

(map blog-query-result-probabilities results)
;; → (list (hash 0 1/2 1 1/2)
;;          (hash '(0 0) 1/2 '(1 2) 1/2)
;;          (hash 1 1))
```

Expression metadata contains parsed datums, not copies of the original source
text. Duplicate queries remain separate records. A program with no queries
returns an empty list. Each call isolates its definitions and observations,
produces no automatic query output, and propagates errors rather than returning
a partial collection. The next call starts with a fresh observation session.

To run a raw BLOG file from Racket:

```racket
(define results
  (call-with-input-file "model.blog"
    (lambda (in) (blog-run "model.blog" in))))
```

For a module containing `#lang roulette/example/blog`, `(require "model.rkt")`
continues to execute it and print all query results. That module-loading path
does not automatically export the records returned by `blog-run`.
