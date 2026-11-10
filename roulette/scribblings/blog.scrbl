#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@(require racket/sandbox
	  scribble/example)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; constants

@(define evaluator (make-base-eval #:lang 'roulette/example/disrupt))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{The BLOG Language}
@defmodule*[(roulette/example/blog) #:lang]

This implementation supports a subset of @hyperlink["https://bayesianlogic.github.io/download/blog-langref.pdf"]{BLOG},
built on @racketmodname[roulette/example/disrupt].
Simple BLOG programs should run without modification after starting the file with
@tt{#lang roulette/example/blog}. For example:


@filebox["blog-example.rkt"]{@verbatim[#<<END
#lang roulette/example/blog

type Person;
distinct Person Alice, Bob;

fixed Boolean AlwaysTrue = true;
fixed Boolean IsAlice(Person p) = p == Alice;

random Real prob ~ Categorical({0.1 -> 0.5, 0.9 -> 0.5});

random Boolean Smokes(Person p) ~ BooleanDistrib(prob);

random Boolean Healthy(Person p) ~
  if Smokes(p) then BooleanDistrib(0.4)
  else BooleanDistrib(0.9);


random Boolean Infected(Person p) ~ BooleanDistrib(prob);

random Boolean ExtraHealthy(Person p) ~
  if (exists Person q Infected(q)) then BooleanDistrib(0.2)
  else BooleanDistrib(0.8);

obs Smokes(Alice) = true;

query (ExtraHealthy(Alice));
END
]}

Here are the supported features:

@itemlist[
 @item{Declare types with @tt{type} and objects with
  @tt{distinct}. Indexed declarations such as @tt{distinct Person P[3];}
  create three distinct objects, accessible as @tt{P[0]}, @tt{P[1]}, and
  @tt{P[2]}. Indices may be arithmetic expressions or finite random values.
 }

 @item{The @tt{fixed} declarations define constants or functions with parameters.
  The @tt{random} declarations use @tt{~} for a distribution or @tt{=} for a deterministic dependency.
  Repeated calls to a random function with the same arguments reuse the same random value.
 }

 @item{@tt{BooleanDistrib(p)} returns a Boolean.
  @tt{Bernoulli(p)} returns @tt{1} with probability @tt{p} and @tt{0} otherwise.
  @tt{UniformInt(lo, hi)} gives equal probability to every integer between
  the bounds, including both endpoints.
  @tt{Categorical} accepts a map such as @tt{Categorical({0 -> 0.25, 1 -> 0.75})}
  where the probabilities must sum to one.
  Distribution parameters and the choice of distribution may depend on finite random values.
 }

 @item{
  The arithmetic operators are @tt{+}, @tt{-}, @tt{*}, @tt{/}, @tt{^}, @tt{%},
  and unary @tt{+} and @tt{-}.
  The comparison operators are @tt{==}, @tt{!=}, @tt{<}, @tt{<=}, @tt{>}, and @tt{>=}.
  The Boolean operators are @tt{!}, @tt{&}, @tt{|}, and @tt{=>}.
  The @tt{forall} and @tt{exists} quantifiers range over the objects of a declared type.
 }

 @item{
  Conditionals are written as @tt{if condition then expression else expression}.
  Case expressions use @tt{case value in {key -> result, ...}}.
  Branches may return values or distributions.
  An omitted @tt{else} or an unmatched case returns @tt{null}.
 }

 @item{
  Supply evidence with @tt{obs expression = expression;}
  and query an expression with @tt{query expression;}.
  Queries execute sequentially using preceding observations.
 }

 @item{
  Query joint distributions with @tt{query expression1, expression2;}.
  Each outcome is a tuple in expression order, preserving correlations
  between expressions that share random choices.
 }

 @item{
  Construct numeric vectors and matrices with @tt{RealMatrix} declarations,
  such as @tt{fixed RealMatrix m = [1, 2; 3, 4];}.
  Matrix support includes indexing, scalar conversion, arithmetic,
  transpose, constructors, stacking, reductions, determinants, and inverses.
  Entries and dimensions may depend on finite random choices.
 }

 @item{Comments use @tt{//} or non-nesting @tt{/* ... */}.}
]

@section[#:tag "blog-declarations"]{Declaration order and random-function domains}

Random functions are instantiated eagerly: when a declaration is evaluated,
its body is evaluated once for every argument tuple formed from the objects
currently declared in its parameter types. The resulting table is fixed for
that function. Later object declarations do not extend it.

For example, both people below exist before @tt{Age} is declared:

@verbatim{
#lang roulette/example/blog

type Person;
distinct Person Alice, Bob;
random Integer Age(Person p) ~ UniformInt(20, 40);

query Age(Alice);                    // works
query Age(Bob);                      // works
query Age(Alice) == Age(Alice);       // always true
}

The declaration creates one random age for Alice and another for Bob.
Repeated calls with the same person retrieve that person's stored choice;
calling the function does not draw another age.

In this separate example, Bob is declared too late for @tt{Age}:

@verbatim{
type Person;
distinct Person Alice;
random Integer Age(Person p) ~ UniformInt(20, 40);
distinct Person Bob;

query Age(Alice);                    // works
query Age(Bob);                      // runtime error: outside grounded domains
}

Bob belongs to @tt{Person}, but the existing function table has no entry for
him. A quantifier over @tt{Person} can see Bob after his declaration; this does
not create an @tt{Age(Bob)} entry. Declare the complete object population
before the random function when all of its members need entries:

@verbatim{
type Person;
distinct Person Alice;
distinct Person Bob;
random Integer Age(Person p) ~ UniformInt(20, 40);
query Age(Alice), Age(Bob);           // works
}

For multiple parameters, instantiation covers every ordered argument tuple:

@verbatim{
type Person;
distinct Person Alice, Bob;
random Boolean Friends(Person p, Person q) ~ BooleanDistrib(0.5);
}

This creates entries for @tt{Friends(Alice, Alice)},
@tt{Friends(Alice, Bob)}, @tt{Friends(Bob, Alice)}, and
@tt{Friends(Bob, Bob)}. Declaring Charlie afterward creates no additional
entries in this table. An empty parameter domain produces no entries.

These rules apply to random declarations using either @tt{~} or @tt{=}.
Types, objects, and parent functions used by the eagerly evaluated body must
already be available. A zero-parameter random declaration evaluates its body
once and stores a single value. Fixed functions are ordinary functions and do
not create a table over the object population when declared.

@subsection{Shared and independent random choices}

A stored random value represents all of its possible outcomes during
inference. Reusing it preserves correlation; it does not select one outcome
for the whole computation:

@verbatim{
random Integer X ~ Bernoulli(0.5);
random Integer Y ~ Bernoulli(0.5);
query X + X;     // 0 or 2, each with probability 0.5
query X + Y;     // 0: 0.25, 1: 0.5, 2: 0.25
}

Both occurrences of @tt{X} refer to the same choice, so @tt{X + X} cannot
equal 1. The second declaration creates an independent choice; @tt{X + Y}
can equal 1 in either of two ways.

@section{Sequential and joint queries}

A program may contain multiple query statements. Each statement requests one
result: either a usual single-expression marginal distribution or a joint
distribution over comma-separated expressions. Both kinds execute sequentially.

@subsection{Usual sequential queries}

Each query uses declarations and observations evaluated before its statement.
Later observations do not change an earlier result. Running a BLOG module
prints every query result in source order:

@verbatim{
#lang roulette/example/blog

random Integer X ~ UniformInt(0, 1);
query X;             // 0 or 1, each with probability 1/2
obs X = 1;
query X;             // 1 with probability 1
query X + X;         // 2 with probability 1
}

Queries reuse the same model allocations; they do not redraw random constants
or grounded random functions. Declarations remain subject to the existing
loading order and grounding rules.

@subsection{Multiple separate queries versus one joint query}

Consider two expressions sharing one random variable:

@verbatim{
random Integer X ~ UniformInt(0, 1);
query X;             // First result:  0 or 1, each with probability 1/2.
query X + X;         // Second result: 0 or 2, each with probability 1/2.
}

These are two separate marginal distributions. They reuse the same @tt{X},
but the results do not explicitly list which pairs can occur together.
Multiplying their marginal probabilities would incorrectly assume independence:
for example, the pair @tt{(0, 2)} is impossible even though each component
appears in its respective marginal.

To request that relationship explicitly, separate the expressions with commas
in one joint-query statement:

@verbatim{
random Integer X ~ UniformInt(0, 1);
query X, X + X;
// Joint outcomes: (0, 0) and (1, 2), each with probability 1/2.
}

Two independent variables instead produce all four pairs:

@verbatim{
random Integer X ~ UniformInt(0, 1);
random Integer Y ~ UniformInt(0, 1);
query X, Y;
// (0, 0), (0, 1), (1, 0), (1, 1), each with probability 1/4.
}

Observations also affect subsequent joint queries. For example,
@tt{obs X = 1;} followed by @tt{query X, X + X;} has only the outcome
@tt{(1, 2)}. An observation placed between two separate queries affects only
the later query; an observation before a joint query conditions the entire
joint tuple. No observation can occur between the comma-separated expressions
of one query statement. Empty joint components and trailing commas are syntax
errors.

@section[#:tag "blog-matrices"]{Lists and matrices}

Square brackets construct lists. These two expressions are equivalent:

@verbatim{
[[1, 2, 3], [4, 5, 6]]
[1, 2, 3; 4, 5, 6]
}

The semicolon form is shorthand for a list of rows. Lists may be empty,
nested, ragged, or contain nonnumeric values. Shorthand rows must be nonempty;
write empty rows explicitly, as in @tt{[[], []]}.

A @tt{RealMatrix} return annotation converts a list to a numeric matrix:

@verbatim{
fixed RealMatrix row = [10, 20, 30];
fixed RealMatrix column = [10; 20; 30];
fixed RealMatrix table = [10, 20, 30; 40, 50, 60];
}

A flat list becomes a row vector, and nested lists become matrix rows.
Conversion requires rectangular data with real numeric cells. Boolean cells,
mixed rows and scalars, and nesting deeper than two levels are rejected.
Existing matrices pass through unchanged. Exact integers and rationals remain
exact; the constructor does not coerce every cell to floating point.

The empty list becomes a 0-by-0 matrix. Explicit empty rows retain their
dimensions: @tt{[[], []]} becomes a 2-by-0 matrix. Matrix constructors also
accept zero dimensions.

@subsection{When a list becomes a matrix}

The @tt{RealMatrix} annotation tells a declaration to convert its result
into a matrix. Brackets alone create a list. For fixed declarations:

@verbatim{
fixed RealMatrix A = [1, 2];
fixed RealMatrix Pair(Integer x) = [x, x];
query Pair(3);                      // row matrix [3, 3]
}

Defining @tt{A} evaluates @tt{[1, 2]}, converts that list into a row matrix,
and stores it once. Calling @tt{Pair(3)} evaluates @tt{[3, 3]} and converts
the result before returning it. A fixed function performs this evaluation
and conversion on each call.

Random declarations convert their results too, but @tt{~} and @tt{=}
produce those results in different ways:

@verbatim{
random Integer X ~ UniformInt(0, 1);
random RealMatrix Draw ~ Categorical({[1, 2] -> 0.5, [3, 4] -> 0.5});
random RealMatrix FromX = [X, X];
query Draw;                         // matrix [1, 2] or [3, 4], equally likely
query FromX;                        // matrix [0, 0] or [1, 1], equally likely
}

The @tt{~} declaration draws from a distribution of lists, then converts
the result to a matrix. The @tt{=} declaration computes a list from the
existing @tt{X}, then converts it. Both entries of @tt{FromX} share that
same choice; this declaration introduces no independent draw. These
zero-parameter random declarations store their results once.

A random function stores one converted result for each combination of
declared object arguments:

@verbatim{
type Person;
distinct Person Alice, Bob;
random RealMatrix M(Person p) ~ Categorical({[1, 2] -> 0.5, [3, 4] -> 0.5});
query M(Alice) == M(Alice);          // always true
query M(Alice) == M(Bob);            // true or false, each with probability 0.5
}

When @tt{M} is declared, Alice and Bob each get an independent matrix-valued
choice. Each result is converted and stored at that time. Repeated calls to
@tt{M(Alice)} retrieve Alice's stored choice; they do not draw or convert again.

Outside a @tt{RealMatrix} declaration, a list stays a list. Equality compares
the value types as well as their contents; it does not insert a conversion:

@verbatim{
fixed RealMatrix m = [1, 2];
query m == [1, 2];                  // false: a matrix compared with a list
fixed RealMatrix n = [1, 2];
query m == n;                       // true: matrices with equal contents
}

@subsection{Indexing and scalar conversion}

Indices are exact, zero-based integers. Families return objects and lists
return stored elements. A matrix with multiple rows and columns returns a
row matrix; indexing a row or column vector returns a 1-by-1 matrix.

@verbatim{
query [[10, 20], [30, 40]][1][0];     // scalar 30
fixed RealMatrix m = [10, 20, 30; 40, 50, 60];
query m[1];                         // matrix [40, 50, 60]
query m[1][2];                      // singleton matrix [60]
query toReal(m[1][2]);               // scalar real 60.0
query transpose(m)[1];              // matrix [20, 50]
}

Indexing binds tightly and supports calls and grouped expressions, such as
@tt{(a + b)[i][j]}. Invalid or out-of-bounds indices raise errors.
@tt{toReal} and @tt{toInt} accept real or Boolean scalars and singleton
matrices. They produce an inexact real or an exact integer truncated toward
zero; Booleans become 1 or 0. Larger or empty matrices are rejected, and
integer conversion rejects nonfinite numbers.

@subsection{Arithmetic and the matrix library}

Matrix operands must be matrix values, supplied by @tt{RealMatrix}
declarations or matrix-producing functions. Raw list literals are not
automatically converted at an arithmetic operator or library call.

@itemlist[
 @item{@tt{a + b} and @tt{a - b} require identical matrix dimensions.
  Unary @tt{+} preserves a matrix and unary @tt{-} negates its cells.}
 @item{@tt{a * b} performs matrix multiplication, requiring the left column
  count to equal the right row count. It is not elementwise multiplication.}
 @item{@tt{a + s}, @tt{s + a}, @tt{a - s}, and @tt{s - a} apply real
  scalar addition/subtraction to every matrix cell, preserving the shape.
  In particular, @tt{s - a} computes @tt{s - cell} for each cell; adding a
  scalar does not mean adding a scaled identity matrix.}
 @item{@tt{s * a} and @tt{a * s} scale a matrix by a real scalar.
  @tt{a / s} divides each cell by a nonzero scalar. Matrix divisors remain
  unsupported.}
 @item{@tt{eye(n)}, @tt{zeros(rows, columns)}, and @tt{ones(rows, columns)}
  construct identity, zero-filled, and one-filled matrices. Dimensions must
  be nonnegative exact integers.}
 @item{@tt{diag(column)} creates a diagonal matrix from a column vector.
  @tt{repmat(m, rows, columns)} tiles a complete matrix vertically and
  horizontally using nonnegative exact integer repetition counts.}
 @item{@tt{hstack(...)} joins matrices with matching row counts.
  @tt{vstack(...)} joins matrices with matching column counts. Both require
  at least one argument and treat real scalars as 1-by-1 matrices, allowing
  compatible mixtures of scalars and matrices.}
 @item{@tt{sum(m)} returns the column sums as a row matrix.
  @tt{trace(m)} returns the diagonal sum of a square matrix.}
 @item{@tt{abs(m)} and @tt{exp(m)} apply elementwise and also accept real
  scalar inputs.}
 @item{@tt{det(m)} and @tt{inv(m)} require square matrices with finite real
  entries. A singular matrix has determinant zero; its inverse raises an
  error. The 0-by-0 matrix has determinant 1, trace 0, and an empty inverse.}
]

@verbatim{
fixed RealMatrix a = [1, 2; 3, 4];
query a + a;                        // [2, 4; 6, 8]
query a + 10;                       // [11, 12; 13, 14]
query 10 - a;                       // [9, 8; 7, 6]
query a / 2;                        // exact entries 1/2, 1, 3/2, 2
query a * a;                        // [7, 10; 15, 22]
query sum(a);                       // row matrix [4, 6]
query det(a);                       // scalar -2
query inv(a);                       // [-2, 1; 3/2, -1/2]
query inv(a) * a == eye(2);          // true with these exact inputs
}

Other library calls construct and transform values:

@verbatim{
query hstack(eye(2), zeros(2, 1));    // [1, 0, 0; 0, 1, 0]
query vstack(ones(1, 2), zeros(1, 2)); // [1, 1; 0, 0]
fixed RealMatrix column = [2; 3];
query diag(column);                 // [2, 0; 0, 3]
query repmat(column, 1, 2);          // [2, 2; 3, 3]
query abs(-3);                      // 3
query exp(0);                       // 1
}

Inexact arithmetic has ordinary floating-point rounding. Use numerical
tolerances when comparing floating-point results from outside BLOG; exact
matrix equality is appropriate for the exact example above.

@subsection{Random entries and dimensions}

Finite random values may determine indices, matrix entries, and dimensions.
Repeated references preserve the same underlying random choices:

@verbatim{
random Integer x ~ UniformInt(0, 1);
random RealMatrix a = [x, x];
query a;                            // [0, 0] or [1, 1], each with probability 1/2
}

The cells share @tt{x}, so @tt{[0, 1]} and @tt{[1, 0]} cannot occur.
Dimensions can also depend on a shared random choice:

@verbatim{
random Integer n ~ UniformInt(1, 2);
query eye(n) * ones(n, 1) == ones(n, 1); // always true
}

Shape checks consider correlated dimensions together, so the last expression
does not pair @tt{eye(1)} with @tt{ones(2, 1)}. Invalid reachable alternatives
raise errors rather than disappearing from a distribution; see
@secref["blog-symbolic-validation"] for the validation rules.

Routine arithmetic retains symbolic cells. Determinant and inverse enumerate
finite concrete matrix alternatives before pivoted Gaussian elimination;
these operations can be expensive with many independent random entries.

@section{Current limitations}

@itemlist[
  @item{
   Object populations are fixed and finite. Number statements for unknown
   populations and origin functions are not supported. Random-function arguments
   must range over declared object types.
   Built-in domains such as @tt{Integer} are not supported as random-function argument domains.
 }

 @item{
  Random functions are eagerly instantiated over the objects declared so far.
  Objects and parent random functions must precede their dependents.
  Objects declared later are not added to an existing random function's domain.
  @tt{RealMatrix} return annotations are checked and converted; other type
  annotations are not fully checked.
 }

 @item{
  Sets and set comprehensions, higher-dimensional numeric arrays, strings,
  characters, and timestep literals are not supported. Numeric matrices use
  @tt{RealMatrix}; no separate @tt{IntegerMatrix} type is provided.
  Distributions other than the four listed above are not provided.
  @bold{Multivariate Gaussian sampling is not implemented.}
  Matrix-valued categorical distributions support finite vector outcomes;
  matrix support does not provide continuous Gaussian distributions.
}]

@section{Implementation notes}

The following Racket forms explain the implementation in
@filepath{roulette/example/blog.rkt}; they are not BLOG source syntax.
Internal helpers are not part of the public language API. The
@tt{test-support} submodule exposes selected constructors for testing.

@subsection{Parsing and normalized expressions}

@tt{blog-parse} accepts a source name and an input port containing raw BLOG
text, without a @tt{#lang} line. It returns declaration and expression
datums; parsing does not evaluate them:

@verbatim{
(blog-parse 'example.blog
            (open-input-string "fixed RealMatrix m = [1, 2; 3, 4];"))
;; '((fixed RealMatrix m ()
;;          (blog-list (blog-list 1 2) (blog-list 3 4))))
}

The empty parameter list marks a constant. Row shorthand has already become
nested @tt{blog-list} calls; matrix conversion happens when the declaration
is evaluated. Other representative translations are:

@tabular[#:sep @hspace[2]
 (list
  (list @bold{BLOG source} @bold{Parsed expression})
  (list @tt{42} @tt{42})
  (list @tt{true} @tt{#t})
  (list @tt{f(1, 2)} @tt{(call f 1 2)})
  (list @tt{(a + b)} @tt{(+ a b)})
  (list @tt{a[i][j]} @tt{(index (index a i) j)}))]

Atoms include literals, names, calls, and parenthesized expressions. A
postfix expression starts with an atom and may add index suffixes, associating
to the left. Parentheses control grouping and disappear from the result.
Parser actions such as @tt{$1} and @tt{$2} select matched grammar components;
for a parenthesized expression, @tt{$2} selects its contents. Routing these
forms through the postfix rule avoids duplicate grammar alternatives.

@subsection{Matrix representation}

BLOG lists use @tt{blog-list-value}; matrices use
@tt{blog-matrix-value}, with explicit row and column counts and an immutable
list of cells in row order. Constructor notation below shows those fields:

@verbatim{
(make-blog-matrix (blog-list 10 20))
;; (blog-matrix-value 1 2 '(10 20))

(make-blog-matrix (blog-list (blog-list 1 2) (blog-list 3 4)))
;; (blog-matrix-value 2 2 '(1 2 3 4))

(make-blog-matrix (blog-list))
;; (blog-matrix-value 0 0 '())

(make-blog-matrix (blog-list (blog-list) (blog-list)))
;; (blog-matrix-value 2 0 '())
}

Converting an existing matrix returns that same value. Numeric cells keep
their exactness: for example, a cell containing @tt{1/3} stays rational and
@tt{2.5} stays inexact. The shape and cell restrictions are described in
@secref["blog-matrices"]. The constructor additionally requires a tagged
BLOG list or an existing matrix; an ordinary Racket list such as
@tt{'(1 2)} and a standalone scalar are invalid inputs.

@subsection{Declaration expansion and result conversion}

The internal @tt{blog-result-warpper} macro inserts conversion for the
@tt{RealMatrix} annotation:

@verbatim{
(define-syntax blog-result-warpper
  (syntax-parser
    [(_ (~datum RealMatrix) body:expr)
     #'(make-blog-matrix body)]
    [(_ result-type:id body:expr)
     #'body]))
}

The first rule matches that specific type name; the second accepts other
type identifiers and leaves the body unchanged. Thus
@tt{(blog-result-warpper Boolean 42)} still evaluates to 42: annotations
other than @tt{RealMatrix} are not checked by this wrapper.

A fixed matrix constant passes through these expansion stages:

@verbatim{
(fixed RealMatrix m () (blog-list 1 2))
;; expands to:
(define m (blog-result-warpper RealMatrix (blog-list 1 2)))
;; then to:
(define m (make-blog-matrix (blog-list 1 2)))
}

Conversion runs once for a fixed constant and on each call to a fixed
function. For a random declaration, the outer @tt{random} macro prepares
the body: @tt{~} adds @tt{draw-distribution}, while @tt{=} uses the dependency
expression directly. It then applies the result wrapper and delegates
storage to @tt{define-random}:

@verbatim{
(define-syntax define-random
  (syntax-parser
    [(_ name:id () body:expr)
     #'(define name body)]
    [(_ name:id ((arg-type:id arg:id) ...+) body:expr)
     #'(define name
         (make-random-function 'name (list arg-type ...)
                               (lambda (arg ...) body)))]))
}

The zero-parameter rule stores the prepared result directly. The parameterized
rule passes the type domains and body procedure to @tt{make-random-function}.
Here, @tt{...+} requires at least one parameter pair; the other ellipses
collect the matched types and names. For example:

@verbatim{
(define-random X () (draw-distribution (Bernoulli 0.5)))
;; expands to:
(define X (draw-distribution (Bernoulli 0.5)))

(define-random Healthy ((Person p))
  (draw-distribution (BooleanDistrib 0.8)))
;; expands to:
(define Healthy
  (make-random-function 'Healthy (list Person)
    (lambda (p) (draw-distribution (BooleanDistrib 0.8)))))
}

The resulting function looks up stored values; it does not evaluate the
procedure again on every call. Its eager grounding and declaration-order
rules are described in @secref["blog-declarations"].

