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

@hyperlink["https://bayesianlogic.github.io/download/blog-langref.pdf"]{BLOG} is
a small probabilistic programming language.
This implementation supports a subset of BLOG,
built on @racketmodname[roulette/example/disrupt].
Simple BLOG programs should run without modification.

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
 }

 @item{Comments use @tt{//} or non-nesting @tt{/* ... */}.}
]

@section{Lists and matrices}

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

Conversion applies to fixed constants and functions, and to both forms of
random declaration, after a distribution draw or deterministic evaluation.
Random functions store the converted result once per grounded argument tuple.
An unannotated list expression remains a list, so list and matrix equality
does not treat identical-looking contents as the same value.

@subsection{Indexing and scalar conversion}

All indices are exact, zero-based integers. Indexing dispatches on the value:

@itemlist[
 @item{An indexed distinct-object family returns the selected object.}
 @item{A list returns its stored element, which may itself be a list.}
 @item{A matrix with multiple rows and columns returns a row matrix.}
 @item{A row or column vector returns a 1-by-1 matrix.}
]

@verbatim{
query [10, 20, 30][1];                // scalar 20
query [[10, 20], [30, 40]][1];        // list [30, 40]
query [[10, 20], [30, 40]][1][0];     // scalar 30

fixed RealMatrix m = [10, 20, 30; 40, 50, 60];
query m[1];                         // matrix [40, 50, 60]
query m[1][2];                      // singleton matrix [60]
query toInt(m[1][2]);                // scalar integer 60
query toReal(m[1][2]);               // scalar real 60.0
query transpose(m)[1];              // matrix [20, 50]
}

Indexing binds more tightly than arithmetic operators and can follow a literal,
function call, parenthesized expression, or another index. For example,
@tt{(a + b)[i][j]} selects a singleton from a matrix sum.
Negative, fractional, inexact integer, Boolean, and out-of-bounds indices raise
runtime errors. Empty vectors have no valid index; a row of a matrix with
multiple empty rows can still be selected as an empty row matrix.

@tt{toReal} and @tt{toInt} accept a real or Boolean scalar, or extract the
element of a 1-by-1 matrix. Larger or empty matrices are rejected.
@tt{toReal} produces an inexact real; @tt{toInt} truncates toward zero and
produces an exact integer. Booleans convert to 1 or 0. Integer conversion
rejects nonfinite numbers.

@subsection{Arithmetic and the matrix library}

Matrix operands must be matrix values, supplied by @tt{RealMatrix}
declarations or matrix-producing functions. Raw list literals are not
automatically converted at an arithmetic operator or library call.

@itemlist[
 @item{@tt{a + b} and @tt{a - b} require identical matrix dimensions.
  Unary @tt{+} preserves a matrix and unary @tt{-} negates its cells.}
 @item{@tt{a * b} performs matrix multiplication, requiring the left column
  count to equal the right row count. It is not elementwise multiplication.}
 @item{@tt{s * a} and @tt{a * s} scale a matrix by a real scalar.
  @tt{a / s} divides each cell by a nonzero scalar. Matrix divisors and
  scalar addition/subtraction with a matrix are unsupported.}
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
query a * a;                        // [7, 10; 15, 22]
query sum(a);                       // row matrix [4, 6]
query inv(a) * a == eye(2);          // true with these exact inputs
}

Inexact arithmetic has ordinary floating-point rounding. Use numerical
tolerances when comparing floating-point results from outside BLOG; exact
matrix equality is appropriate for the exact example above.

@subsection{Random indices, entries, and dimensions}

Finite random values may determine indices, matrix entries, and dimensions.
Repeated references preserve the same underlying random choices:

@verbatim{
random Integer i ~ UniformInt(0, 1);
random RealMatrix a = [i, 10; 20, i];
query toInt(a[i][i]) == i;           // always true
}

Bounds and dimension checks validate correlated alternatives together. For
example, the two reachable shape/index combinations below are both valid:

@verbatim{
random Integer i ~ UniformInt(0, 1);
random RealMatrix a =
  if i == 0 then [7] else [10, 20; 30, 40];
query toInt(a[i][0]);                // 7 or 30, each with probability 1/2
}

Validation skips alternatives whose guards simplify to false under the current
execution assumptions. It does not invoke an SMT solver to establish arbitrary
unreachability. Invalid alternatives retained by the checks raise errors;
they are not silently dropped from the distribution.

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
}]
