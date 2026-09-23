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
This implementation supports a subset of BLOG, built on
@racketmodname[roulette/example/disrupt].

Simple BLOG programs should run out of the box.

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



@section{Supported features}

@itemlist[
 @item{@bold{Types and objects.} Declare types with @tt{type} and objects with
 @tt{distinct}. Indexed declarations such as @tt{distinct Person P[3];}
 create three distinct objects, accessible as @tt{P[0]}, @tt{P[1]}, and
 @tt{P[2]}. Indices may be arithmetic expressions or finite random values.}

 @item{@bold{Fixed and random functions.} @tt{fixed} declarations define
 constants or functions with parameters. @tt{random} declarations use
 @tt{~} for a distribution or @tt{=} for a deterministic dependency.
 Repeated calls to a random function with the same arguments reuse the same
 random value.}

 @item{@bold{Distributions.} @tt{BooleanDistrib(p)} returns a Boolean;
 @tt{Bernoulli(p)} returns 1 with probability @tt{p} and 0 otherwise.
 @tt{UniformInt(lo, hi)} gives equal probability to every integer between
 the bounds, including both endpoints. @tt{Categorical} accepts a map such as
 @tt{Categorical({0 -> 0.25, 1 -> 0.75})}; probabilities must sum to one.
 Distribution parameters and the choice of distribution may depend on
 finite random values.}

 @item{@bold{Arithmetic and comparisons.} Supported arithmetic is
 @tt{+}, @tt{-}, @tt{*}, and unary negation. Comparisons are
 @tt{==}, @tt{!=}, @tt{<}, @tt{<=}, @tt{>}, and @tt{>=}.}

 @item{@bold{Logic and quantifiers.} Boolean operators are @tt{!}, @tt{&},
 @tt{|}, and @tt{=>}. The @tt{forall} and @tt{exists} quantifiers range over
 the objects of a declared type.}

 @item{@bold{Conditionals.} Expressions support
 @tt{if condition then expression else expression}, including nested
 conditionals and an optional @tt{else}. Case expressions use
 @tt{case value in {key -> result, ...}}. Branches may return values or
 distributions. An omitted @tt{else} or an unmatched case returns @tt{null}.}

 @item{@bold{Evidence and queries.} Supply evidence with
 @tt{obs expression = expression;} and query an expression with
 @tt{query expression;}.}

 @item{@bold{Literals and comments.} Supported literals include integers,
 reals (including scientific notation), @tt{true}, @tt{false}, and @tt{null}.
 Comments use @tt{//} or non-nesting @tt{/* ... */}.}
]

@section{Current limitations}

Object populations are fixed and finite. Number statements for unknown
populations and origin functions are not supported. Random-function arguments
must range over declared object types; built-in domains such as @tt{Integer}
are not supported as random-function argument domains. 
We expect it is not hard to overcome the limitation that object populations 
are fixed, but requires more substantial work to support distributions 
over infinite domains. 

Random functions are eagerly instantiated over the objects declared so far.
Objects and parent random functions must therefore precede their dependents;
objects declared later are not added to an existing random function's domain.
Type annotations are not fully checked.

Division, exponentiation, remainder, sets and set comprehensions, general
arrays and matrices, strings, characters, and timestep literals are not
supported. Indexed distinct objects are supported, but are not general arrays.
Distributions other than the four listed above are not provided.
