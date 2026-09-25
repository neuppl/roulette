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
  The arithmetic operators is @tt{+}, @tt{-}, @tt{*}, @tt{/}, @tt{^}, @tt{%}, and unary negation.
  The comparisons operators are @tt{==}, @tt{!=}, @tt{<}, @tt{<=}, @tt{>}, and @tt{>=}.
  The boolean operators are @tt{!}, @tt{&}, @tt{|}, and @tt{=>}.
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

Here are the current limitations:

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
  Type annotations are not fully checked.
 }

 @item{
  Sets and set comprehensions, general
  arrays and matrices, strings, characters, and timestep literals are not
  supported. Indexed distinct objects are supported, but are not general arrays.
  Distributions other than the four listed above are not provided.
}]
