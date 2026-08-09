#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@(require (for-label "label.rkt"
		     (only-in roulette/example/disrupt
			      flip
			      query
			      observe!
			      sample
			      pmf?
			      in-pmf))
	  racket/sandbox
	  scribble/example)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; constants

@(define evaluator (make-base-eval #:lang 'roulette/example/disrupt))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Disrupt}
@defmodule*[(roulette/example/disrupt roulette/example/disrupt/safe) #:lang]

Disrupt is an example discrete probabilistic programming language built on top
of the RSDD inference engine.
@examples[#:eval evaluator #:label #f
  (define first-coin (flip 0.5))
  first-coin]
The @racket[flip] construct returns a Boolean where @racket[#t] has the given
probability. Evaluating @racket[first-coin] at the REPL prints out its probability
distribution.
@examples[#:eval evaluator #:label #f
  (define second-coin (flip 0.5))
  (define both-heads (and first-coin second-coin))
  both-heads]
Now, @racket[both-heads] is a Boolean that takes on
the value @racket[#t] when both coins are also @racket[#t].
@examples[#:eval evaluator #:label #f
  (observe! (not both-heads))
  first-coin]
Observing @racket[both-heads], specifically that it is @racket[#f],
changes the probability of @racket[first-coin]. Conditional on
@racket[both-heads] being @racket[#f], the probability @racket[first-coin] being
@racket[#t] is @racket[1/3].

@defproc[(flip [p (real-in 0 1)]) boolean?]{
  Returns a Boolean where @racket[#t] has probability @racket[p]
  and @racket[#f] has probability @racket[(- 1 p)].
  @examples[
    #:eval evaluator #:label #f
    (if (flip 1/2) 'a 'b)]
}

@defform[(query maybe-option body ...+)
         #:grammar
         [(maybe-option (code:line)
                        (code:line #:samples nat))]]{
  Returns the probability mass function (PMF) associated with @racket[e].
  In other words, @racket[query] performs top-level inference. See
  @racket[in-pmf] for an example of how to use the result of this
  function.
  @examples[
    #:eval evaluator #:label #f
    (query (flip 1/2))]
  Delimits observations to the dynamic extent of @racket[body]. After
  @racket[body] has finished, any observations executed during @racket[body]
  are forgotten.
    Runs the @racket[body] expression @racket[iter] times,
  producing a probabilistic value according to the sampling distribution.
  Note that observations are automatically delimited
  (using @racket[with-observe])
  inside of @racket[body]
  to prevent observations from leaking between samples.
}

@defproc[(observe! [e boolean?]) void?]{
  Conditions the current execution on @racket[e] evaluating to @racket[#t].
  @examples[
    #:eval evaluator #:label #f
    (define x (flip 1/2))
    (define y (flip 1/2))
    (and x y)
    (observe! x)
    (and x y)]
}

@defproc[(sample [e any/c]) any/c]{
  Samples a concrete value from the given probabilistic value.
  @examples[
    #:eval evaluator #:label #f
    (sample (flip 1/2))]
}

@defproc[(in-pmf [e pmf?]) stream?]{
  Sequence constructor for PMFs.
  @examples[
    #:eval evaluator #:label #f
    (define-syntax-rule (expectation e)
      (for/all ([pmf (query e)])
        (for/sum ([(val prob) (in-pmf pmf)])
	  (* val prob))))

    (expectation (if (flip 1/2) 5 10))]
}

@defproc[(pmf? [e any/c]) boolean?]{
  Predicate for PMFs.
}
