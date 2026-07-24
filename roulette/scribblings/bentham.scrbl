#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@(require (for-label "label.rkt"
		     (only-in roulette/example/bentham
			      flip
			      expected-utility
			      observe!
                              reward!))
	  racket/sandbox
	  scribble/example)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; constants

@(define evaluator (make-base-eval #:lang 'roulette/example/bentham))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Bentham}
@defmodule*[(roulette/example/bentham) #:lang]

Bentham is a more restrictive version of Disrupt
with support for calculating expected utility.
@examples[#:eval evaluator #:label #f
  (define x (flip 1/2))
  (define y (flip 1/2))
  (when x (reward! 99))
  (expected-utility (not (and x y)))]
If @racket[x] holds,
then the program accumulates @racket[99] in utility
via the @racket[reward!] procedure.
The @racket[query] function returns the expected utility
of the program conditional on @racket[(not (and x y))].
From earlier,
we know that the conditional probability of @racket[x]
given this expression is @racket[1/3].
Therefore,
the expected utility is @racket[33].

@deftogether[(@defproc[(flip [p (real-in 0 1)]) boolean?]
              @defproc[(observe! [e boolean?]) void?])]{
  These procedures have the same meaning
  as they do in Disrupt.
}

@defproc[(reward! [e number?]) void?]{
  Adds @racket[e] to the amount of utility accumulated
  by the current program execution.
}

@defproc[(expected-utility [e any/c]) number?]{
  Returns the expected utility given @racket[e].
}
