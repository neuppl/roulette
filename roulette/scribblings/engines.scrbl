#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@(require (for-label "label.rkt")
	  racket/sandbox
	  scribble/example)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; constants

@(define evaluator (make-base-eval #:lang 'roulette))
@examples[#:eval evaluator #:hidden
  (require roulette/engine/rsdd)]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Engines}

An engine provides a backend for performing inference. By default, Roulette
automatically installs the RSDD backend.

@section{RSDD}

@defmodule[roulette/engine/rsdd]

@defproc[(rsdd-engine [#:semiring s semiring? number-semiring])
	 (engine/c (immutable-set/c any/c) s)]{
  Performs inference over the @racket[s] semiring using RSDD.
  When an engine is garbage collected,
  all the memory associated with it
  (i.e., BDDs and weights)
  is freed.
  @examples[#:eval evaluator #:label #f
  (define poly-semiring (polynomial-semiring number-semiring))
  (define poly-engine (rsdd-engine #:semiring poly-semiring))]
}

@defproc[(bernoulli-measure [f s]
			    [t s]
			    [#:semiring s semiring? number-semiring])
	 (measure/c (immutable-set/c boolean?) s)]{
  Returns a measure such that
  @racket[(set)] gets @racket[(semiring-zero s)],
  @racket[(set #f)] gets @racket[f],
  @racket[(set #t)] gets @racket[t],
  and @racket[(set #f #t)] gets @racket[((semiring-plus s) f t)].
  @examples[#:eval evaluator #:label #f
    (define-measurable y
      (bernoulli-measure '(0.1 0.6) '(0.9 0.4) #:semiring poly-semiring))
    ((infer y #:engine poly-engine) (set #f #t))]
}

@deftogether[(@defproc[(semiring [pred predicate/c]
                                 [zero any/c] [add (-> any/c any/c any/c)]
                                 [one any/c] [mul (-> any/c any/c any/c)]) semiring?]
              @defproc[(semiring? [v any/c]) boolean?]
              @defproc[(semiring-zero [s semiring?]) any/c]
              @defproc[(semiring-add [s semiring?]) procedure?]
              @defproc[(semiring-one [s semiring?]) any/c]
              @defproc[(semiring-mul [s semiring?]) procedure?])]{
  Constructor, predicate, and accessors for semirings.
  Addition and multiplication are guaranteed to have variable arity.
}

@deftogether[(@defthing[boolean-semiring semiring?]
              @defthing[number-semiring semiring?]
              @defthing[complex-semiring semiring?]
              @defthing[log-semiring semiring?]
              @defthing[expectation-semiring semiring?])]{
  Base semirings.
}

@defproc[(polynomial-semiring [s semiring?]) semiring?]{
  Constructs a polynomial semiring where coefficients are members of @racket[s].
}
