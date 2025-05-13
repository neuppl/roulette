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

@defproc[(rsdd-engine [#:semiring s semiring? real-semiring])
	 (engine/c (immutable-set/c any/c) s)]{
  Performs inference over the @racket[s] semiring using RSDD.
  @examples[#:eval evaluator #:label #f
  (define engine (rsdd-engine #:semiring complex-semiring))]
}

@defproc[(bernoulli-measure [f s]
			    [t s]
			    [#:semiring s semiring? real-semiring])
	 (measure/c (immutable-set/c boolean?) s)]{
  Returns a measure such that
  @racket[(set)] gets @racket[(semiring-zero s)],
  @racket[(set #f)] gets @racket[f],
  @racket[(set #t)] gets @racket[t],
  and @racket[(set #f #t)] gets @racket[((semiring-plus s) f t)].
  @examples[#:eval evaluator #:label #f
  (define-measurable x
    (bernoulli-measure 0+i 1 #:semiring complex-semiring))
  ((infer x #:engine engine) (set #f #t))]
}

@defproc[(semiring? [v any/c]) boolean?]{
  Recognizes a semiring instance. A semiring acts like a contract that recognizes elements of the semiring.
}

@deftogether[(@defthing[real-semiring semiring?]
              @defthing[complex-semiring semiring?])]{
  Semirings that can be used with RSDD.
}
