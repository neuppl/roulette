#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@(require (for-label "label.rkt")
	  racket/sandbox
	  scribble/example)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; constants

@(define evaluator (make-base-eval #:lang 'roulette))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Reference}

Roulette augments Racket with first-class @emph{measurable values}. A variable
can be bound to a measurable value of a given type and then be used in
computations, just as any ordinary concrete value of that type. An @emph{engine}
is then used to perform inference on derived values. The examples in this
section use the @secref{RSDD} backend.

@section{Measures}

@defform[(define-measurable id ...+ measure)
	 #:contracts
	 ([measure (measure/c dom cod)])]{
  Binds each provided identifier to a value of type @racket[(measurable-space-point dom)] with the given measure.
  The identifiers are bound to the same constants every time the form is evaluated.
  One way to think about @racket[define-measurable] is that it reflects a measure into the meta level.
  @examples[#:eval evaluator #:label #f
  (require roulette/engine/rsdd)
  (define-measurable x (bernoulli-measure 0.4 0.6))]
}

@defform[(define-measurable* id ...+ measure)
	 #:contracts
	 ([measure (measure/c dom cod)])]{
  Like @racket[define-measurable], but constructs a new value each time the form is evaluated.
}

@defproc[(infer [v (measurable-space-point dom)] [#:engine engine (engine/c dom cod) (rsdd-engine)])
	 (measure/c dom cod)]{
  Returns the measure associated with @racket[v] using @racket[engine].
  One way to think about @racket[infer] is that it reifies a measure from the meta level.
  If @racket[x] is defined by @racket[(define-measurable x m)],
  then @racket[(infer x)] should be equal to @racket[m].
  @examples[#:eval evaluator #:label #f
  (define m (infer (if x 'apple 'banana)))
  (m (set 'apple))]
}

@defproc[(support [m (measure/c dom cod)]) (or/c dom #f)]{
  If possible, returns the largest (measurable) set such that every open neighbourhood of every point in the set has positive measure.
  @examples[#:eval evaluator #:label #f
  (support (bernoulli-measure 0 1))
  (support m)]
}

@defproc[(density [m (measure/c dom cod)])
	 (-> (measurable-space-point dom) cod)]{
  If possible, returns the derivative of the given measure.
  @examples[#:eval evaluator #:label #f
  (define d (density m))
  (d 'apple)
  (d 'banana)]
}

@defproc[(measure/c [dom measurable-space?] [cod flat-contract?])
	 chaperone-contract?]{
  Contract for measures. A measure acts like functions from the measurable space @racket[dom] to a commutative monoid @racket[cod].
}

@defproc[(engine/c [dom measurable-space?] [cod flat-contract?])
	 chaperone-contract?]{
  Contract for engines that permits inference on measurable values.
}

@section{Measurable Spaces}

@defproc[(immutable-set/c [elem/c chaperone-contract?]) measurable-space?]{
  Equivalent to @racket[(set/c elem/c #:kind 'immutable)].
}

@defproc[(measurable-space-point [c measurable-space?]) predicate/c]{
  Returns a predicate for @emph{points} of the measurable space.
  @examples[#:eval evaluator #:label #f
  (measurable-space-point (immutable-set/c boolean?))]
}

@defproc[(measurable-space? [c any/c]) boolean?]{
  Returns whether @racket[c] is a measurable space. A measurable space acts like a contract that recognizes elements of a σ-algebra.
}
