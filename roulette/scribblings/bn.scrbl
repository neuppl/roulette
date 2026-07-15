#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@(require (for-label (only-in roulette/example/disrupt observe!)
                     (only-in racket/base equal? require))
          racket/sandbox
	  scribble/example)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; constants

@(define evaluator (make-base-eval #:lang 'roulette/example/disrupt))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Bayesian Network}
@defmodule*[(roulette/example/bn) #:lang]

A Bayesian network is a common kind of probabilistic graphical model
that expresses conditional dependencies between variables.
These networks are often described in the @hyperlink[BIF]{Bayesian Interchange Format} (BIF).
There are many such networks freely available online,
including in the @hyperlink[BNL]{BNLearn} repository.
The BN language interprets Bayesian networks written in the BIF.

@filebox["cancer.rkt"]{@verbatim[#<<END
#lang roulette/example/bn

variable Pollution { type discrete [ 3 ] { low, medium, high }; }
variable Smoker { type discrete [ 2 ] { True, False }; }
variable Cancer { type discrete [ 2 ] { True, False }; }
variable Xray { type discrete [ 2 ] { positive, negative }; }
variable Dyspnoea { type discrete [ 2 ] { True, False }; }

probability ( Pollution ) {
  table 0.5, 0.4, 0.1;
}
probability ( Smoker ) {
  table 0.3, 0.7;
}
probability ( Cancer | Pollution, Smoker ) {
  (low, True) 0.03, 0.97;
  (medium, True) 0.03, 0.97;
  (high, True) 0.05, 0.95;
  (low, False) 0.001, 0.999;
  (medium, False) 0.001, 0.999;
  (high, False) 0.02, 0.98;
}
probability ( Xray | Cancer ) {
  (True) 0.9, 0.1;
  (False) 0.2, 0.8;
}
probability ( Dyspnoea | Cancer ) {
  (True) 0.65, 0.35;
  (False) 0.3, 0.7;
}
END
]}

This Bayesian network is the famous "cancer" example
that describes the relationship between several variables
and whether a patient has lung cancer.
Running this file in DrRacket allows one to explore the network
with all the constructs from Disrupt
in the interactions area.
Additionally,
the module exports all the variables,
making them available to Disrupt programs via @racket[require].

@examples[#:eval evaluator #:hidden (require roulette/test/bn/cancer)]
@examples[#:eval evaluator #:label #f
  Cancer
  (observe! (equal? Xray 'positive))
  Cancer]

This interaction shows the posterior probability that a patient has lung cancer
given that they have a positive X-ray result.
Notice that the posterior probability of @racket[Cancer]
only marginally increases with a positive test result.
Many people are surprised by this phenomenon,
which is known as the @emph{base rate fallacy}.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; links

@(define BIF "http://sites.poli.usp.br/p/fabio.cozman/Research/InterchangeFormat/xmlbif02.html")
@(define BNL "https://www.bnlearn.com")
