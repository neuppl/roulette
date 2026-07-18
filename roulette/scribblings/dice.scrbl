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

@title{Dice}
@defmodule*[(roulette/example/dice) #:lang]

@hyperlink[DICE]{Dice} is a small
discrete probabilistic programming language.
It was the first functional language
to use knowledge-compilation-based probabilistic inference.

@filebox["noisy-or.rkt"]{@verbatim[#<<END
#lang roulette/example/dice

let n0  = flip 0.5 in
let n4  = flip 0.5 in
let n1  = if n0 then flip 0.8 else flip 0.1 in
let n21  = if n0 then flip 0.8 else flip 0.1 in
let n22  = if n4 then flip 0.8 else flip 0.1 in
let n33  = if n4 then flip 0.8 else flip 0.1 in
let n2  = (n21 || n22) in
let n31  = if n1 then flip 0.8 else flip 0.1 in
let n32  = if n2 then flip 0.8 else flip 0.1 in
let n3  = (n31  || (n32 || n33)) in
n3
END
]}

Most programs should run out of the box,
but there are a few limitations.
A small number of operations are not supported.
The implementation does not perform typechecking,
so evaluating an ill-typed program is undefined behavior.
Functions are not compiled modularly,
and evaluation follows the "eager" strategy.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; links

@(define DICE "http://dicelang.cs.ucla.edu/")
