#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Roulette}
@author{Cameron Moy}
@defmodule[roulette #:lang]

Roulette is an inference-aided host language, built on top of Rosette, that
provides first-class support for @emph{measurable values}. The programmer-facing
interface to Roulette is nearly the same as Rosette, except values can be
associated with measures. Instead of an SMT solver for the backend, Roulette
uses specialized solvers intended for inference.

Roulette is available on Racket's package server. There are two ways to install it.

@itemize[
@item{From DrRacket you can install Roulette by going to @exec{File}, selecting
  @exec{Install Package}, entering @exec{roulette} into the package source field,
  and hitting @exec{Update}.}

@item{From the command line you can install Roulette through @exec{raco}:
@verbatim|{
$ raco pkg install roulette
}|}]

@bold{Roulette is currently under active development so expect frequent
backwards incompatible changes.}

@(table-of-contents)

@include-section["examples.scrbl"]
@include-section["reference.scrbl"]
@include-section["engines.scrbl"]
