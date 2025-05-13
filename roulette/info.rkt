#lang info

;; general

(define name "roulette")
(define collection "roulette")
(define pkg-desc "Full implementation of Roulette.")
(define version "0.0")
(define pkg-authors '(camoy))
(define license 'Apache-2.0)
(define scribblings
  '(["scribblings/roulette.scrbl" (multi-page)]))

;; dependencies

(define deps
  '("base"
    "rackunit-lib"
    "roulette-lib"))

(define implies
  '("roulette-lib"))

(define build-deps
  '("racket-doc"
    "rosette"
    "sandbox-lib"
    "scribble-lib"))
