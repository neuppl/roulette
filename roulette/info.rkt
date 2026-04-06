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

(define test-omit-paths '("extra"))
(define compile-omit-paths '("extra"))

;; dependencies

(define deps
  '("base"
    "parser-tools-lib"
    "rackunit-lib"
    "roulette-lib"
    "text-table"))

(define implies
  '("roulette-lib"))

(define build-deps
  '("racket-doc"
    "rosette"
    "sandbox-lib"
    "scribble-lib"))
