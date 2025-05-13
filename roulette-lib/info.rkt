#lang info

;; general

(define name "roulette")
(define collection "roulette")
(define pkg-desc "Core implementation of Roulette (no tests or documentation).")
(define version "0.0")
(define pkg-authors '(camoy))
(define license 'Apache-2.0)

;; dependencies

(define deps
  `(["roulette-x86_64-linux" #:platform #rx"^x86_64-linux(?:-natipkg)?$"
                             #:version ,version]
    ["roulette-aarch64-macosx" #:platform "aarch64-macosx"
                               #:version ,version]
    ["roulette-x86_64-macosx" #:platform "x86_64-macosx"
                              #:version ,version]
    ["roulette-x86_64-win32" #:platform "win32\\x86_64"
                             #:version ,version]
    "base"
    "ddict"
    "gui-lib"
    "pict-lib"
    "rosette"))

(define build-deps '())
