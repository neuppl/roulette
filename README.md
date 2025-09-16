# Roulette

[![Build Status][build-badge]][build]
[![Scribble][docs-badge]][docs]

Roulette is an inference-aided host language, built on top of Rosette, that
provides first-class support for measurable values. The programmer-facing
interface to Roulette is nearly the same as Rosette, except values can be
associated with measures. Instead of an SMT solver for the backend, Roulette
uses specialized solvers intended for inference. See [the documentation][docs]
for information on how to install and use the software.

Benchmarks for Roulette exist in the roulette/benchmark directory.
Benchmark data and summaries can be found in [this](https://github.com/neuppl/roulette-bench-data) repository.

[build-badge]: https://github.com/camoy/roulette/actions/workflows/build.yml/badge.svg
[build]: https://github.com/camoy/roulette/actions/workflows/build.yml?query=workflow%3Abuild
[docs-badge]: https://img.shields.io/badge/Docs-Scribble-blue.svg
[docs]: https://docs.racket-lang.org/roulette
[benchmarks]: https://neuppl.github.io/roulette-bench-data/
