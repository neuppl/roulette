#lang roulette/example/interrupt

(define a1 (flip 0.8))
(define a2 (flip 0.8))
(define a3 (flip 0.8))
(define a4 (flip 0.8))
(define a5 (flip 0.8))
(define a6 (flip 0.8))
(define a7 (flip 0.8))
(define a8 (flip 0.8))
(define a9 (flip 0.8))
(define a10 (flip 0.8))
(define a11 (flip 0.8))
(define a12 (flip 0.8))
(define a13 (flip 0.8))
(define a14 (flip 0.8))
(define a15 (flip 0.8))
(define a16 (flip 0.8))
(define a17 (flip 0.8))
(define a18 (flip 0.8))
(define a19 (flip 0.8))
(define a20 (flip 0.8))
(define a21 (flip 0.8))
(define a22 (flip 0.8))
(define a23 (flip 0.8))
(define a24 (flip 0.8))
(define a25 (flip 0.8))
(define a26 (flip 0.8))
(define a27 (flip 0.8))
(define a28 (flip 0.8))
(define a29 (flip 0.8))
(define a30 (flip 0.8))



(define out (and a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23
                 a24 a25 a26 a27 a28 a29 a30))



(let ([entry (read)])
  (cond
    [(equal? entry "generate-json") (make-json-visualization out)]
    [(equal? entry "profiler-run") (query out)]
    [else (error "unrecognized entry point ~v" entry)]))



