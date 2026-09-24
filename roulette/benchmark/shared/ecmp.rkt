#lang roulette/example/disrupt
(provide ecmp-reliability ecmp-reliability/interleaved ecmp-diagnosis)

;; ECMP routing with link failures, from Bayonet's reliability.bayonet and
;; reliability-observe.bayonet (Gehr et al., PLDI 2018,
;; https://github.com/eth-sri/bayonet). Bayonet's topology is a diamond
;; H0 -> S0 -> {S1, S2} -> S3 -> H1; we chain `n` of them.

(define P-FAIL 0.01)  ; per-switch probability of being down
(define P-UPPER 0.5)  ; ECMP split

;; Switch state is drawn once and persists across packets, as Bayonet's
;; `state failing(2)` does.
(define (make-fabric n)
  (for/list ([_ (in-range n)])
    (cons (! (flip P-FAIL)) (! (flip P-FAIL)))))

(define (deliver fabric)
  (for/fold ([alive #t]) ([diamond (in-list fabric)])
    (&& alive (if (flip P-UPPER) (car diamond) (cdr diamond)))))

;; Exponential in `n`: declaring the whole fabric before any routing puts
;; every failure variable ahead of every routing variable. 383,801 apply
;; calls at n=10, against 309 for the interleaved form below.
(define (ecmp-reliability n)
  (deliver (make-fabric n)))

;; Same distribution, each diamond's variables created together. Valid only
;; for a single packet, which looks at each switch once.
(define (ecmp-reliability/interleaved n)
  (for/fold ([alive #t]) ([_ (in-range n)])
    (&& alive (if (flip P-UPPER) (! (flip P-FAIL)) (! (flip P-FAIL))))))

;; Bayonet's reliability-observe: `k` packets share one fabric but re-roll
;; their routes, none arrive, and we ask whether the first upper switch is
;; the one that is down.
(define (ecmp-diagnosis n k)
  (define fabric (make-fabric n))
  (for ([_ (in-range k)])
    (observe! (! (deliver fabric))))
  (! (car (first fabric))))
