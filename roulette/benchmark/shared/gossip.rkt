#lang roulette/example/disrupt
(provide gossip)

;; Gossip broadcast on a complete graph, from Bayonet's gossip.bayonet
;; (Gehr et al., PLDI 2018, https://github.com/eth-sri/bayonet). Node 0
;; starts with the rumour; the query is whether the last node hears it.
;; `n` drives cost far harder than `rounds`.
(define (gossip n rounds)
  (define P-SPREAD 0.5)

  (define initial
    (for/list ([i (in-range n)]) (= i 0)))

  (define final
    (for/fold ([known initial]) ([_ (in-range rounds)])
      (for/list ([i (in-range n)])
        (for/fold ([acc (list-ref known i)])
                  ([j (in-range n)] #:unless (= i j))
          (|| acc (&& (list-ref known j) (flip P-SPREAD)))))))

  (last final))
