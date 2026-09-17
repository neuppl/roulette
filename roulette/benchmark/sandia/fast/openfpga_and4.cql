

(def-netlist openfpga_and4.v
(hash 0 1 1 1 2 1 3 1)
(hash 4 1)
(hash)
(5 (<w= (bv-const 0 1)))
(6 (<w= (bv-const 1 1)))
(7 (<w= (bv-const 0 1)))
(8 (w& 1 0))
(9 (<w= 8))
(10 (w& 9 2))
(11 (w~ 10))
(12 (<w= 11))
(13 (w~ 12))
(14 (wc (list 13 3)))
(15 (ws (ws 14 (list 0)) (list 0)))
(16 (ws (ws 14 (list 1)) (list 0)))
(17 (w& 15 16))
(18 (<w= (ws 17 (list 0))))
(4 (<<= 18)))

(define internal-signals (hash 5  6  7  8  9  10  11  12  13  14  15  16  17  18 ))

(sketch-reroll openfpga_and4.v (loops ))

(define p-clean  0.99)
(define p-faulty 0.01)


(define expected-output (list #f))


