

(def-netlist jsrflipflop.v
(hash 0 1 1 2)
(hash 2 1 3 1)
(hash 4 1)
(5 (<w= (ws 1 (list 0))))
(6 (<w= (ws 1 (list 1))))
(7 (<w= 4))
(8 (<w= (bv-const 0 1)))
(9 (<w= (bv-const 1 1)))
(10 (<w= (bv-const 0 1)))
(11 (w|| 5 6))
(12 (<w= 11))
(13 (w~ 5))
(14 (wc (list 13 6)))
(15 (ws (ws 14 (list 0)) (list 0)))
(16 (ws (ws 14 (list 1)) (list 0)))
(17 (w& 15 16))
(18 (<w= (ws 17 (list 0))))
(19 (w~ 7))
(20 (<w= 19))
(21 (wx 12 4 18))
(22 (wx 0 21 (bv-const 0 1)))
(4 (<<= 22))
(2 (<<= 20))
(3 (<<= 7)))

(define internal-signals (hash 5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22 ))

(sketch-reroll jsrflipflop.v (loops ))

(define p-clean  0.99)
(define p-faulty 0.01)


(define expected-output (list #f #f #f))


