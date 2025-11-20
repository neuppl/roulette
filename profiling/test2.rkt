#lang roulette/example/interrupt
(provide place-main)
(provide generate-json)



(define (make-bayesian-node dep1 dep2)
  (define node (if (and dep1 dep2) (flip 0.5) (flip 0.4)))
  node)


(define (n-grid-bayesian n)
  (for*/fold ([grid (list)])
             ([row (in-range 1 n 1)]
              [col (in-range 1 n 1)])
    (if (< (length grid) row)
        (append grid (list (list (make-bayesian-node
                                  #t 
                                  (if (= row 1)
                                      #t (first (list-ref grid (- row 2))))))))
        (for/list ([cur-row grid])
          (if (< (length cur-row) col)
              (append
               cur-row
               (list (make-bayesian-node
                      (list-ref     (list-ref grid (- row 1)) (- col 2))
                      (if (= row 1)
                          #t
                          (list-ref (list-ref grid (- row 2)) (- col 1)))
                      )))
              cur-row)))))


(define out (if (flip 0.5) #t (last (last (n-grid-bayesian 12)))))

(define (place-main pch)
  (query out pch))

(define (generate-json pch)
  (make-json-visualization out pch))
