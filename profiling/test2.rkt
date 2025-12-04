#lang roulette/example/interrupt
(provide place-main)
(provide generate-json)



(define (make-bayesian-node dep1 dep2 row col)
  (define label (string-append "row: " (number->string row) ", column: " (number->string col)))
  (define node (if (and dep1 dep2) 
                   (flip 0.5 #:label (string-append label "(true)")) 
                   (flip 0.4 #:label (string-append label "(false)"))))
  node)


(define (n-grid-bayesian n)
  (for*/fold ([grid (list)])
             ([row (in-range 1 n 1)]
              [col (in-range 1 n 1)])
    (if (< (length grid) row) ; create a new row and append (first node in a new row)
        (append grid (list (list (make-bayesian-node
                                  #t 
                                  (if (= row 1)
                                      #t (first (list-ref grid (- row 2))))
                                  row
                                  col))))
        (for/list ([cur-row grid])
          (if (< (length cur-row) col)
              (append
               cur-row
               (list (make-bayesian-node
                      (list-ref     (list-ref grid (- row 1)) (- col 2))
                      (if (= row 1)
                          #t
                          (list-ref (list-ref grid (- row 2)) (- col 1)))
                      row
                      col)))
              cur-row)))))


(define out (if (flip 0.5 #:label 'important-maybe) #t (last (last (n-grid-bayesian 11)))))

(define (place-main pch)
  (query out pch))

(define (generate-json pch)
  (make-json-visualization out pch))
