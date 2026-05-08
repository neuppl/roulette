#lang roulette
(provide make-heuristics
         make-random-specialization-transition
         process-results
         top-results)




; Initial state: empty list, no variables substituted
; Transition: randomly select a set of n vars freshly to find the cost of
; Heuristic: number of times a variable occurs in non false samples that actually run. 
; (maybe some weightage for the actual number of recusive calls used: completing with fewer rec. calls 
; probably means the sampled variables are more important)



(define (make-random-specialization-transition vars num-vars)
    (lambda () (take (shuffle vars) num-vars)))



;; Accumulates a hash from results of the cost function in interrupt.rkt. 
; avg-map maps from variable to (list num-successful-samples total-samples)
(define (make-heuristics)
    (let ([acc-var-costs (make-hash)])
        (lambda (cost-map)
            ;(displayln cost-map)
            (if cost-map
                (begin
                    (for ([(env rec-calls) (in-hash cost-map)])
                        (for ([(var _) (in-hash env)])
                            (hash-update!
                                acc-var-costs 
                                var
                                (lambda (x) (list (if rec-calls 
                                                    (add1 (first x))
                                                    (first x)) 
                                                  (add1 (second x))))
                                (list 0 0)))
                        (hash-update!  acc-var-costs "Total-runs" add1 0))
                        
                    (hash-update!  acc-var-costs "Total-samples" add1 0))
                acc-var-costs))))



(define (top-results n variable-contexts acc-var-costs)
    (let ([ratios (for/hash ([(var stats) (in-hash acc-var-costs)]
                             #:when (list? stats))
                    (values var (/ (first stats) (second stats))))])
      (let ([sorted (sort (hash-keys ratios) > #:key (lambda (k) (hash-ref ratios k)))])
        (map (lambda (k) (list (third (hash-ref variable-contexts k)) (hash-ref ratios k)))
             (take sorted (min n (length sorted)))))))

(define (process-results acc-var-costs) 
    (let ([ratios (for/hash ([(var stats) (in-hash acc-var-costs)]
                             #:when (list? stats))
                    (values var (/ (first stats) (second stats))))])
      (sort (hash-keys ratios) > #:key (lambda (k) (hash-ref ratios k)))))