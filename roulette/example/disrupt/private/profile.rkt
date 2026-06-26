#lang roulette
(provide make-heuristics
         make-random-specialization-transition
         variable-labels
				 var-value)




; Initial state: empty list, no variables substituted
; Transition: randomly select a set of n vars freshly to find the cost of
; Heuristic: number of times a variable occurs in non false samples that actually run. 
; (maybe some weightage for the actual number of recusive calls used: completing with fewer rec. calls 
; probably means the sampled variables are more important)



(define (make-random-specialization-transition vars num-vars)
    (lambda () (take (shuffle vars) num-vars)))



;; Accumulates a hash from results of the cost function in interface.rkt. 
; acc-var-costs maps from variable to (list num-successful-samples total-samples total-num-recursive-calls)
; the total recursive calls count is only for samples that run within the timeout.
(define (make-heuristics config-data)
    (let ([acc-var-costs (make-hash (list (cons "config" config-data)))])
        (lambda (cost-map)
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
                                                  (add1 (second x))
                                                  (if rec-calls
                                                    (+ rec-calls (third x))
                                                    (third x))))
                                (list 0 0 0)))
                        (hash-update!  acc-var-costs "Total-runs" add1 0))
                    (hash-update!  acc-var-costs "Collected-samples" add1 0))
                acc-var-costs))))


(define (var-value stats) 
    (let ([num-successful-samples (first stats)]
					[num-total-samples (second stats)]
					[total-rec-calls (third stats)])
			(if (= num-total-samples 0)
					0
					(exact->inexact (/ num-successful-samples num-total-samples)))))

(define (variable-labels var-label-map acc-var-costs)
    (let* ([ratios (for/hash ([(var stats) (in-hash acc-var-costs)]
                              #:when (list? stats))
                              (values var (var-value stats)))]
           [sorted (sort (hash-keys ratios) > #:key (lambda (k) (hash-ref ratios k)))])
        (map (lambda (k) (hash-ref var-label-map k))           
             sorted)))