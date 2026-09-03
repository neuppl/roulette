#lang roulette/example/disrupt
;; Standalone timing benchmark and plot.
;; Runs all benchmarks, aggregates the timing breakdown, and saves a
;; stacked horizontal bar chart to probalog-timing.png.
;;
;; Uses plot/no-gui so no DrRacket window is needed -- the chart is
;; written directly to disk and can be run as a plain script:
;;   racket probalog-timing-plot.rkt
(require roulette/example/probalog/probalog-core
         roulette/example/probalog/probalog-set-equal
         plot/no-gui)
(provide probalog-timing-split-plot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program generators

(define (make-layered-dag layers width [edge-prob 0.9])
  (define (node l i) (format "L~a_~a" l i))
  (define facts
    (append
     (for/list ([j (in-range width)])
       (cons (fact 'Edge (list "SRC" (node 0 j))) edge-prob))
     (for*/list ([i (in-range (sub1 layers))]
                 [j (in-range width)]
                 [k (in-range width)])
       (cons (fact 'Edge (list (node i j) (node (add1 i) k))) edge-prob))
     (for/list ([j (in-range width)])
       (cons (fact 'Edge (list (node (sub1 layers) j) "SINK")) edge-prob))))
  (define rules
    (list (rule (fact 'Path (list 'x 'y))
                (list (fact 'Edge (list 'x 'y))))
          (rule (fact 'Path (list 'x 'z))
                (list (fact 'Path (list 'x 'y))
                      (fact 'Edge (list 'y 'z))))))
  (values facts rules))

(define (make-family depth [parent-prob 0.95])
  (define (person i) (format "P~a" i))
  (define size (sub1 (expt 2 depth)))
  (define facts
    (append
     (for*/list ([i (in-range size)]
                 [c (in-list (list (+ (* 2 i) 1) (+ (* 2 i) 2)))]
                 #:when (< c size))
       (cons (fact 'Parent (list (person i) (person c))) parent-prob))
     (for/list ([i (in-range size)])
       (cons (fact (if (even? i) 'Male 'Female) (list (person i))) 1))))
  (define rules
    (list (rule (fact 'Ancestor (list 'x 'y))
                (list (fact 'Parent (list 'x 'y))))
          (rule (fact 'Ancestor (list 'x 'z))
                (list (fact 'Ancestor (list 'x 'y))
                      (fact 'Parent (list 'y 'z))))
          (rule (fact 'Grandfather (list 'x 'y))
                (list (fact 'Parent (list 'x 'z))
                      (fact 'Parent (list 'z 'y))
                      (fact 'Male (list 'x))))
          (rule (fact 'Grandmother (list 'x 'y))
                (list (fact 'Parent (list 'x 'z))
                      (fact 'Parent (list 'z 'y))
                      (fact 'Female (list 'x))))
          (rule (fact 'Son (list 'x 'y))
                (list (fact 'Parent (list 'y 'x))
                      (fact 'Male (list 'x))))
          (rule (fact 'Daughter (list 'x 'y))
                (list (fact 'Parent (list 'y 'x))
                      (fact 'Female (list 'x))))))
  (values facts rules))

(define (make-supply-chain layers width)
  (define (pkg l i) (format "pkg~a_~a" l i))
  (define facts
    (append
     (for*/list ([l (in-range (sub1 layers))]
                 [i (in-range width)]
                 [d (in-list (list i (modulo (add1 i) width)))])
       (cons (fact 'DependsOn (list (pkg l i) (pkg (add1 l) d))) 0.9))
     (for/list ([i (in-range width)])
       (cons (fact 'HasCVE (list (pkg (sub1 layers) i))) (if (even? i) 0.3 0.15)))
     (for*/list ([l (in-range layers)]
                 [i (in-range width)])
       (cons (fact 'Unmaintained (list (pkg l i))) 0.2))))
  (define rules
    (list (rule (fact 'Vulnerable (list 'p))
                (list (fact 'HasCVE (list 'p))))
          (rule (fact 'Vulnerable (list 'p))
                (list (fact 'DependsOn (list 'p 'q))
                      (fact 'Vulnerable (list 'q))))
          (rule (fact 'NeedsAudit (list 'p))
                (list (fact 'Vulnerable (list 'p))
                      (fact 'Unmaintained (list 'p))))))
  (values facts rules))

(define (make-cyclic-ring n [chord 3] [edge-prob 0.85])
  (define (node i) (format "N~a" i))
  (define facts
    (append
     (for/list ([i (in-range n)])
       (cons (fact 'Edge (list (node i) (node (modulo (add1 i) n)))) edge-prob))
     (for/list ([i (in-range n)])
       (cons (fact 'Edge (list (node i) (node (modulo (+ i chord) n)))) edge-prob))))
  (define rules
    (list (rule (fact 'Reach (list 'x 'y))
                (list (fact 'Edge (list 'x 'y))))
          (rule (fact 'Reach (list 'x 'z))
                (list (fact 'Reach (list 'x 'y))
                      (fact 'Edge (list 'y 'z))))))
  (values facts rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarks

(define benchmarks
  (list
   (list "layered-dag"
         (lambda () (make-layered-dag 6 3))
         (fact 'Path (list "SRC" "SINK")))
   (list "cyclic-ring"
         (lambda () (make-cyclic-ring 9 3))
         (fact 'Reach (list "N0" "N5")))
   (list "family"
         (lambda () (make-family 7))
         (fact 'Ancestor (list "P0" "P100")))
   (list "supply-chain"
         (lambda () (make-supply-chain 12 10))
         (fact 'NeedsAudit (list "pkg0_0")))))

(define (run-benchmark name make-program query-target)
  (define-values (facts rules) (make-program))
  (define equal-before    total-my-hash-equal?-time)
  (define bindings-before total-find-bindings-time)
  (define guard-before    total-guard-build-time)
  (define union-before    total-set-union-time)
  (define index-before    total-index-time)
  (define wall-start (current-inexact-monotonic-milliseconds))
  (define result (run-datalog facts rules))
  (define wall (- (current-inexact-monotonic-milliseconds) wall-start))
  (printf "~a: ~a facts, ~a rules, ~ams -- ~a: ~a\n"
          name (length facts) (length rules) (~r wall #:precision 0)
          query-target (query-fact result query-target))
  (flush-output)
  (list name wall
        (- total-my-hash-equal?-time   equal-before)
        (- total-find-bindings-time    bindings-before)
        (- total-guard-build-time      guard-before)
        (- total-set-union-time        union-before)
        (- total-index-time            index-before)))

(define (aggregate-timing results)
  (for/fold ([wall 0] [eq 0] [bind 0] [guard 0] [union 0] [idx 0])
            ([r results])
    (values (+ wall  (list-ref r 1))
            (+ eq    (list-ref r 2))
            (+ bind  (list-ref r 3))
            (+ guard (list-ref r 4))
            (+ union (list-ref r 5))
            (+ idx   (list-ref r 6)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run benchmarks and save chart

(define results
  (for/list ([b benchmarks])
    (run-benchmark (car b) (cadr b) (caddr b))))

(define-values (wall eq bind guard union idx) (aggregate-timing results))

(define named
  (list (cons "find-bindings"    bind)
        (cons "set-add"          guard)
        (cons "my-hash-equal?"   eq)
        (cons "index construction" idx)
        (cons "set-union"        union)))

(define parts
  (sort (append named
                (list (cons "other" (max 0 (- wall (apply + (map cdr named)))))))
        > #:key cdr))

(define total (apply + (map cdr parts)))

(define probalog-timing-split-plot (plot-pict
 (stacked-histogram
  (list (vector "" (map cdr parts)))
  #:invert? #t
  #:labels (for/list ([p parts])
             (format "~a (~a%)"
                     (car p)
                     (~r (* 100 (/ (cdr p) total)) #:precision 1))))
 #:title (format "timing split across ~a benchmarks (~ams total)"
                 (length results) (~r wall #:precision 0))
 #:x-label "time (ms)"
 #:y-label #f
 #:legend-anchor 'outside-right-top
 #:width 800
 #:height 300))

 probalog-timing-split-plot