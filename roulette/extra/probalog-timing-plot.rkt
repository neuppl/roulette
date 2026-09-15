#lang roulette/example/disrupt
;; Standalone timing benchmark and plot: runs six programs, aggregates
;; where saturation spends its time, and renders a stacked bar. Uses
;; plot/no-gui, so `racket probalog-timing-plot.rkt` works as a script.
;;
;; The six press on different parts of the engine so none dominates:
;;
;;   ring        long sparse cycle          smokers   recursion, 3 clauses
;;   chordring   dense cycle, re-derived    pointsto  mutual recursion
;;   dag         wide converging, acyclic   sg        recursive atom in middle
;;
;; Only saturation is timed, so `dag` is a smaller slice: its cost is in
;; the query, not saturation. Cost grows steeply in the size parameters,
;; so re-calibrate rather than nudging blindly.
(require roulette/example/probalog/probalog-core
         roulette/example/probalog/probalog-set-equal
         plot/no-gui)
(provide probalog-timing-split-plot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program generators
;;
;; Each returns (values facts rules query-fact), where facts is a list
;; of (cons fact probability).

(define reach-rules
  (list (rule (fact 'Reach (list 'x 'y))
              (list (fact 'Edge (list 'x 'y))))
        (rule (fact 'Reach (list 'x 'z))
              (list (fact 'Reach (list 'x 'y))
                    (fact 'Edge (list 'y 'z))))))

;; A plain directed cycle. Every pair is reachable, so a handful of
;; facts yield a relation of n^2 tuples.
(define (make-ring n [edge-prob 0.9])
  (values (for/list ([i (in-range n)])
            (cons (fact 'Edge (list i (modulo (add1 i) n))) edge-prob))
          reach-rules
          (fact 'Reach (list 0 (quotient n 2)))))

;; The same cycle with chords to the node `chord` ahead, so each fact is
;; re-derived by many different routes.
(define (make-chord-ring n [chord 3] [edge-prob 0.85])
  (values (append
           (for/list ([i (in-range n)])
             (cons (fact 'Edge (list i (modulo (add1 i) n))) edge-prob))
           (for/list ([i (in-range n)])
             (cons (fact 'Edge (list i (modulo (+ i chord) n))) edge-prob)))
          reach-rules
          (fact 'Reach (list 0 (quotient n 2)))))

;; SRC -> L0_* -> ... -> SINK, fully connected between adjacent layers.
;; Every SRC->SINK path is the same length and there are width^layers of
;; them, so this is many derivations converging on one conclusion.
(define (make-layered-dag layers width [edge-prob 0.9])
  (define (node l i) (format "L~a_~a" l i))
  (values (append
           (for/list ([j (in-range width)])
             (cons (fact 'Edge (list "SRC" (node 0 j))) edge-prob))
           (for*/list ([l (in-range (sub1 layers))]
                       [a (in-range width)]
                       [b (in-range width)])
             (cons (fact 'Edge (list (node l a) (node (add1 l) b))) edge-prob))
           (for/list ([j (in-range width)])
             (cons (fact 'Edge (list (node (sub1 layers) j) "SINK")) edge-prob)))
          (list (rule (fact 'Path (list 'x 'y))
                      (list (fact 'Edge (list 'x 'y))))
                (rule (fact 'Path (list 'x 'z))
                      (list (fact 'Path (list 'x 'y))
                            (fact 'Edge (list 'y 'z)))))
          (fact 'Path (list "SRC" "SINK"))))

;; Friends and smokers on a friendship ring: recursion around a cycle,
;; through a rule with three body clauses.
(define (make-smokers n [stress 0.2] [influence 0.3])
  (define (p i) (format "P~a" i))
  (values (append
           (for/list ([i (in-range n)])
             (cons (fact 'Stress (list (p i))) stress))
           (for/list ([i (in-range n)])
             (cons (fact 'Influences (list (p i) (p (modulo (add1 i) n)))) influence))
           (for/list ([i (in-range n)])
             (cons (fact 'Influences (list (p (modulo (add1 i) n)) (p i))) influence))
           (for/list ([i (in-range n)])
             (cons (fact 'Friend (list (p i) (p (modulo (add1 i) n)))) 1))
           (for/list ([i (in-range n)])
             (cons (fact 'Friend (list (p (modulo (add1 i) n)) (p i))) 1)))
          (list (rule (fact 'Smokes (list 'x))
                      (list (fact 'Stress (list 'x))))
                (rule (fact 'Smokes (list 'x))
                      (list (fact 'Friend (list 'x 'y))
                            (fact 'Smokes (list 'y))
                            (fact 'Influences (list 'y 'x)))))
          (fact 'Smokes (list (p 0)))))

;; Andersen's points-to analysis over a chain of assignments and field
;; accesses. VarPointsTo and FieldPointsTo depend on each other, so this
;; is mutual recursion across predicates rather than one self-recursive
;; predicate.
(define (make-points-to n)
  (define (v i) (format "v~a" i))
  (define (w i) (format "w~a" i))
  (define (u i) (format "u~a" i))
  (define (o i) (format "o~a" i))
  (values (cons (cons (fact 'AddressOf (list (v 0) (o 0))) 1)
                (for*/list ([i (in-range n)]
                            [f (in-list
                                (list (cons (fact 'Assign (list (v (add1 i)) (v i))) 0.9)
                                      (cons (fact 'AddressOf (list (w i) (o (add1 i)))) 0.8)
                                      (cons (fact 'Store (list (v i) "f" (w i))) 0.9)
                                      (cons (fact 'Load (list (u i) (v (add1 i)) "f")) 0.9)))])
                  f))
          (list (rule (fact 'VarPointsTo (list 'v 'o))
                      (list (fact 'AddressOf (list 'v 'o))))
                (rule (fact 'VarPointsTo (list 'v 'o))
                      (list (fact 'Assign (list 'v 'u))
                            (fact 'VarPointsTo (list 'u 'o))))
                (rule (fact 'VarPointsTo (list 'v 'o))
                      (list (fact 'Load (list 'v 'u 'f))
                            (fact 'VarPointsTo (list 'u 'b))
                            (fact 'FieldPointsTo (list 'b 'f 'o))))
                (rule (fact 'FieldPointsTo (list 'b 'f 'o))
                      (list (fact 'Store (list 'u 'f 'v))
                            (fact 'VarPointsTo (list 'u 'b))
                            (fact 'VarPointsTo (list 'v 'o)))))
          (fact 'VarPointsTo (list (v n) (o 0)))))

;; Same generation over a balanced binary tree. The recursive atom is in
;; the middle of the body, so bindings arrive from `Up` on the left and
;; have to be pushed back out through `Down` on the right.
(define (make-same-generation depth [up-prob 0.9])
  (define nodes
    (let loop ([frontier (list "r")] [all (list "r")] [d 0])
      (if (>= d depth)
          all
          (let ([next (for*/list ([p (in-list frontier)] [k (in-range 2)])
                        (format "~a~a" p k))])
            (loop next (append all next) (add1 d))))))
  (define (parent-of c) (substring c 0 (sub1 (string-length c))))
  (define deepest
    (for/list ([c (in-list nodes)] #:when (= (string-length c) (add1 depth))) c))
  (values (append
           (for/list ([c (in-list nodes)] #:unless (equal? c "r"))
             (cons (fact 'Up (list c (parent-of c))) up-prob))
           (list (cons (fact 'Flat (list "r" "r")) 1)))
          (list (rule (fact 'Down (list 'p 'c))
                      (list (fact 'Up (list 'c 'p))))
                (rule (fact 'SameGen (list 'x 'y))
                      (list (fact 'Flat (list 'x 'y))))
                (rule (fact 'SameGen (list 'x 'y))
                      (list (fact 'Up (list 'x 'z1))
                            (fact 'SameGen (list 'z1 'z2))
                            (fact 'Down (list 'z2 'y)))))
          (fact 'SameGen (list (car deepest) (last deepest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarks
;;
;; Sizes calibrated so each program takes roughly a second.

(define benchmarks
  (list (list "ring"       (lambda () (make-ring 70)))
        (list "chordring"  (lambda () (make-chord-ring 18)))
        (list "dag"        (lambda () (make-layered-dag 7 5)))
        (list "smokers"    (lambda () (make-smokers 70)))
        (list "pointsto"   (lambda () (make-points-to 70)))
        (list "sg"         (lambda () (make-same-generation 7)))))

;; Timings come from subtracting the exported accumulators before and
;; after. Read-only access across modules is fine; set!-ing an imported
;; variable is not, which is why we subtract rather than reset.
(define (run-benchmark name make-program)
  (define-values (facts rules query-target) (make-program))
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
;; Run benchmarks and build the chart

(define results
  (for/list ([b benchmarks])
    (displayln (car b))
    (run-benchmark (car b) (cadr b))))

(define-values (wall eq bind guard union idx) (aggregate-timing results))

;; Each program's share of the total, so it is visible at a glance
;; whether any one of them is dictating the split.
(printf "\nshare of total per benchmark:\n")
(for ([r results])
  (printf "  ~a ~a%\n"
          (~a (car r) #:width 12)
          (~r (* 100 (/ (list-ref r 1) wall)) #:precision 1)))

(define named
  (list (cons "find-bindings"      bind)
        (cons "set-add"            guard)
        (cons "my-hash-equal?"     eq)
        (cons "index construction" idx)
        (cons "set-union"          union)))

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
