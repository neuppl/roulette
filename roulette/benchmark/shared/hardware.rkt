#lang roulette/example/disrupt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitvector

(require rosette/lib/destruct
         "../benchmarking.rkt")

(provide hardware)

(define (natural->bv k size)
  (cond
    [(zero? size) '()]
    [else (cons (odd? k) (natural->bv (truncate (/ k 2)) (sub1 size)))]))

(define (int->bv i size)
  (define max-int (sub1 (expt 2 size)))
  (cond
    [(> i max-int) (int->bv max-int size)]
    [else (natural->bv i size)]))

(define (bv->natural xs)
  (destruct xs
    [(list) 0]
    [(cons x xt) (+ (if x 1 0) (* 2 (bv->natural xt)))]))

(define (uniform-bv size)
  (for/list ([_ (in-range size)])
    (flip 1/2)))

(define (bveq x y)
  (andmap equal? x y))

(define (bvadd xs ys)
  (let go ([xs xs] [ys ys] [cin #f])
    (match* (xs ys)
      [('() '()) '()]
      [((cons x xt) (cons y yt))
       (define x⊕y (xor x y))
       (cons (xor x⊕y cin) (go xt yt (or (and x y) (and cin x⊕y))))])))

(define (bv-neg1 size) (build-list size (λ (_) #t)))
(define (bv-zero size) (build-list size (λ (_) #f)))
(define (bv-nonzero? xs) (ormap (λ (x) x) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp

;; expr = memory -> bitvector
;; comm = fuel memory -> memory + #f

;; expr -> expr
(define (read e) (λ (mem) (mem-read mem (e mem))))

;; int -> expr
(define (int i) (λ (mem) (int->bv i BIT-WIDTH)))

;; expr expr -> expr
(define (add e1 e2) (λ (mem) (bvadd (e1 mem) (e2 mem))))

;; comm
(define skip (λ (fuel mem) mem))

;; comm comm -> comm
(define (seq c1 c2) (λ (fuel mem) (c2 fuel (c1 fuel mem))))

;; expr expr -> comm
(define (asgn e1 e2) (λ (fuel mem) (mem-write mem (e1 mem) (e2 mem))))

;; expr comm comm -> comm
(define (ite e c1 c2) (λ (fuel mem) (if (bv-nonzero? (e mem)) (c1 fuel mem) (c2 fuel mem))))

;; expr comm -> comm
(define (while e c) (λ (fuel mem)
  (cond
    [(= fuel 0) #f]
    [(bv-nonzero? (e mem)) ((seq c (while e c)) (- fuel 1) mem)]
    [#t mem])))

;; comm -> comm
(define (trace c) (λ (fuel mem) (print fuel mem) (displayln "") (c fuel mem)))

;; mem bv -> bv
(define (mem-read mem a)
  (define i (bv->natural a))
  (mem-perturb mem i)
  (vector-ref mem i))

;; mem bv bv -> mem
(define (mem-write mem a v)
  (define i (bv->natural a))
  (mem-perturb mem i)
  (vector-set! mem i v)
  mem)

(define (mem-perturb mem i)
  (vector-set! mem i (if (flip 0.9999) (vector-ref mem i) (bv-zero BIT-WIDTH))))

(define (blank-memory)
  (list->vector (build-list MEMORY-SIZE (λ (_) (bv-zero BIT-WIDTH)))))

(define (tri n)
  (define total (int 0))
  (define counter (int 1))
  (seq
    (asgn counter (int n))
    (while (read counter)
      (seq
        (asgn total (add (read total) (read counter)))
        (asgn counter (add (read counter) (int -1)))))))

(define (read-total mem)
  (if mem (bv->natural (vector-ref mem 0)) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(define MEMORY-SIZE 2)
(define BIT-WIDTH 2)

(define (main bw n fuel)
  (set! BIT-WIDTH bw)
  (read-total ((tri n) fuel (blank-memory))))

;; scale all three arguments to main from a single number
(define (hardware k) (main k (+ 1 k) (+ 2 k)))
