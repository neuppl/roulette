#lang roulette/example/disrupt
(require "../../../bdd-engine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitvectors

(define (bv k size)
  (cond
    [(zero? size) '()]
    [else (cons (odd? k) (bv (truncate (/ k 2)) (sub1 size)))]))

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

(define (bvxor xs ys)
  (map xor xs ys))

(define (rotate-left k xs)
  (define n (length xs))
  (for/all ([k k #:exhaustive])
    (append (build-list k (λ _ #f)) (drop-right xs k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (select l)
  (define n (length l))
  (cond
    [(= n 1) (first l)]
    [(flip (/ 1 n)) (first l)]
    [else (select (rest l))]))

(define SIZE 4)

(define (split left right)
  (if (flip 0.5) left right))

(define (forward rate node)
  (if (flip rate) node #f))

(define (diamond msg)
  (split (noisy 0.01 msg)
         (forward 0.99 msg)))

(define (noisy rate msg)
  (define bit (select (range SIZE)))
  (define mask (rotate-left bit (bv 1 SIZE)))
  (if (and (flip rate) msg) (bvxor msg mask) msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(define (main iters)
  (run iters))

(define (run iters)
  (for/fold ([acc (bv #b1111 SIZE)])
            ([_ (in-range iters)])
    (diamond acc)))


(when (not (equal? bdd-engine-backend "rsdd")) (max-arg main #:start 1 #:end 50 #:timeout 1))
