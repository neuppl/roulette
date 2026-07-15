#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "bif-parse.rkt"
         json
         racket/cmdline
         racket/promise
         racket/match
         rosette
         mischief/sort
         mischief/dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct entry (depends tree))

(define DEFAULT (gensym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interpreter

(define (bayes-eval e query-var var-order)
  (let go ([e e] [vars (hash)] [probs (hash)])
    (match e
      [(list)
       (define result (eval-blks vars probs var-order))
       (if query-var
           (hash-ref result query-var)
           (hash->list result))]
      [(cons blk rst)
       (define-values (vars* probs*)
         (collect-blk blk vars probs))
       (go rst vars* probs*)])))

(define (eval-blks vars probs var-order)
  (define deps
    (for/hash ([(var an-entry) (in-hash probs)])
      (values var (entry-depends an-entry))))
  (define var-ordered
    (or var-order (topo-sort (hash-keys vars) deps)))
  (for/fold ([acc (hash)])
            ([var (in-list var-ordered)])
    (match-define (entry depends tree) (hash-ref probs var))
    (define cond-vals (map (curry hash-ref acc) depends))
    (define var-vals (hash-ref vars var))
    (define prob (get-prob cond-vals var-vals tree))
    (hash-set acc var prob)))

(define (collect-blk blk vars probs)
  (match blk
    [`(network ,_) (values vars probs)]
    [`(variable ,var (type discrete (,len) ,vals))
     (values (hash-set vars var vals) probs)]
    [`(probability (,var) (table . ,nums))
     (collect-blk `(probability (,var) [() . ,nums]) vars probs)]
    [`(probability (,var . ,conds) . ,table)
     (values vars (hash-set probs var (entry conds (make-tree table (hash-ref vars var)))))]))

(define (make-tree table var-vals)
  (define result (make-hash))
  (for ([row (in-list table)])
    (tree-add-row! result var-vals row))
  result)

(define (tree-add-row! tree var-vals row)
  (match-define (cons vals probs) row)
  (for/fold ([tree tree]
             #:result (hash-set! tree DEFAULT probs))
            ([val (in-list vals)])
    (hash-ref! tree val make-hash)))

(define (get-prob cond-vals var-vals tree)
  (let go ([cond-vals cond-vals] [tree tree])
    (match cond-vals
      [(list)
       (make-categorical (map cons var-vals (hash-ref tree DEFAULT)))]
      [(cons cond-val cond-vals-rst)
       (if (union? cond-val)
           (let go* ([xs (union-contents cond-val)])
             (match xs
               [(list (cons g v))
                (go cond-vals-rst (hash-ref tree v))]
               [(cons (cons g v) rst)
                (if g
                    (go cond-vals-rst (hash-ref tree v))
                    (go* rst))]))
           (go cond-vals-rst (hash-ref tree cond-val)))
       ;; Using `for/all` gives worse performance!
       #;(for/all ([cond-val cond-val])
         (go cond-vals-rst (hash-ref tree cond-val)))])))

(define (make-categorical xs)
  (bin-cat (filter (λ (x) (not (zero? (cdr x)))) xs)))

(define (topo-sort vars ins)
  (define outs (hash-reverse ins))
  (define result null)
  (define (visit var)
    (when (empty? (hash-ref ins var))
      (set! result (cons var result))
      (for ([next (in-list (hash-ref outs var null))])
        (set! ins (hash-update ins next (curry remove var)))
        (visit next))))
  (for-each visit (filter (λ (var) (empty? (hash-ref ins var))) vars))
  (reverse result))

(define (hash-reverse ht)
  (for*/fold ([acc (hash)])
             ([(k vs) (in-hash ht)]
              [v (in-list vs)])
    (hash-update acc v (λ (xt) (cons k xt)) null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one-hot

(define (one-hot-cat xs)
  (let go ([xs xs] [p 1])
    (match xs
      [(list (cons b _)) b]
      [(cons (cons b x) xt)
       (define p* (/ x p))
       (if (flip p*)
           b
           (go xt (* p (- 1 p*))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bin-cat

(require (only-in rosette assert))

(define (bin-cat xs)
  (match xs
    [(list) (assert #f)]
    [(list (cons x _)) x]
    [_
     (define-values (left right)
       (split-at xs (floor (/ (length xs) 2))))
     (define left-sum (foldl + 0 (map cdr left)))
     (if (flip left-sum)
         (bin-cat (renormalize left left-sum))
         (bin-cat (renormalize right (- 1 left-sum))))]))

(define (renormalize xs n)
  (for/list ([x+y (in-list xs)])
    (cons (car x+y) (/ (cdr x+y) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(define (main name query-var var-order)
  (with-input-from-file name
    (λ () (query (bayes-eval (bif-parse) query-var var-order)))))
