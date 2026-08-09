#lang roulette

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (rename-out
  [module-begin #%module-begin]
  [top-interaction #%top-interaction])

 ;; operations
 flip
 query
 sample
 observe!
 region?

 ;; debug
 clear-cache!
 recursive-calls
 size

 ;; pmf
 pmf
 pmf?
 pmf-support
 in-pmf
 for/pmf
 pmf-hash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (prefix-in base: racket/base)
         (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/struct
         roulette/engine/rsdd
         text-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants and data

(gc-terms!)

(base:struct region (vals) #:mutable)
(define (make-region) (region (set)))
(define (region-add! reg val)
  (set-region-vals! reg (set-add (region-vals reg) val)))
(define (innermost-region)
  (match (current-regions)
    [(cons x _) x]
    [_ top-region]))

(define engine (rsdd-engine))
(define top-region (make-region))
(define current-regions (make-parameter null))

(struct evidence (observe sample))
(define current-evidence (make-parameter (evidence #t #t)))
(define ⊥ (gensym '⊥))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probability mass function

(struct pmf (hash)
  #:property prop:procedure
  (λ (self value) (hash-ref (pmf-hash self) value 0))
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) 'pmf)
      (λ (self)
        (match-define (pmf ht) self)
        (for/list ([(k v) (in-hash ht)])
          (unquoted-printing-string (format "[~v ~a]" k v))))))])

(define (make-pmf ht)
  (for/pmf ([(value measure) (in-hash ht)]
            #:when (not (zero? measure)))
    (values value measure)))

(define (pmf-support pmf)
  (hash-keys (pmf-hash pmf)))

(define (in-pmf pmf)
  (in-hash (pmf-hash pmf)))

(define-syntax-rule (for/pmf (for-clause ...) body-or-break ... body)
  (pmf (for/hash (for-clause ...) body-or-break ... body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic features

(define (flip pr #:region [reg #f])
  (cond
    [(= pr 0) #f]
    [(= pr 1) #t]
    [else
     (for*/all ([pr pr] [reg reg])
       (when reg (check-region-validity! reg))
       (define-measurable* x (bernoulli-measure (- 1 pr) pr))
       (region-add! (or reg (innermost-region)) x)
       x)]))

(define (check-region-validity! reg)
  (define regs (member reg (current-regions)))
  (assert-panic! regs "region not active")
  (define ok?
    (for/and ([var (in-list (symbolics (vc-assumes (vc))))])
      (memf (λ (r) (set-member? (region-vals r) var)) regs)))
  (assert-panic! ok? "region does not live long enough"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sampling

(define (sample e)
  (define ev (current-evidence))
  (match-define (evidence obs samp) ev)
  (define e′ (if (&& obs samp) e ⊥))
  (define ht (query-val e′ (set)))
  (define result (hash-sample ht))
  (define pr (hash-ref ht result))
  (when obs
    (define-measurable* γ #:affine? #t (bernoulli-measure 1 (/ 1 pr)))
    (region-add! (innermost-region) γ)
    (define samp′ (&& samp (guard-with-assume (&& (equal? e result) γ))))
    (current-evidence (struct-copy evidence ev [sample samp′])))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; observation

(define (observe! e)
  (define ev (current-evidence))
  (match-define (evidence obs _samp) ev)
  (define obs′ (&& obs (guard-with-assume e)))
  (current-evidence (struct-copy evidence ev [observe obs′])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inference

(define-syntax query
  (syntax-parser
    [(_ (~alt (~optional (~seq #:samples samples:nat))
              (~optional (~seq #:region x:id))) ...
        body:expr ...+)
     #'(query-fn (λ ((~? x)) body ...) (~? (~@ #:samples samples)))]))

(define (query-fn body #:samples [n 1] #:region [reg (make-region)])
  (for/lists (vs ws #:result (mean vs ws))
             ([_ (in-range n)])
    (parameterize ([current-regions (cons reg (current-regions))]
                   [current-evidence (current-evidence)])
      (define val (if (zero? (procedure-arity body)) (body) (body reg)))
      (match-define (evidence obs samp) (current-evidence))
      (define val′ (if (&& obs samp) val ⊥))
      (check-query-flips! val′)
      (define prev-vars (allocated-vars (rest (current-regions))))
      (define ev′ (if samp obs ⊥))
      (values (query-val val′ prev-vars)
              (query-val ev′ prev-vars)))))

(define (allocated-vars regs)
  (apply set-union (set) (map region-vals regs)))

(define (query-val val vars)
  (define unnormalized (infer val #:keep vars #:engine engine))
  (define prob (density unnormalized))
  (define supp (set->list (support unnormalized)))
  (let go ([supp-probs (map prob supp)]
           [rev-probs '()])
    (match supp-probs
      [(cons x xt)
       (for/all ([x x #:exhaustive])
         (go xt (cons x rev-probs)))]
      [(list)
       (query-weight supp (reverse rev-probs))])))

(define (query-weight supp probs)
  (define normalizer
    (for/fold ([acc 0])
              ([value (in-list supp)]
               [prob (in-list probs)]
               #:unless (eq? value ⊥))
      (+ acc prob)))
  (and (positive? normalizer)
       (for/hash ([value (in-list supp)]
                  [prob (in-list probs)]
                  #:unless (eq? value ⊥)
                  #:unless (zero? prob))
         (values value (/ prob normalizer)))))

(define (check-query-flips! val)
  (assert-panic!
   (subset? (list->set (symbolics val))
            (allocated-vars (current-regions)))
   "region for value has ended"))

(define (hash-sample ht)
  (define target (random))
  (let go ([seq (sequence->stream (in-hash ht))] [acc 0])
    (match-define (stream* (values v p) rst) seq)
    (define acc* (+ acc p))
    (if (< target acc*) v (go rst acc*))))

(define (mean vs ws)
  (define total
    (for/fold ([acc 0])
              ([w (in-list ws)])
      (for*/all ([acc acc] [w w])
        (+ acc (hash-ref w #t 0)))))
  (for/fold ([acc (hash)] #:result (for/all ([acc acc]) (make-pmf acc)))
            ([v (in-list vs)]
             [w (in-list ws)])
    (for*/all ([acc acc] [v v] [w w] [total total])
      (if (hash? v)
          (hash-combine acc v (hash-ref w #t 0) total)
          acc))))

(define (hash-combine acc v w total)
  (for/fold ([acc acc])
            ([(k p) (in-hash v)])
    (hash-update acc k (curry + (/ (* p w) total)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wrapping

(struct header (val))

(define-syntax module-begin
  (make-wrapping-module-begin #'wrap))

(define-syntax top-interaction
  (make-wrapping-top-interaction #'wrap))

(define-syntax-rule (wrap e ...)
  (void (print-value (λ () e)) ...))

;; At the top level, we must allocate in the top region. Additionally,
(define (print-value thk)
  (print-result
   (query-fn thk #:region top-region)))

(define ((~header f) x)
  (match x
    [(header x) (~a x)]
    [_ (f x)]))

(define (print-result pmf)
  (define ht (pmf-hash pmf))
  (if (= (hash-count ht) 1)
      ((current-print) (first (hash-keys ht)))
      (print-table
       #:row-sep? '(#t #f ...)
       #:->string (list (~header ~v) (~header ~a))
       (cons
        (map header '(Value Probability))
        (for/list ([(v p) (in-hash ht)])
          (list v p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug

(define (clear-cache!)
  (set! engine (rsdd-engine)))

(define (recursive-calls)
  (send engine recursive-calls))

(define (size v)
  (send engine size v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; categorical random variable

(define (make-categorical xs)
  (bin-cat (filter (λ (x) (not (zero? (cdr x)))) xs)))

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
;; util

;; If parameters were properly lifted, then this utility wouldn't be needed.
(define (guard-with-assume v)
  (=> (vc-assumes (vc)) v))

;; Make region errors a fatal error for now, but we should really figure out
;; how to properly handle errors.
(define (assert-panic! val msg)
  (unless val
    (raise (exn msg (current-continuation-marks)))))
