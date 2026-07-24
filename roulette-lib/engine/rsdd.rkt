#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require (except-in racket/contract ->)
         (rename-in racket/contract [-> base->]))
(provide
 (struct-out semiring)
 (contract-out
  [rename make-rsdd-engine
          rsdd-engine
          (->* ()
               (#:semiring semiring?)
               (is-a?/c engine<%>))]
  [bernoulli-measure (->i ([f (s) (if (unsupplied-arg? s) real-semiring s)]
                           [t (s) (if (unsupplied-arg? s) real-semiring s)])
                          (#:semiring [s semiring?])
                          any)]
  [boolean-semiring semiring?]
  [real-semiring semiring?]
  [complex-semiring semiring?]
  [log-semiring semiring?]
  [polynomial-semiring (base-> semiring? semiring?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rosette define-symbolic* [boolean? @boolean?])
         (only-in rosette/base/core/reflect symbolics)
         ffi/unsafe
         ffi/unsafe/custodian
         ffi/unsafe/define
         ffi/unsafe/define/conventions
         racket/class
         racket/file
         racket/runtime-path
         racket/system
         racket/function
         racket/set
         racket/match
         racket/lazy-require
         rosette/base/core/bool
         rosette/base/core/term
         data/gvector
         "../private/engine.rkt"
         "../private/measure.rkt"
         "../private/measurable-space.rkt"
         "../private/util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FFI prelude

(define rsdd-ffi-lib
  (ffi-lib "librsdd"))

(define-ffi-definer define-rsdd rsdd-ffi-lib
  #:make-c-id convention:hyphen->underscore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BDD FFI

(define-cpointer-type _rsdd_bdd_builder)
(define-cpointer-type _rsdd_bdd_ptr)

(define-rsdd free-bdd-manager
  (_fun _rsdd_bdd_builder -> _void))

(define-rsdd mk-bdd-manager-default-order
  (_fun _int64 -> _rsdd_bdd_builder))

(define-wrap rsdd-label
  #:from bdd-new-label
  #:fields builder)
(define-rsdd bdd-new-label
  (_fun _rsdd_bdd_builder -> _int64))

(define-wrap rsdd-var
  #:from (λ (builder lab) (bdd-var builder lab #t))
  #:fields builder label)
(define-rsdd bdd-var
  (_fun _rsdd_bdd_builder _int64 _stdbool -> _rsdd_bdd_ptr))

(define-wrap rsdd-ite #:from bdd-ite #:fields builder x y z)
(define-rsdd bdd-ite
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-and #:from bdd-and #:fields builder x y)
(define-rsdd bdd-and
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-or #:from bdd-or #:fields builder x y)
(define-rsdd bdd-or
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-compose #:from bdd-compose #:fields builder x l y)
(define-rsdd bdd-compose
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _int64 _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-not #:from bdd-negate #:fields builder x)
(define-rsdd bdd-negate
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-low #:from bdd-low #:fields x)
(define-rsdd bdd-low
  (_fun _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-high #:from bdd-high #:fields x)
(define-rsdd bdd-high
  (_fun _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-scratch #:from bdd-scratch #:fields x d)
(define-rsdd bdd-scratch
  (_fun _rsdd_bdd_ptr _gcpointer -> _gcpointer))

(define-wrap rsdd-set-scratch! #:from bdd-set-scratch #:fields x v)
(define-rsdd bdd-set-scratch
  (_fun _rsdd_bdd_ptr _gcpointer -> _void))

(define-wrap rsdd-clear-scratch! #:from bdd-clear-scratch #:fields x)
(define-rsdd bdd-clear-scratch
  (_fun _rsdd_bdd_ptr -> _void))

(define-wrap make-rsdd-true #:from bdd-true #:fields builder)
(define-rsdd bdd-true
  (_fun _rsdd_bdd_builder -> _rsdd_bdd_ptr))

(define-wrap make-rsdd-false #:from bdd-false #:fields builder)
(define-rsdd bdd-false
  (_fun _rsdd_bdd_builder -> _rsdd_bdd_ptr))

(define-wrap rsdd-true? #:from bdd-is-true #:fields x)
(define-rsdd bdd-is-true
  (_fun _rsdd_bdd_ptr -> _stdbool))

(define-wrap rsdd-false? #:from bdd-is-false #:fields x)
(define-rsdd bdd-is-false
  (_fun _rsdd_bdd_ptr -> _stdbool))

(define-wrap rsdd-neg? #:from bdd-is-neg #:fields x)
(define-rsdd bdd-is-neg
  (_fun _rsdd_bdd_ptr -> _stdbool))

(define-wrap rsdd-const? #:from bdd-is-const #:fields x)
(define-rsdd bdd-is-const
  (_fun _rsdd_bdd_ptr -> _stdbool))

(define-wrap rsdd-nodes #:from bdd-count-nodes #:fields x)
(define-rsdd bdd-count-nodes
  (_fun _rsdd_bdd_ptr -> _size))

(define-wrap rsdd-equal? #:from bdd-eq #:fields builder x y)
(define-rsdd bdd-eq
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr -> _stdbool))

(define-wrap rsdd-topvar #:from bdd-topvar #:fields x)
(define-rsdd bdd-topvar
  (_fun _rsdd_bdd_ptr -> _int64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug

(define-wrap rsdd-num-recursive-calls
  #:from bdd-num-recursive-calls
  #:fields builder)
(define-rsdd bdd-num-recursive-calls
  (_fun _rsdd_bdd_builder -> _size))

(define-wrap rsdd-to-json
  #:from bdd-to-json
  #:fields ptr)
(define-rsdd bdd-to-json
  (_fun _rsdd_bdd_ptr -> _string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; engine

(define (make-rsdd-engine #:semiring [semi real-semiring])
  (new rsdd-engine% [semi semi]))

(define rsdd-engine%
  (class* object% (engine<%>)
    (init semi)
    (super-new)

    (define semiring semi)
    (define zero (semiring-zero semi))
    (define add (semiring-add semi))
    (define one (semiring-one semi))
    (define builder (mk-bdd-manager-default-order 0))
    (register-finalizer-and-custodian-shutdown builder free-bdd-manager)

    (define/cache (const->label _) (rsdd-label builder))
    (define enc (make-enc builder const->label))
    (define weight-map (make-gvector))
    (define weight-cache (box '()))
    (define smoothing #t)
    (register-finalizer-and-custodian-shutdown weight-cache free-weight-cache)

    ;; Use `in-measures` for "program order" as the variable order.
    (define (ensure-labels-exist!)
      (for ([(var measure pc) (in-measures)]
            #:do [(define label (const->label var))]
            #:unless (< label (gvector-count weight-map)))
        (define f (measure (set #f)))
        (define t (measure (set #t)))
        (cond
          [(or (eq? pc #f) (equal? (add f t) one))
           (gvector-set! weight-map label (cons f t))]
          [else
           (define-symbolic* dummy @boolean?)
           (define dummy-label (const->label dummy))
           (set! smoothing (@&& smoothing (@=> pc (@<=> var dummy))))
           (gvector-set! weight-map label (cons f t))
           (gvector-set! weight-map dummy-label (cons one one))])))

    (define/public (domain)
      (immutable-set/c any/c))

    (define/public (infer val path-aware? lazy?)
      (define assumes (if path-aware? (vc-assumes (vc)) #t))
      (ensure-labels-exist!)

      ;; Compute measure
      (define ht (flatten-symbolic val))
      (define (procedure elems)
        (for/fold ([acc zero])
                  ([elem (in-set elems)])
          (add acc (density elem))))
      (define/cache (density val)
        (if (hash-has-key? ht val)
            (wmc (enc (&& assumes smoothing (hash-ref ht val))) weight-map weight-cache semiring)
            zero))
      (define support
        (list->set
         (if lazy?
             (hash-keys ht)
             (filter (λ (k) (not (equal? (density k) zero))) (hash-keys ht)))))
      (measure procedure support density (immutable-set/c any/c)))

    (define/public (recursive-calls)
      (rsdd-num-recursive-calls builder))

    (define/public (size v)
      (ensure-labels-exist!)
      (for/sum ([f (in-hash-values (flatten-symbolic (&& v smoothing)))])
        (bdd-size (enc f))))

    (define/public (show val)
      (for/list ([(val expr) (in-hash (flatten-symbolic val))])
        (define bdd (enc expr))
        (define p (bdd->pict bdd))
        (cons p val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bernoulli

(define (bernoulli-measure f t #:semiring [s real-semiring])
  (define zero (semiring-zero s))
  (define add (semiring-add s))
  (define (proc val)
    (add (if (set-member? val #f) f zero)
         (if (set-member? val #t) t zero)))
  (define (density val)
    (if val t f))
  (define support
    (for/set ([val '(#f #t)]
              #:unless (equal? (density val) zero))
      val))
  (measure proc support density (immutable-set/c @boolean?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; weight maps

(define (wmc val weight-map weight-cache semi)
  (match-define (semiring _ zero add one mul) semi)
  (let go ([val val])
    (define neg? (rsdd-neg? val))
    (define-values (self other)
      (let* ([result (rsdd-scratch val #f)]
             [result (and result (ptr-ref result _racket 0))])
        (cond
          [(not result) (values #f #f)]
          [neg? (values (cdr result) (car result))]
          [else (values (car result) (cdr result))])))
    (cond
      [self self]
      [(rsdd-true? val) (if neg? zero one)]
      [(rsdd-false? val) (if neg? one zero)]
      [else
       (match-define (cons f t) (gvector-ref weight-map (bdd-topvar val)))
       (define result
         (add (mul f (go (rsdd-low val))) (mul t (go (rsdd-high val)))))
       (define scratch
         (malloc-immobile-cell
          (if neg? (cons other result) (cons result other))))
       (set-box! weight-cache (cons scratch (unbox weight-cache)))
       (rsdd-set-scratch! val scratch)
       result])))

(define (free-weight-cache cache)
  (for ([ptr (in-list (unbox cache))])
    (free-immobile-cell ptr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semirings

(struct semiring (predicate zero add one mul)
  #:property prop:procedure 0
  #:guard
  (λ (predicate zero add one mul name)
    (unless (procedure? predicate)
      (raise-arguments-error name "invalid predicate"))
    (unless (and (procedure? add) (procedure-arity-includes? add 2))
      (raise-arguments-error name "invalid addition"))
    (unless (and (procedure? mul) (procedure-arity-includes? mul 2))
      (raise-arguments-error name "invalid multiplication"))
    (values predicate zero add one mul)))

(define boolean-semiring
  (semiring boolean? #f (λ (x y) (or x y)) #t (λ (x y) (and x y))))
(define real-semiring (semiring real? 0 + 1 *))
(define complex-semiring (semiring complex? 0 + 1 *))
(define log-semiring
  (semiring inexact-real?
            -inf.0
            (λ (x y) (log (+ (exp x) (exp y))))
            0
            +))

(define (polynomial-semiring coeff-semi)
  (match-define (semiring predicate _ add one mul) coeff-semi)
  (define (polynomial? v)
    (and (list? v) (andmap predicate v)))
  (define (polynomial-add p1 p2)
    (let go ([p1 p1] [p2 p2])
      (match* (p1 p2)
        [('() '()) '()]
        [('() p2) p2]
        [(p1 '()) p1]
        [((cons c1 r1) (cons c2 r2))
         (cons (add c1 c2) (go r1 r2))])))
  (define (polynomial-mul p1 p2)
    (cond
      [(and (null? p1) (null? p2)) '()]
      [else
       (define result (make-vector (sub1 (+ (length p1) (length p2)))))
       (for ([c1 (in-list p1)]
             [k (in-naturals)]
             #:when #t
             [c2 (in-list p2)]
             [l (in-naturals)])
         (define index (+ k l))
         (vector-set! result index (add (vector-ref result index) (mul c1 c2))))
       (vector->list result)]))
  (semiring polynomial? '() polynomial-add (list one) polynomial-mul))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding

(define (make-enc b const->label)
  (define rsdd-true (make-rsdd-true b))
  (define rsdd-false (make-rsdd-false b))

  (define/cache (enc v)
    (match v
      [(? expression?) (enc-expr v)]
      [(? constant?)   (enc-const v)]
      [_               (enc-lit v)]))

  (define (enc-expr v)
    (match v
      ;; Recognize `ite` shape and use BDD `ite` operation
      [(or (expression (== @||)
                       (expression (== @&&) (expression (== @!) g) e1)
                       (expression (== @&&) g e2))
           (expression (== @||)
                       (expression (== @&&) g e2)
                       (expression (== @&&) (expression (== @!) g) e1)))
       (rsdd-ite b (enc g) (enc e2) (enc e1))]
      [(expression (app rsdd-encoder (? procedure? $op)) es ...)
       (apply $op (map enc es))]
      [_ (error 'enc "cannot encode ~a" v)]))

  (define-encoder rsdd-encoder
    [@! (curry rsdd-not b)]
    [@&& (lift-arity (curry rsdd-and b))]
    [@|| (lift-arity (curry rsdd-or b))]
    [@=> bdd-implies]
    [@<=> bdd-iff])

  (define (bdd-implies x y)
    (rsdd-or b (rsdd-not b x) y))

  (define (bdd-iff x y)
    (rsdd-ite b x y (rsdd-not b y)))

  (define (enc-const v)
    (rsdd-var b (const->label v)))

  (define (enc-lit v)
    (match v
      [#t rsdd-true]
      [#f rsdd-false]
      [(? number?) (hash v rsdd-true)]
      [_ (error 'enc "expected a boolean?, or number?, given ~a" v)]))

  enc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; size

(define/cache (bdd-size v)
  (define DONE (malloc-immobile-cell #t))
  (begin0
    (let go ([v v])
      (cond
        [(or (rsdd-const? v) (eq? DONE (rsdd-scratch v #f))) 0]
        [else
         (rsdd-set-scratch! v DONE)
         (+ 1 (go (rsdd-low v)) (go (rsdd-high v)))]))
    (free-immobile-cell DONE)
    (rsdd-clear-scratch! v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visualization

(lazy-require [pict (bitmap)])
(define-runtime-path HERE ".")
(define PYTHON (find-executable-path "python3"))
(define render-py (build-path HERE ".." "private" "etc" "render_graphviz.py"))

(define (bdd->pict bdd)
  (define input-path (make-temporary-file "rkttmp~a.bdd"))
  (define output-path (make-temporary-file "rkttmp~a.png"))
  (with-output-to-file input-path
    #:exists 'replace
    (λ ()
      (displayln (rsdd-to-json bdd))))
  (define ok?
    (system* PYTHON (path->string render-py)
             "--file" (path->string input-path)
             "--output" (path->string output-path)))
  (unless ok?
    (raise-user-error "failed to convert symbolic expression to image"))
  (bitmap output-path))
