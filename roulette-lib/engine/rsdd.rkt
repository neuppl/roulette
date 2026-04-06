#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require (except-in racket/contract ->))
(provide
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
  [real-semiring semiring?]
  [complex-semiring semiring?]
  [polynomial-semiring semiring?]
  [semiring? predicate/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rosette/base/core/reflect symbolics)
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
  (_fun _rsdd_bdd_ptr _int64 -> _int64))

(define-wrap rsdd-set-scratch! #:from bdd-set-scratch #:fields x v)
(define-rsdd bdd-set-scratch
  (_fun _rsdd_bdd_ptr _int64 -> _void))

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

(define-wrap rsdd-const? #:from bdd-is-const #:fields x)
(define-rsdd bdd-is-const
  (_fun _rsdd_bdd_ptr -> _stdbool))

(define-wrap rsdd-nodes #:from bdd-count-nodes #:fields x)
(define-rsdd bdd-count-nodes
  (_fun _rsdd_bdd_ptr -> _size))

(define-wrap rsdd-equal? #:from bdd-eq #:fields builder x y)
(define-rsdd bdd-eq
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr -> _stdbool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; real semiring FFI

(define-cpointer-type _rsdd_wmc_params_r)

(define-rsdd new-wmc-params-f64
  (_fun -> _rsdd_wmc_params_r))

(define-rsdd free-wmc-params-f64
  (_fun _rsdd_wmc_params_r -> _void))

(define-wrap rsdd-real-wmc
  #:from (λ (ws ptr) (bdd-wmc ptr ws))
  #:fields weights ptr)
(define-rsdd bdd-wmc
  (_fun _rsdd_bdd_ptr _rsdd_wmc_params_r -> _double))

(define-wrap rsdd-set-real-measure!
  #:from (λ (ws lab lo hi)
           (define lo* (exact->inexact lo))
           (define hi* (exact->inexact hi))
           (wmc-param-f64-set-weight ws lab lo* hi*))
  #:fields weights label lo hi)
(define-rsdd wmc-param-f64-set-weight
  (_fun _rsdd_wmc_params_r _uint64 _double _double -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complex semiring FFI

(define-cpointer-type _rsdd_wmc_params_c)
(define-cstruct _complex_c ([re _double] [im _double]))

(define-rsdd new-wmc-params-complex
  (_fun -> _rsdd_wmc_params_c))

(define-rsdd free-wmc-params-complex
  (_fun _rsdd_wmc_params_c -> _void))

(define-wrap rsdd-complex-wmc
  #:from (λ (ws ptr) (_complex->complex (bdd-wmc-complex ptr ws)))
  #:fields weights ptr)
(define-rsdd bdd-wmc-complex
  (_fun _rsdd_bdd_ptr _rsdd_wmc_params_c -> _complex_c))

(define-wrap rsdd-set-complex-measure!
  #:from (λ (ws lab lo hi)
           (define lo* (complex->_complex lo))
           (define hi* (complex->_complex hi))
           (wmc-param-complex-set-weight ws lab lo* hi*))
  #:fields weights label lo hi)
(define-rsdd wmc-param-complex-set-weight
  (_fun _rsdd_wmc_params_c _uint64 _complex_c _complex_c -> _void))

(define (complex->_complex c)
  (make-complex_c (exact->inexact (real-part c))
                  (exact->inexact (imag-part c))))

(define (_complex->complex c)
  (make-rectangular (complex_c-re c) (complex_c-im c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polynomial semiring FFI

(define-cpointer-type _rsdd_poly_weight)
(define-cpointer-type _rsdd_wmc_params_poly)

(define-cstruct _weight_poly
  ([low _rsdd_poly_weight]
   [high _rsdd_poly_weight]))

(define-rsdd new-wmc-params-poly
  (_fun -> _rsdd_wmc_params_poly))

(define-rsdd destroy_wmc_params_poly
  (_fun _rsdd_wmc_params_poly -> _void))

(define-wrap rsdd-polynomial-wmc
  #:from (lambda (ws ptr)
           (define poly-ptr (bdd-wmc-poly ptr ws))
           (begin0
             (polynomial-get-coeffs poly-ptr (polynomial-len poly-ptr))
             (destroy-polynomial poly-ptr)))
  #:fields weights ptr)
(define-rsdd bdd-wmc-poly
  (_fun _rsdd_bdd_ptr _rsdd_wmc_params_poly -> _rsdd_poly_weight))
(define-rsdd polynomial-len
  (_fun _rsdd_poly_weight -> _size))
(define-rsdd polynomial-get-coeffs
  (_fun _rsdd_poly_weight
        [res : (_list o _double len)]
        [len : _size]
        -> _size
        -> res))
(define-rsdd destroy-polynomial
  (_fun _rsdd_poly_weight -> _void))

(define-wrap rsdd-set-polynomial-measure!
  #:from (lambda (ws lab lo hi)
           (define lo-lst (if (list? lo) lo (list lo)))
           (define hi-lst (if (list? hi) hi (list hi)))
           (wmc-param-poly-set-weight ws lab
                                      lo-lst (length lo-lst)
                                      hi-lst (length hi-lst)))
  #:fields weights label lo hi)
(define-rsdd wmc-param-poly-set-weight
  (_fun _rsdd_wmc_params_poly _uint64
        (_list i _double) _size
        (_list i _double) _size
        -> _void))

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

    (match-define (semiring _ zero add var-set! wmc get-map) semi)
    (define builder (mk-bdd-manager-default-order 0))
    (register-finalizer-and-custodian-shutdown builder free-bdd-manager)

    (define weights (make-weights))
    (define weight-map (get-map weights))
    (define/cache (const->label _) (rsdd-label builder))
    (define enc (make-enc builder const->label))

    (define/public (domain)
      (immutable-set/c any/c))

    (define/public (infer val path-aware? lazy?)
      (define assumes (if path-aware? (vc-assumes (vc)) #t))
      (define vars (list->set (append (symbolics val) (symbolics assumes))))

      ;; Use `in-measures` for "program order" as the variable order.
      (for ([(var measure) (in-measures)]
            #:when (set-member? vars var))
        (define f (measure (set #f)))
        (define t (measure (set #t)))
        (var-set! weight-map (const->label var) f t))

      ;; Compute measure
      (define ht (flatten-symbolic val))
      (define (procedure elems)
        (for/fold ([acc zero])
                  ([elem (in-set elems)])
          (add acc (density elem))))
      (define/cache (density val)
        (if (hash-has-key? ht val)
            (wmc weight-map (enc (&& assumes (hash-ref ht val))))
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
      (for/sum ([f (in-hash-values (flatten-symbolic v))])
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

(struct weights (real complex polynomial))

(define (make-weights)
  (define real (new-wmc-params-f64))
  (define complex (new-wmc-params-complex))
  (define polynomial (new-wmc-params-poly))
  (define result (weights real complex polynomial))
  (define (free _)
    (free-wmc-params-f64 real)
    (free-wmc-params-complex complex)
    (destroy_wmc_params_poly polynomial))
  (register-finalizer-and-custodian-shutdown weights free)
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semirings

(struct semiring (predicate zero add set-measure! wmc get-weight-map)
  #:property prop:procedure 0)

(define real-semiring
  (semiring real? 0.0 +
            rsdd-set-real-measure!
            rsdd-real-wmc
            weights-real))

(define complex-semiring
  (semiring complex? 0.0 +
            rsdd-set-complex-measure!
            rsdd-complex-wmc
            weights-complex))

(define polynomial-semiring
  (semiring polynomial? '()
            polynomial-add
            rsdd-set-polynomial-measure!
            rsdd-polynomial-wmc
            weights-polynomial))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding

(define (make-enc b const->label)
  (define rsdd-true (make-rsdd-true b))
  (define rsdd-false (make-rsdd-false b))

  ;; Using `(sleep 0)` allows encoding to be interleaved with other threads.
  (define/cache (enc v)
    (sleep 0)
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
  (cond
    [(or (rsdd-const? v) (= 1 (rsdd-scratch v 0))) 0]
    [else
     (rsdd-set-scratch! v 1)
     (+ 1 (bdd-size (rsdd-low v)) (bdd-size (rsdd-high v)))]))

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
