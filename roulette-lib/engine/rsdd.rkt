#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require (except-in racket/contract ->))
(provide
 (contract-out
  [rsdd-engine (->* () (#:semiring semiring?) engine?)]
  [bernoulli-measure (->i ([f (s) (if (unsupplied-arg? s) real-semiring s)]
                           [t (s) (if (unsupplied-arg? s) real-semiring s)])
                          (#:semiring [s semiring?])
                          any)]
  [real-semiring semiring?]
  [complex-semiring semiring?]
  [semiring? predicate/c])
  const->label
  rsdd-free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rosette/base/core/reflect symbolics)
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/define/conventions
         racket/set
         racket/match
         rosette/base/core/bool
         rosette/base/core/term
         "../private/engine.rkt"
         "../private/log.rkt"
         "../private/measure.rkt"
         "../private/measurable-space.rkt"
         "../private/util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(define rsdd-ffi-lib
  (ffi-lib "librsdd" #:custodian 'place))

(define-ffi-definer define-rsdd rsdd-ffi-lib
  #:make-c-id convention:hyphen->underscore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdd

(define-cpointer-type _rsdd_bdd_builder)
(define-cpointer-type _rsdd_bdd_ptr)

(define-rsdd free-bdd-manager
  (_fun _rsdd_bdd_builder -> _void))

(define-rsdd mk-bdd-manager-default-order
  (_fun _int64 -> _rsdd_bdd_builder))

(define-wrap rsdd-label
  #:from bdd-new-label
  #:target BUILDER)
(define-rsdd bdd-new-label
  (_fun _rsdd_bdd_builder -> _int64))

(define-wrap rsdd-var
  #:from (λ (builder lab) (bdd-var builder lab #t))
  #:target BUILDER
  #:fields label)
(define-rsdd bdd-var
  (_fun _rsdd_bdd_builder _int64 _stdbool -> _rsdd_bdd_ptr))

(define-wrap rsdd-ite #:from bdd-ite #:target BUILDER #:fields x y z #:cache)
(define-rsdd bdd-ite
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-and #:from bdd-and #:target BUILDER #:fields x y #:cache)
(define-rsdd bdd-and
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-or #:from bdd-or #:target BUILDER #:fields x y #:cache)
(define-rsdd bdd-or
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-compose #:from bdd-compose #:target BUILDER #:fields x l y #:cache)
(define-rsdd bdd-compose
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _int64 _rsdd_bdd_ptr -> _rsdd_bdd_ptr))

(define-wrap rsdd-not #:from bdd-negate #:target BUILDER #:fields x #:cache)
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

(define-wrap make-rsdd-true #:from bdd-true #:target BUILDER)
(define-rsdd bdd-true
  (_fun _rsdd_bdd_builder -> _rsdd_bdd_ptr))

(define-wrap make-rsdd-false #:from bdd-false #:target BUILDER)
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

(define-wrap rsdd-equal? #:from bdd-eq #:target BUILDER #:fields x y #:cache)
(define-rsdd bdd-eq
  (_fun _rsdd_bdd_builder _rsdd_bdd_ptr _rsdd_bdd_ptr -> _stdbool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wmc

(define-cpointer-type _rsdd_wmc_params_r)

(define-rsdd new-wmc-params-f64
  (_fun -> _rsdd_wmc_params_r))

(define-wrap rsdd-real-wmc
  #:from (λ (ws ptr) (bdd-wmc ptr ws))
  #:target WEIGHTS
  #:fields ptr)
(define-rsdd bdd-wmc
  (_fun _rsdd_bdd_ptr _rsdd_wmc_params_r -> _double))

(define-wrap rsdd-set-real-measure!
  #:from (λ (ws lab lo hi)
           (define lo* (exact->inexact lo))
           (define hi* (exact->inexact hi))
           (wmc-param-f64-set-weight ws lab lo* hi*))
  #:target WEIGHTS
  #:fields label lo hi)
(define-rsdd wmc-param-f64-set-weight
  (_fun _rsdd_wmc_params_r _uint64 _double _double -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complex wmc

(define-cpointer-type _rsdd_wmc_params_c)
(define-cstruct _complex_c ([re _double] [im _double]))

(define-rsdd new-wmc-params-complex
  (_fun -> _rsdd_wmc_params_c))

(define-wrap rsdd-complex-wmc
  #:from (λ (ws ptr) (_complex->complex (bdd-wmc-complex ptr ws)))
  #:target COMPLEX-WEIGHTS
  #:fields ptr)
(define-rsdd bdd-wmc-complex
  (_fun _rsdd_bdd_ptr _rsdd_wmc_params_c -> _complex_c))

(define-wrap rsdd-set-complex-measure!
  #:from (λ (ws lab lo hi)
           (define lo* (complex->_complex lo))
           (define hi* (complex->_complex hi))
           (wmc-param-complex-set-weight ws lab lo* hi*))
  #:target COMPLEX-WEIGHTS
  #:fields label lo hi)
(define-rsdd wmc-param-complex-set-weight
  (_fun _rsdd_wmc_params_c _uint64 _complex_c _complex_c -> _void))

(define (complex->_complex c)
  (make-complex_c (exact->inexact (real-part c))
                  (exact->inexact (imag-part c))))

(define (_complex->complex c)
  (make-rectangular (complex_c-re c) (complex_c-im c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug

(define-wrap rsdd-num-recursive-calls
  #:from bdd-num-recursive-calls
  #:target BUILDER)
(define-rsdd bdd-num-recursive-calls
  (_fun _rsdd_bdd_builder -> _size))

(define-wrap rsdd-to-json
  #:from bdd-to-json
  #:fields ptr)
(define-rsdd bdd-to-json
  (_fun _rsdd_bdd_ptr -> _string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define BUILDER (mk-bdd-manager-default-order 0))
(define WEIGHTS (new-wmc-params-f64))
(define COMPLEX-WEIGHTS (new-wmc-params-complex))

(define rsdd-true (make-rsdd-true))
(define rsdd-false (make-rsdd-false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; engine

(define (rsdd-engine #:semiring [s real-semiring])
  (engine (make-infer s) (immutable-set/c any/c)))

(define (rsdd-free)
  (free-bdd-manager BUILDER)
  (set! BUILDER (mk-bdd-manager-default-order 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semirings

(provide semiring-predicate)

(struct semiring (predicate zero plus var-set! wmc)
  #:property prop:procedure 0
  #:transparent)

(define (bernoulli-measure f t #:semiring [s real-semiring])
  (match-define (semiring _ zero plus _ _) s)
  (define (proc val)
    (plus (if (set-member? val #f) f zero)
          (if (set-member? val #t) t zero)))
  (define (density val)
    (if val t f))
  (define support
    (for/set ([val '(#f #t)]
              #:unless (equal? (density val) zero))
      val))
  (measure proc support density (immutable-set/c @boolean?)))

(define real-semiring
  (semiring real? 0.0 + rsdd-set-real-measure! rsdd-real-wmc))

(define complex-semiring
  (semiring complex? 0.0 + rsdd-set-complex-measure! rsdd-complex-wmc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding

(define (enc v)
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
     (rsdd-ite (enc g) (enc e2) (enc e1))]
    [(expression (app rsdd-encoder (? procedure? $op)) es ...)
     (apply $op (map enc es))]
    [_ (error 'enc "cannot encode ~a" v)]))

(define-encoder rsdd-encoder
  [@! rsdd-not]
  [@&& (lift-arity rsdd-and)]
  [@|| (lift-arity rsdd-or)]
  [@=> rsdd-implies]
  [@<=> rsdd-iff])

(define (rsdd-implies x y)
  (rsdd-or (rsdd-not x) y))

(define (rsdd-iff x y)
  (rsdd-ite x y (rsdd-not y)))

(define (enc-const v)
  (rsdd-var (const->label v)))

;; Marked as cached because used in `wmc`
(define/cache (const->label v)
  (rsdd-label))

(define (enc-lit v)
  (match v
    [#t rsdd-true]
    [#f rsdd-false]
    [(? number?) (hash v rsdd-true)]
    [_ (error 'enc "expected a boolean?, or number?, given ~a" v)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; infer
(require racket/dict)
(define (make-infer s)
  (match-define (semiring _predicate zero plus var-set! wmc) s)
  (λ (val lazy?)
    ;(displayln "inside make infer")
    (define vars (list->set (symbolics val)))

    ;(printf "before for. size: ~v\n" (dict-count measures))
    (displayln "before for")
    ;; Use `in-ddict-reverse` for "program order" as the variable order.
    (for ([(var measure) (in-ddict-reverse measures)]
          #:when (set-member? vars var))
      ;(displayln var)
      (define temp (const->label var))
      ;(displayln "after const->label")
      (var-set! temp (measure (set #f)) (measure (set #t))))

    (displayln "after for")
    ;; Compute measure
    (define ht (flatten-symbolic val))
    (displayln "after flatten-symbolic")
    (define (procedure elems)
      (for/fold ([acc zero])
                ([elem (in-set elems)])
        (plus acc (density elem))))
    (define (density val)
      (displayln "before enc/log!")
      (begin0 
        (if (hash-has-key? ht val)
            (wmc (enc/log! (hash-ref ht val)))
            zero)
        (displayln "after enc/log!")))
    (define support
      (list->set
       (if lazy?
           (hash-keys ht)
           (filter (compose not zero? density) (hash-keys ht)))))
    (measure procedure support density (immutable-set/c any/c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; size

(define (enc/log! expr)
  (define bdd (enc expr))
  (begin0
    bdd
    (log-roulette-info "~a total size" (cached-size bdd))
    (log-roulette-info "~a recursive calls" (rsdd-num-recursive-calls))))

(define (cached-size bdd)
  (cond
    [(or (rsdd-const? bdd) (= 1 (rsdd-scratch bdd 0))) 0]
    [else
     (rsdd-set-scratch! bdd 1)
     (+ 1 (cached-size (rsdd-low bdd)) (cached-size (rsdd-high bdd)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visualization

(module+ gui
  (provide debug)

  (require racket/runtime-path
           racket/gui
           pict)

  (define-runtime-path HERE ".")
  (define PYTHON (find-executable-path "python3"))
  (define render-py (build-path HERE ".." "private" "etc" "render_graphviz.py"))

  (define (debug val #:show? [show? #f])
    (begin0
      (for/list ([(val expr) (in-hash (flatten-symbolic val))])
        (define bdd (enc expr))
        (define p (bdd->pict bdd))
        (when show? (show-pict p))
        (log-roulette-info "~v has size ~a" val (rsdd-nodes (enc expr)))
        (cons p val))
      (log-roulette-info "~a recursive calls" (rsdd-num-recursive-calls))))

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
    (bitmap output-path)))
