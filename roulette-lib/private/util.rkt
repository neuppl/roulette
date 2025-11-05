#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide define-wrap
         define/cache
         define-encoder
         flatten-symbolic
         lift-arity
         dict-first-key
         dict-first-value
         in-ddict-reverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         data/ddict
         racket/dict
         racket/match
         racket/stream
         racket/struct
         rosette/base/core/bool
         rosette/base/core/polymorphic
         rosette/base/core/term
         rosette/base/core/union)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ffi

(define-syntax define-wrap
  (syntax-parser
    [(_ to:id
        #:from from:expr
        (~optional (~seq #:target target:id))
        (~optional (~seq #:fields args:id ...))
        (~optional (~and #:cache should-cache)))
     #:with def (if (attribute should-cache) #'define/cache #'define)
     #`(def (to (~? (~@ args ...)))
         (from (~? target) (~? (~@ args ...))))]))

(define-syntax define/cache
  (syntax-parser
    [(_ (x:id a:id ...) body:expr ...)
     #'(define x
         (let ([cache (make-hash)])
           (λ (a ...)
             (define args (list a ...))
             (cond
               [(hash-has-key? cache args)
                (hash-ref cache args)]
               [else
                (define result (let () body ...))
                (hash-set! cache (list a ...) result)
                result]))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rosette

(define-syntax define-encoder
  (syntax-rules ()
    [(_ id [rosette-op smt-op] ...)
     (define (id op)
       (cond [(eq? op rosette-op) smt-op] ...
             [else #f]))]))

;; Modified from `rosette/base/core/forall.rkt`
(define (flatten-symbolic val)
  (let go ([val val])
    (match val
      ;; Can't just use (? @boolean?) because that might produce a symbolic Boolean itself.
      [(and (app @boolean? #t) (? encodable?))
       (hash #t val #f (! val))]
      [(union gvs) (go (apply ite* gvs))]
      [(cons (app go l) (app go r))
       (for*/hash/or ([(l-val l-guard) (in-hash l)]
                      [(r-val r-guard) (in-hash r)])
         (values (cons l-val r-val) (&& l-guard r-guard)))]
      [(expression (== ite) c t e)
       (define guards (go c))
       ;; Is that OK with default value?
       (go (expression ite*
                       (guarded (hash-ref guards #t #f) t)
                       (guarded (hash-ref guards #f #f) e)))]
      [(expression (== ite*) gvs ...)
       (for*/hash/or ([gv (in-list gvs)]
                      [(v g) (in-hash (go (guarded-value gv)))])
         (define test (hash-ref (go (guarded-test gv)) #t))
         (values v (&& test g)))]
      [(expression op es ...)
       (define es-hash
         (let loop ([es es])
           (match es
             [(? null?) (hash '() #t)]
             [(cons e et)
              (define e-hash (go e))
              (define et-hash (loop et))
              (for*/hash/or ([(e-val e-guard) (in-hash e-hash)]
                             [(et-val et-guard) (in-hash et-hash)])
                (values (cons e-val et-val) (&& e-guard et-guard)))])))
       (for*/hash/or ([(vs g) (in-hash es-hash)])
         (values (apply op vs) g))]
      [(? struct?)
       (define-values (si _) (struct-info val))
       (define fields (struct->list val))
       (define make (struct-type-make-constructor si))
       (for*/hash/or ([(fs g) (in-hash (go fields))])
         (values (apply make fs) g))]
      [(? vector?)
       (for*/hash/or ([(es g) (in-hash (go (vector->list val)))])
         (values (list->vector es) g))]
      [_ (hash val #t)])))

(define/cache (encodable? v)
  (match v
    [(expression (or (== @!) (== @!) (== @&&) (== @||) (== @=>) (== @<=>)) es ...)
     (andmap encodable? es)]
    [(? constant?) #t]
    [(or #t #f) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(define-syntax-rule (for*/hash/or (cl ...) body ...)
  (for*/fold ([acc (hash)])
             (cl ...)
    (define-values (k v) (let () body ...))
    (hash-update acc k (λ (x) (|| v x)) #f)))

(define ((lift-arity f) . args)
  (let go ([args args])
    (match args
      [(list x y) (f x y)]
      [(cons x rst) (f x (go rst))])))

(define (dict-first-key ht)
  (dict-iterate-key ht (dict-iterate-first ht)))

(define (dict-first-value ht)
  (dict-iterate-value ht (dict-iterate-first ht)))

(define-syntax-rule (in-ddict-reverse dd)
  (in-stream (ddict->reverse-stream dd)))

(define (ddict->reverse-stream dd)
  (for/stream ([k (reverse (ddict-keys dd))])
    (values k (ddict-ref dd k))))