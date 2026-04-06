#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide define-wrap
         define/cache
         define-encoder
         flatten-symbolic
         polynomial?
         polynomial-add
         lift-arity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
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
        (~optional (~seq #:fields args:id ...))
        (~optional (~and #:cache should-cache)))
     #:with def (if (attribute should-cache) #'define/cache #'define)
     #`(def (to (~? (~@ args ...)))
         (from (~? (~@ args ...))))]))

;; WARNING: Making these caches anything but `hasheq` will cause them to
;; not be kill safe, and may cause indefinite hangs in multi-threaded
;; applications.
(define-syntax define/cache
  (syntax-parser
    [(_ (x:id a:id ... an:id) body:expr ...)
     #:with (b ...) (generate-temporaries #'(a ...))
     #:with (c ... d) #'(cache b ...)
     #'(define x
         (let ([cache (make-weak-hasheq)])
           (λ (a ... an)
             (define b (hash-ref! c a make-weak-hasheq)) ...
             (hash-ref! d an (λ () body ...)))))]))

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
  (define (go val)
    (match val
      [(or #t #f) (hash val #t)]
      ;; Can't just use (? @boolean?) because that might produce a symbolic Boolean itself.
      [(and (app @boolean? #t) (? encodable?))
       (hash #t val #f (! val))]
      [(union gvs)
       (go-gvs gvs)]
      [(cons (app go l) (app go r))
       (for*/hash/or ([(l-val l-guard) (in-hash l)]
                      [(r-val r-guard) (in-hash r)])
         (values (cons l-val r-val) (&& l-guard r-guard)))]
      [(expression (== ite) c t e)
       (define guards (go c))
       ;; Is that OK with default value?
       (go-gvs (list (cons (hash-ref guards #t #f) t)
                     (cons (hash-ref guards #f #f) e)))]
      [(expression (== ite*) gvs ...)
       (go-gvs (map (λ (g) (cons (guarded-test g) (guarded-value g))) gvs))]
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
      [_ (hash val #t)]))

  (define (go-gvs gvs)
    (for*/hash/or ([gv (in-list gvs)]
                   [(v g) (in-hash (go (cdr gv)))])
      (define test (hash-ref (go (car gv)) #t))
      (values v (&& test g))))

  (go val))

(define/cache (encodable? v)
  (match v
    [(expression (or (== @!) (== @!) (== @&&) (== @||) (== @=>) (== @<=>)) es ...)
     (andmap encodable? es)]
    [(? constant?) #t]
    [(or #t #f) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polynomial

(define (polynomial? v)
  (and (list? v) (andmap real? v)))

(define (polynomial-add p1 p2)
  (let go ([p1 p1] [p2 p2])
    (match* (p1 p2)
      [('() '()) '()]
      [('() p2) p2]
      [(p1 '()) p1]
      [((cons c1 r1) (cons c2 r2))
       (cons (+ c1 c2) (go r1 r2))])))

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
