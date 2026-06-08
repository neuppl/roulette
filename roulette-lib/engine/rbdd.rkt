#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require (except-in racket/contract ->))
(provide
 (contract-out
  [rename make-rbdd-engine
          rbdd-engine
          (->* ()
               (is-a?/c engine<%>))]
    [bernoulli-measure (->* (number? number?) () any)])
  rbdd-kill-signal-box)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rosette/base/core/reflect symbolics)
         racket/class
         racket/set
         racket/match
         rosette/base/core/bool
         rosette/base/core/term
         "../private/engine.rkt"
         "../private/measure.rkt"
         "../private/measurable-space.rkt"
         "../private/util.rkt"
         "../private/bdd.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define bdd-true true-ptr)
(define bdd-false false-ptr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; engine

(define (make-rbdd-engine)
  (new rbdd-engine%))

(define rbdd-engine%
  (class* object% (engine<%>)
    (super-new)
    (define enc (make-enc))

    (define/public (domain)
      (immutable-set/c any/c))

    (define/public (infer val path-aware? lazy? env)
      (define assumes (if path-aware? (vc-assumes (vc)) #t))
      (define vars (list->set (append (symbolics val) (symbolics assumes))))

      ;; Use `in-measures` for "program order" as the variable order.
      (for ([(var measure) (in-measures)]
            #:when (set-member? vars var))
        (define f (measure (set #f)))
        (define t (measure (set #t)))
        (define temp (enc var))
        (set-var-weight! temp (measure (set #f)) (measure (set #t))))

      ;; Compute measure
      (define ht (flatten-symbolic val))
      (define (procedure elems)
        (for/fold ([acc 0])
                  ([elem (in-set elems)])
          (+ acc (density elem))))
      (define/cache (density val)
        (cond
          [(hash-has-key? ht val)
           (define g (&& assumes (hash-ref ht val)))
           (wmc (enc (if env (substitute g env) g)))]
          [else 0]))
      (define support
        (list->set
         (if lazy?
             (hash-keys ht)
             (filter (λ (k) (not (equal? (density k) 0))) (hash-keys ht)))))
      (measure procedure support density (immutable-set/c any/c)))

    (define/public (recursive-calls)
      (bdd-num-recursive-calls))

    (define/public (size v)
      (for/sum ([f (in-hash-values (flatten-symbolic v))])
        (cached-size (enc f))))

    (define/public (show val)
      (displayln "Not implemented :("))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bernoulli

(define (bernoulli-measure f t)
  (define (proc val)
    (+ (if (set-member? val #f) f 0)
       (if (set-member? val #t) t 0)))
  (define (density val)
    (if val t f))
  (define support
    (for/set ([val '(#f #t)]
              #:unless (equal? (density val) 0))
      val))
  (measure proc support density (immutable-set/c @boolean?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding

(define rbdd-kill-signal-box (box #f))

(define (make-enc)
  (define/cache (enc v)
    ;(display ".")

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
       (bdd-ite (enc g) (enc e2) (enc e1))]
      [(expression (app bdd-encoder (? procedure? $op)) es ...)
       (apply $op (map enc es))]
      [_ (error 'enc "cannot encode ~a" v)]))

  (define-encoder bdd-encoder
    [@! bdd-not]
    [@&& (lift-arity (lambda (x y) 
      (sleep 0)
      (let ([return (unbox rbdd-kill-signal-box)])
        (when return
          (return)))
          (bdd-and x y)))]
    [@|| (lift-arity bdd-or)]
    [@=> bdd-implies]
    [@<=> bdd-iff])

  (define (bdd-implies x y)
    (bdd-or (bdd-not x) y))

  (define (bdd-iff x y)
    (bdd-ite x y (bdd-not y)))

  (define (enc-const v)
    (fresh-var #t))

  (define (enc-lit v)
    (match v
      [#t bdd-true]
      [#f bdd-false]
      [(? number?) (hash v bdd-true)]
      [_ (error 'enc "expected a boolean?, or number?, given ~a" v)]))

  enc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; substitution

(define (substitute v env)
  (define/cache (go v)
    (match v
      [(? expression?) (go-expr v)]
      [(? constant?)   (hash-ref env v v)]
      [_               v]))

  (define (go-expr v)
    (match v
      [(or (expression (== @||)
                       (expression (== @&&) (expression (== @!) g) e1)
                       (expression (== @&&) g e2))
           (expression (== @||)
                       (expression (== @&&) g e2)
                       (expression (== @&&) (expression (== @!) g) e1)))
       (if-then-else (go g) (go e1) (go e2))]

      [(expression (? procedure? $op) es ...)
       (apply $op (map go es))]))

  (go v))

(define (if-then-else g e1 e2)
  (cond
    [(eq? g #t) e1]
    [(eq? g #f) e2]
    [(eq? e1 #t) (|| g e2)]
    [(eq? e1 #f) (&& (! g) e2)]
    [(eq? e2 #t) (|| g e1)]
    [(eq? e2 #f) (&& (! g) e1)]
    [else (expression @||
                      (expression @&& (expression @! g) e1)
                      (expression @&& g e2))]))
