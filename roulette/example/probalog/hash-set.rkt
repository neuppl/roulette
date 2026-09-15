#lang racket

(provide (all-defined-out))

(require rackunit
         rackunit/text-ui
         racket/pretty
         (for-syntax racket syntax/parse)
         "guards.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guarded hashes
;;
;; Guarded hashes map concrete keys to the guards under which they are
;; present.

;; Path condition of a key
(define (pc ht key) (hash-ref ht key guard-false))

;; Constructor
(define-syntax (my-hash stx)
  (syntax-parse stx
    [(_ (~seq key guard) ...)
     #'(for/fold ([ht (hash)])
                 ([k (in-list (list key ...))]
                  [g (in-list (list guard ...))])
         (hash-update ht k (lambda (existing) (guard-or existing g))
                      (lambda () (guard-false))))]))

;; Hash utilities

(define (my-hash-has-key? ht key) (pc ht key))

(define (my-hash-set ht key [guard (guard-true)])
  (if (guard-known-true? guard)
      (hash-set ht key (guard-true))
      (hash-set ht key (guard-or guard (pc ht key)))))

(define (my-hash-remove ht key [guard (guard-true)])
  (if (guard-known-true? guard)
      (hash-remove ht key)
      (hash-set ht key (guard-and (pc ht key) (guard-not guard)))))

(define (my-hash-union ht . hts)
  (for*/fold ([acc ht])
             ([h (in-list hts)]
              [(key g) (in-hash h)])
    (hash-set acc key (guard-or (pc acc key) g))))

(define (my-hash-intersect ht . hts)
  (for/fold ([acc ht]) ([h (in-list hts)])
    (for/fold ([acc2 (hash)]) ([(key g) (in-hash acc)])
      (if (hash-has-key? h key)
          (hash-set acc2 key (guard-and g (pc h key)))
          acc2))))

(define (my-hash-keys-subset? ht1 ht2)
  (for/fold ([acc (guard-true)]) ([(key g) (in-hash ht1)])
    (guard-and acc (guard-implies g (pc ht2 key)))))

(define (my-hash-empty? ht)
  (for/fold ([acc (guard-true)]) ([(_ g) (in-hash ht)])
    (guard-and acc (guard-not g))))

(define (all-keys ht1 ht2)
  (for/fold ([acc (hash-keys ht1)]) ([k (in-hash-keys ht2)])
    (if (hash-has-key? ht1 k) acc (cons k acc))))

;; Semantic equality: every key present under equivalent conditions.
;; Not used by the fixpoint loop, which compares `changed-keys` instead.
(define (my-hash-equal? ht1 ht2 [keys (all-keys ht1 ht2)])
  (for/and ([key (in-list keys)])
    (guard-equiv? (pc ht1 key) (pc ht2 key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets
;;
;; A thin wrapper over guarded hashes for set behaviour


;; struct with custom printer and iterator
(struct sym-set (ht)
  #:transparent
  #:property prop:sequence
  (lambda (s) (in-hash (sym-set-ht s)))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (parameterize ([pretty-printing #f])
       (define (write-elem k)
         (if (eq? mode #f) (display k port) (write k port)))
       (write-string "(sym-set" port)
       (for ([(k g) (in-hash (sym-set-ht self))])
         (write-string "\n  " port)
         (cond
           ;; An element present in every world prints bare; the rest
           ;; print as [guard element].
           [(guard-known-true? g) (write-elem k)]
           [else
            (write-string "[" port)
            (write-elem g)
            (write-string " " port)
            (write-elem k)
            (write-string "]" port)]))
       (write-string ")" port)))])

;; constructor
(define-syntax (set stx)
  (syntax-parse stx
    [(_ elem ...)
     #:with (pr ...) (apply append
                            (map (lambda (e) (list e #'(guard-true)))
                                 (syntax->list #'(elem ...))))
     #'(sym-set (my-hash pr ...))]))


;; Set utilities

(define (set-member? st v) (my-hash-has-key? (sym-set-ht st) v))

(define (set-add st v [guard (guard-true)])
  (sym-set (my-hash-set (sym-set-ht st) v guard)))

(define (set-empty? st) (my-hash-empty? (sym-set-ht st)))

(define (set-remove st v [guard (guard-true)])
  (sym-set (my-hash-remove (sym-set-ht st) v guard)))

(define (set-union st . sts)
  (sym-set (apply my-hash-union (sym-set-ht st) (map sym-set-ht sts))))

(define (set-intersect st . sts)
  (sym-set (apply my-hash-intersect (sym-set-ht st) (map sym-set-ht sts))))

(define (set-subtract st0 . sts)
  (for*/fold ([acc st0])
             ([st (in-list sts)]
              [key (in-hash-keys (sym-set-ht st))])
    (set-remove acc key)))

(define (subset? st1 st2)
  (my-hash-keys-subset? (sym-set-ht st1) (sym-set-ht st2)))

(define (set-equal? st1 st2 [keys #f])
  (if keys
      (my-hash-equal? (sym-set-ht st1) (sym-set-ht st2) keys)
      (my-hash-equal? (sym-set-ht st1) (sym-set-ht st2))))



;; Mutable hash table used in for/sym-set iteration as a
;; performance optimization.
;; Avoids repeated copying of hash to functionally update entries.
(struct sym-set-builder (ht))
(define (make-sym-set-builder) (sym-set-builder (make-hash)))

(define (builder-add! b v [guard (guard-true)])
  (if (guard-known-true? guard)
      (hash-set! (sym-set-builder-ht b) v (guard-true))
      (hash-update! (sym-set-builder-ht b) v
                    (lambda (existing) (guard-or existing guard))
                    (lambda () (guard-false)))))

(define (builder->sym-set b)
  (sym-set (for/hash ([(v g) (in-hash (sym-set-builder-ht b))])
             (values v g))))

;; Each iteration's body returns either an element, added in every
;; world, or an element and a guard as two values.
(define-syntax-rule (for/sym-set (clause ...) body ...)
  (let ([b (make-sym-set-builder)])
    (for (clause ...)
      (call-with-values
       (lambda () body ...)
       (case-lambda
         [(v) (builder-add! b v)]
         [(v g) (builder-add! b v g)])))
    (builder->sym-set b)))

(define-syntax-rule (for*/sym-set (clause ...) body ...)
  (let ([b (make-sym-set-builder)])
    (for* (clause ...)
      (call-with-values
       (lambda () body ...)
       (case-lambda
         [(v) (builder-add! b v)]
         [(v g) (builder-add! b v g)])))
    (builder->sym-set b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test helpers

(define (check-guard-equiv! actual expected)
  (check-true (guard-equiv? actual expected)))
(define (check-guard-true! g)  (check-true (guard-known-true? g)))
(define (check-guard-false! g) (check-true (guard-known-false? g)))

(define (fresh) (guard-var 1/2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash test suite

(define hash-tests
  (test-suite
   "my-hash"

   (test-case "construction from key/guard pairs"
              (define h (my-hash 'a (guard-true) 'b (guard-false)))
              (check-guard-true! (my-hash-has-key? h 'a))
              (check-guard-false! (my-hash-has-key? h 'b)))

   (test-case "duplicate key disjoins guards"
              (define x (fresh))
              (define y (fresh))
              (define h (my-hash 'a x 'a y))
              (check-guard-equiv! (my-hash-has-key? h 'a) (guard-or x y)))

   (test-case "my-hash-has-key? on an absent key is false"
              (define h (my-hash 'a (guard-true)))
              (check-guard-false! (my-hash-has-key? h 'b)))

   (test-case "my-hash-set adds a key unconditionally"
              (define h (my-hash-set (hash) 'a))
              (check-guard-true! (my-hash-has-key? h 'a)))

   (test-case "my-hash-set under a guard adds conditionally"
              (define x (fresh))
              (define h (my-hash-set (hash) 'a x))
              (check-guard-equiv! (my-hash-has-key? h 'a) x))

   (test-case "my-hash-set on an existing key disjoins"
              (define x (fresh))
              (define y (fresh))
              (define h (my-hash-set (my-hash-set (hash) 'a x) 'a y))
              (check-guard-equiv! (my-hash-has-key? h 'a) (guard-or x y)))

   (test-case "my-hash-remove drops a key"
              (define h (my-hash-remove (my-hash 'a (guard-true)) 'a))
              (check-guard-false! (my-hash-has-key? h 'a)))

   (test-case "my-hash-remove under a guard narrows"
              (define x (fresh))
              (define h (my-hash-remove (my-hash 'a (guard-true)) 'a x))
              (check-guard-equiv! (my-hash-has-key? h 'a) (guard-not x)))

   (test-case "my-hash-remove ignores keys never present"
              (define h (my-hash-remove (my-hash 'a (guard-true)) 'b))
              (check-guard-true! (my-hash-has-key? h 'a))
              (check-guard-false! (my-hash-has-key? h 'b)))

   (test-case "my-hash-union combines disjoint keys"
              (define h (my-hash-union (my-hash 'a (guard-true))
                                       (my-hash 'b (guard-true))))
              (check-guard-true! (my-hash-has-key? h 'a))
              (check-guard-true! (my-hash-has-key? h 'b)))

   (test-case "my-hash-union ORs guards on overlapping keys"
              (define x (fresh))
              (define y (fresh))
              (define h (my-hash-union (my-hash 'a x) (my-hash 'a y)))
              (check-guard-equiv! (my-hash-has-key? h 'a) (guard-or x y)))

   (test-case "my-hash-intersect keeps only common keys"
              (define h (my-hash-intersect (my-hash 'a (guard-true) 'b (guard-true))
                                           (my-hash 'b (guard-true))))
              (check-guard-false! (my-hash-has-key? h 'a))
              (check-guard-true! (my-hash-has-key? h 'b)))

   (test-case "my-hash-intersect ANDs guards"
              (define x (fresh))
              (define y (fresh))
              (define h (my-hash-intersect (my-hash 'a x) (my-hash 'a y)))
              (check-guard-equiv! (my-hash-has-key? h 'a) (guard-and x y)))

   (test-case "my-hash-keys-subset? true case"
              (check-guard-true!
               (my-hash-keys-subset? (my-hash 'a (guard-true))
                                     (my-hash 'a (guard-true) 'b (guard-true)))))

   (test-case "my-hash-keys-subset? false: missing key"
              (check-guard-false!
               (my-hash-keys-subset? (my-hash 'a (guard-true))
                                     (my-hash 'b (guard-true)))))

   (test-case "my-hash-keys-subset? checks guard implication"
              (define x (fresh))
              ;; present under x on the left, always on the right: holds
              (check-guard-true!
               (my-hash-keys-subset? (my-hash 'a x) (my-hash 'a (guard-true))))
              ;; the other way round, only where x holds
              (check-guard-equiv!
               (my-hash-keys-subset? (my-hash 'a (guard-true)) (my-hash 'a x)) x))

   (test-case "my-hash-empty? true for an empty hash"
              (check-guard-true! (my-hash-empty? (hash))))

   (test-case "my-hash-empty? false when an entry is unconditional"
              (check-guard-false! (my-hash-empty? (my-hash 'a (guard-true)))))

   (test-case "my-hash-empty? is the negation of the only guard"
              (define x (fresh))
              (check-guard-equiv! (my-hash-empty? (my-hash 'a x)) (guard-not x)))

   (test-case "my-hash-equal? compares guards, not structure"
              (define x (fresh))
              ;; (x or x) is a different expression but the same guard
              (check-true (my-hash-equal? (my-hash 'a x)
                                          (my-hash 'a (guard-or x x))))
              (check-false (my-hash-equal? (my-hash 'a x)
                                           (my-hash 'a (guard-not x)))))

   (test-case "my-hash-equal? treats an absent key as a false guard"
              (check-true (my-hash-equal? (hash) (my-hash 'a (guard-false))))
              (check-false (my-hash-equal? (hash) (my-hash 'a (guard-true)))))

   (test-case "my-hash-equal? can be restricted to given keys"
              ;; 'b differs, but comparing only 'a ignores that
              (check-true (my-hash-equal? (my-hash 'a (guard-true) 'b (guard-true))
                                          (my-hash 'a (guard-true))
                                          '(a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set test suite

(define set-tests
  (test-suite
   "sym-set"

   (test-case "membership"
              (define s (set 1 2))
              (check-guard-true! (set-member? s 1))
              (check-guard-false! (set-member? s 3)))

   (test-case "set-add introduces a new element"
              (define s (set-add (set 1) 2))
              (check-guard-true! (set-member? s 2)))

   (test-case "set-add with a guard inserts conditionally"
              (define x (fresh))
              (define s (set-add (set) 1 x))
              (check-guard-equiv! (set-member? s 1) x))

   (test-case "set-add twice under different guards disjoins"
              (define x (fresh))
              (define y (fresh))
              (define s (set-add (set-add (set) 1 x) 1 y))
              (check-guard-equiv! (set-member? s 1) (guard-or x y)))

   (test-case "for/sym-set with a single-value body"
              (define s (for/sym-set ([i (in-range 3)]) i))
              (for ([i (in-range 3)]) (check-guard-true! (set-member? s i)))
              (check-guard-false! (set-member? s 3)))

   (test-case "for/sym-set with a two-value body (element, guard)"
              (define x (fresh))
              (define s (for/sym-set ([i (in-range 2)]) (values i x)))
              (check-guard-equiv! (set-member? s 0) x)
              (check-guard-equiv! (set-member? s 1) x))

   (test-case "for/sym-set ORs guards for duplicate elements"
              (define x (fresh))
              (define y (fresh))
              (define s (for/sym-set ([g (in-list (list x y))]) (values 1 g)))
              (check-guard-equiv! (set-member? s 1) (guard-or x y)))

   (test-case "for/sym-set honours #:when"
              (define s (for/sym-set ([i (in-range 4)] #:when (even? i)) i))
              (check-guard-true! (set-member? s 2))
              (check-guard-false! (set-member? s 1)))

   (test-case "for*/sym-set iterates nested clauses"
              (define s (for*/sym-set ([i (in-range 2)] [j (in-range 2)])
                          (cons i j)))
              (for* ([i (in-range 2)] [j (in-range 2)])
                (check-guard-true! (set-member? s (cons i j))))
              (check-guard-false! (set-member? s (cons 9 9))))

   (test-case "for*/sym-set ORs guards for duplicate elements"
              (define x (fresh))
              (define y (fresh))
              (define s (for*/sym-set ([g (in-list (list x y))] [k (in-range 1)])
                          (values k g)))
              (check-guard-equiv! (set-member? s 0) (guard-or x y)))

   (test-case "set-empty?"
              (check-guard-true! (set-empty? (set)))
              (check-guard-false! (set-empty? (set 1)))
              (define x (fresh))
              (check-guard-equiv! (set-empty? (set-add (set) 1 x)) (guard-not x)))

   (test-case "set-remove narrows membership"
              (check-guard-false! (set-member? (set-remove (set 1) 1) 1))
              (define x (fresh))
              (check-guard-equiv! (set-member? (set-remove (set 1) 1 x) 1)
                                  (guard-not x)))

   (test-case "set-union"
              (define x (fresh))
              (define y (fresh))
              (define s (set-union (set-add (set) 1 x) (set-add (set) 1 y)))
              (check-guard-equiv! (set-member? s 1) (guard-or x y)))

   (test-case "set-intersect"
              (define x (fresh))
              (define y (fresh))
              (define s (set-intersect (set-add (set) 1 x) (set-add (set) 1 y)))
              (check-guard-equiv! (set-member? s 1) (guard-and x y)))

   (test-case "set-subtract removes every element of the argument sets"
              (define s (set-subtract (set 1 2 3) (set 2 3)))
              (check-guard-true! (set-member? s 1))
              (check-guard-false! (set-member? s 2))
              (check-guard-false! (set-member? s 3)))

   (test-case "subset? true and false cases"
              (check-guard-true! (subset? (set 1) (set 1 2)))
              (check-guard-false! (subset? (set 1 2) (set 1))))

   (test-case "set-equal? sees through syntactically different guards"
              (define x (fresh))
              (check-true (set-equal? (set-add (set) 1 x)
                                      (set-add (set) 1 (guard-or x x))))
              (check-false (set-equal? (set-add (set) 1 x)
                                       (set-add (set) 1 (guard-not x)))))

   (test-case "a sym-set is a sequence of element and guard"
              (define s (set 1 2))
              (check-equal? (sort (for/list ([(k g) s]) k) <) '(1 2))
              (for ([(k g) s]) (check-guard-true! g)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test

(define (run-all-tests)
  (run-tests hash-tests)
  (run-tests set-tests))

(module+ test
  (run-all-tests))
