#lang rosette

(require racket/hash
         rackunit
         (only-in roulette/example/disrupt flatten-symbolic)
         (for-syntax syntax/parse)
         rackunit/text-ui
         racket/pretty
         (prefix-in rkt: racket/set))


(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash merging

(struct guarded-entry (value path-condition))

(define (merge-symbolic-hash sym-ht)
  (define contents (if (union? sym-ht) (union-contents sym-ht) (list (cons #t sym-ht))))
  (for*/fold ([acc (hash)])
             ([gv contents]
              [(key entry) (in-hash (cdr gv))])
    (define guard (&& (car gv) (guarded-entry-path-condition entry)))
    (define value (guarded-entry-value entry))
    (if (hash-has-key? acc key)
        (let ([existing (hash-ref acc key)])
          (hash-set acc key
                    (guarded-entry (if guard value (guarded-entry-value existing))
                                   (|| (guarded-entry-path-condition existing) guard))))
        (hash-set acc key (guarded-entry value guard)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities/helpers

(define (force-ref ht key)
  (let ([out (hash-ref ht key)])
    (if (guarded-entry? out)
        (begin
          (assert (guarded-entry-path-condition out))
          (guarded-entry-value out))
        out)))

(define (dispatch-guarded gvs proc)
  (match gvs
    [(list (cons _ v)) (proc v)]
    [(cons (cons g v) rest)
     (if g (proc v) (dispatch-guarded rest proc))]))

(define-syntax-rule (for/all/flat ([id val-expr]) body ...)
  (dispatch-guarded
   (hash-map (flatten-symbolic val-expr) (λ (v g) (cons g v)))
   (λ (id) body ...)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash functions

(define-syntax (my-hash stx)
  (syntax-parse stx
    [(_ (~seq key value) ...)
     #'(for*/fold ([ht (hash)])
                  ([(k v) (in-parallel (list key ...) (list value ...))]
                   [(key-value key-guard) (in-hash (flatten-symbolic k))])
         (define existing (hash-ref ht key-value #f))
         (define old-val (if existing (guarded-entry-value existing) v))
         (define old-pc  (if existing (guarded-entry-path-condition existing) #f))
         (hash-set ht key-value
                   (guarded-entry (if key-guard v old-val)
                                  (|| key-guard old-pc))))]))

(define (my-hash-ref ht key)
  (if (concrete? key) 
      (force-ref ht key)
      (for/all/flat ([v key])
        (force-ref ht v))))

(define (my-hash-has-key? ht key)
  (if (concrete? key)
      (and (hash-has-key? ht key)
           (guarded-entry-path-condition (hash-ref ht key)))
      (for/fold ([acc #t])
                ([(key-value key-guard) (in-hash (flatten-symbolic key))])
        (&& acc (=> key-guard (my-hash-has-key? ht key-value))))))

;; Optional guard argument is a workaround since setting a hash key under a
;; unary condition isn't possible.
;; ie. (when x (my-hash-set ...)) creates an unexpected symbolic union
;; at the top level, with one of the entries as <void>

(define (my-hash-set ht key value [guard #t])
  (if (and (concrete? key) (concrete? guard) guard)
      (hash-set ht key (guarded-entry value #t))
      (for/fold ([acc ht])
                ([(key-value key-guard) (in-hash (flatten-symbolic key))])
        (define combined-guard (&& key-guard guard))
        (define existing (hash-ref acc key-value #f))
        (define old-val (if existing (guarded-entry-value existing) value))
        (define old-pc  (if existing (guarded-entry-path-condition existing) #f))
        (hash-set acc key-value
                  (guarded-entry (if combined-guard value old-val)
                                 (|| combined-guard old-pc))))))

(define (my-hash-remove ht key)
  (if (concrete? key)
      (hash-remove ht key)
      (for/fold ([acc ht])
                ([(key-value key-guard) (in-hash (flatten-symbolic key))])
        (if (hash-has-key? acc key-value)
            (let* ([entry (hash-ref acc key-value)]
                   [val (guarded-entry-value entry)]
                   [pc (guarded-entry-path-condition entry)])
              (hash-set acc key-value (guarded-entry val (&& pc (! key-guard)))))
            acc))))


(define (my-hash-union ht . hts)
  (for*/fold ([acc ht])
             ([h hts]
              [(key entry) (in-hash h)])
    (if (hash-has-key? acc key)
        (let* ([existing (hash-ref acc key)]
               [existing-val (guarded-entry-value existing)]
               [existing-pc (guarded-entry-path-condition existing)]
               [new-val (guarded-entry-value entry)]
               [new-pc (guarded-entry-path-condition entry)])
          (hash-set acc key
                    (guarded-entry (if new-pc new-val existing-val)
                                   (|| existing-pc new-pc))))
        (hash-set acc key entry))))


(define (my-hash-intersect ht . hts)
  (for/fold ([acc ht])
            ([h hts])
    (for/fold ([acc2 (hash)])
              ([(key entry) (in-hash acc)])
      (if (hash-has-key? h key)
          (let* ([val (guarded-entry-value entry)]
                 [pc (guarded-entry-path-condition entry)]
                 [other (hash-ref h key)]
                 [other-val (guarded-entry-value other)]
                 [other-pc (guarded-entry-path-condition other)])
            (hash-set acc2 key
                      (guarded-entry (if pc val other-val)
                                     (&& pc other-pc))))
          acc2))))


(define (my-hash-keys-subset? ht1 ht2)
  (and (hash-keys-subset? ht1 ht2)
       (for/fold ([acc #t])
                 ([(key entry1) (in-hash ht1)])
         (&& acc
             (=> (guarded-entry-path-condition entry1)
                 (guarded-entry-path-condition (hash-ref ht2 key)))))))

(define (my-hash-empty? ht)
  (or (hash-empty? ht)
      (for/fold ([acc #t])
                ([(_ entry) (in-hash ht)])
        (&& acc (! (guarded-entry-path-condition entry))))))

(define (my-hash-count ht)
  (for/fold
   ([acc 0])
   ([(_ entry) (in-hash ht)])
    (+ acc
       (if (guarded-entry-path-condition entry)
           1
           0))))

(define total-my-hash-equal?-time 0.0)

(define (my-hash-equal? ht1 ht2
                        [keys (rkt:set-union (rkt:list->set (hash-keys ht1))
                                             (rkt:list->set (hash-keys ht2)))])
  (define start (current-inexact-monotonic-milliseconds))
  (define result
    (for/and ([key keys])
      (let* ([e1 (hash-ref ht1 key #f)]
             [e2 (hash-ref ht2 key #f)]
             [entry1-pc (if e1 (guarded-entry-path-condition e1) #f)]
             [entry1-value (if e1 (guarded-entry-value e1) #f)]
             [entry2-pc (if e2 (guarded-entry-path-condition e2) #f)]
             [entry2-value (if e2 (guarded-entry-value e2) #f)]
             [clause (&& (<=> entry1-pc entry2-pc)
                         (=> entry1-pc (equal? entry1-value entry2-value)))])
        (if (and (concrete? entry1-pc) (concrete? entry2-pc)
                 (concrete? entry1-value) (concrete? entry2-value))
            (and (eq? entry1-pc entry2-pc) (equal? entry1-value entry2-value))
            (unsat? (verify (assert clause)))))))
  (define elapsed (- (current-inexact-monotonic-milliseconds) start))
  (set! total-my-hash-equal?-time (+ total-my-hash-equal?-time elapsed))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets

(struct sym-set (ht)
  #:transparent
  #:property prop:sequence
  (lambda (s)
    (define contents
      (let ([h (sym-set-ht s)])
        (if (union? h) (union-contents h) (list (cons #t h)))))
    (define pairs
      (for*/list ([gv contents]
                  [(k entry) (in-hash (cdr gv))])
        (cons k (&& (car gv) (guarded-entry-path-condition entry)))))
    (in-parallel (in-list (map car pairs)) (in-list (map cdr pairs))))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (parameterize ([pretty-printing #f])
       (define (write-elem k)
         (if (eq? mode #f) (display k port) (write k port)))
       (define contents
         (let ([h (sym-set-ht self)])
           (if (union? h) (union-contents h) (list (cons #t h)))))
       (write-string "(sym-set" port)
       (for* ([gv contents]
              [(k entry) (in-hash (cdr gv))])
         (write-string "\n  " port)
         (define g (&& (car gv) (guarded-entry-path-condition entry)))
         (cond
           [(and (concrete? g) (equal? g #t)) (write-elem k)]
           [else
            (write-string "[" port)
            (write-elem g)
            (write-string " " port)
            (write-elem k)
            (write-string "]" port)]))
       (write-string ")" port)))])


(define-syntax (set stx)
  (syntax-parse stx
    [(_ elem ...)
     #:with (pr ...) (apply append (map (λ (e) (list e #'#t)) (syntax->list #'(elem ...))))
     #'(sym-set (my-hash pr ...))]))


(define (merge-symbolic-set sym-set-val)
  (sym-set (merge-symbolic-hash (sym-set-ht sym-set-val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set functions

(define (set-member? st v)
  (my-hash-has-key? (sym-set-ht st) v))

(define (set-add st v [guard #t])
  (sym-set (my-hash-set (sym-set-ht st) v #t guard)))

(define (set-count st)
  (my-hash-count (sym-set-ht st)))

(define (set-empty? st)
  (my-hash-empty? (sym-set-ht st)))

(define (set-remove st v)
  (sym-set (my-hash-remove (sym-set-ht st) v)))

(define (set-union st . sts)
  (sym-set (apply my-hash-union (sym-set-ht st) (map sym-set-ht sts))))

(define (set-intersect st . sts)
  (sym-set (apply my-hash-intersect (sym-set-ht st) (map sym-set-ht sts))))

(define (set-subtract st0 . sts)
  (for*/fold ([acc st0])
             ([st sts]
              [key (in-hash-keys (sym-set-ht st))])
    (set-remove acc key)))

(define-syntax-rule (for/sym-set (clause ...) body ...)
  (for/fold ([acc (set)])
            (clause ...)
    (call-with-values
     (lambda () body ...)
     (case-lambda
       [(v) (set-add acc v)]
       [(v g) (set-add acc v g)]))))

(define-syntax-rule (for*/sym-set (clause ...) body ...)
  (for*/fold ([acc (set)])
             (clause ...)
    (call-with-values
     (lambda () body ...)
     (case-lambda
       [(v) (set-add acc v)]
       [(v g) (set-add acc v g)]))))

(define (subset? st1 st2)
  (my-hash-keys-subset? (sym-set-ht st1) (sym-set-ht st2)))


(define (set-equal? st1 st2 [keys #f])
  (if keys
      (my-hash-equal? (sym-set-ht st1) (sym-set-ht st2) keys)
      (my-hash-equal? (sym-set-ht st1) (sym-set-ht st2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test helpers

;; Check that a symbolic/concrete boolean formula is logically equivalent
;; to `expected`, without polluting the ambient vc.
(define (check-formula-equiv! actual expected)
  (check-true (unsat? (verify (assert (<=> actual expected))))))

;; Check that two (possibly symbolic) values are guaranteed equal.
(define (check-sym-equal! actual expected)
  (check-true (unsat? (verify (assert (equal? actual expected))))))

;; Run body with a clean vc, and restore/clear it afterward so assertions
;; made by my-hash-ref/force-ref don't leak into later tests.
(define-syntax-rule (with-clean-vc body ...)
  (dynamic-wind
   (λ () (clear-vc!))
   (λ () body ...)
   (λ () (clear-vc!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash test suite

(define hash-tests
  (test-suite
   "my-hash"

   (test-case "concrete construction and ref"
              (define h (my-hash 'a 1 'b 2))
              (check-equal? (force-ref h 'a) 1)
              (check-equal? (force-ref h 'b) 2))

   (test-case "duplicate concrete key: later write wins"
              (define h (my-hash 'a 1 'a 2))
              (check-equal? (force-ref h 'a) 2))

   (test-case "later symbolic key only overwrites in the worlds it applies to"
              (define-symbolic x1 boolean?)
              (define h (my-hash 'a 1 (if x1 'a 'b) 2))
              (with-clean-vc
                  (check-sym-equal! (force-ref h 'a) (if x1 2 1)))
              (with-clean-vc
                  (check-equal? (force-ref h 'b) 2)))

   (test-case "symbolic key construction flattens into multiple entries"
              (define-symbolic x2 boolean?)
              (define h (my-hash (if x2 'a 'b) 1))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-has-key? h 'a) x2))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-has-key? h 'b) (! x2))))

   (test-case "my-hash-ref on concrete key"
              (define h (my-hash 'a 1))
              (with-clean-vc
                  (check-equal? (my-hash-ref h 'a) 1)))

   (test-case "my-hash-ref on symbolic key merges across branches"
              (define-symbolic x3 boolean?)
              (define h (my-hash 'a 1 'b 2))
              (with-clean-vc
                  (check-sym-equal! (my-hash-ref h (if x3 'a 'b)) (if x3 1 2))))

   (test-case "my-hash-has-key? concrete present/absent"
              (define h (my-hash 'a 1))
              (check-equal? (my-hash-has-key? h 'a) #t)
              (check-equal? (my-hash-has-key? h 'b) #f))

   (test-case "my-hash-has-key? symbolic key ANDs implications"
              (define-symbolic x4 boolean?)
              (define h (my-hash 'a 1))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-has-key? h (if x4 'a 'b)) x4)))

   (test-case "my-hash-set adds a new concrete key"
              (define h (my-hash 'a 1))
              (define h2 (my-hash-set h 'b 2))
              (check-equal? (force-ref h2 'a) 1)
              (check-equal? (force-ref h2 'b) 2))

   (test-case "my-hash-set with symbolic key only overwrites conditionally"
              (define-symbolic x5 boolean?)
              (define h (my-hash 'a 1))
              (define h2 (my-hash-set h (if x5 'a 'b) 2))
              (with-clean-vc
                  (check-sym-equal! (force-ref h2 'a) (if x5 2 1))))

   (test-case "my-hash-remove concrete key"
              (define h (my-hash 'a 1 'b 2))
              (define h2 (my-hash-remove h 'a))
              (check-false (my-hash-has-key? h2 'a))
              (check-true (my-hash-has-key? h2 'b)))

   (test-case "my-hash-remove with symbolic key narrows presence conditionally"
              (define-symbolic x6 boolean?)
              (define h (my-hash 'a 1))
              (define h2 (my-hash-remove h (if x6 'a 'b)))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-has-key? h2 'a) (! x6))))

   (test-case "my-hash-remove ignores keys never present"
              (define h (my-hash 'a 1))
              ;; should not error even though 'z was never a key
              (check-not-exn (λ () (my-hash-remove h 'z))))

   (test-case "my-hash-union combines disjoint keys"
              (define h1 (my-hash 'a 1))
              (define h2 (my-hash 'b 2))
              (define u (my-hash-union h1 h2))
              (check-true (my-hash-has-key? u 'a))
              (check-true (my-hash-has-key? u 'b)))

   (test-case "my-hash-union ORs guards on overlapping keys"
              (define-symbolic x7 x8 boolean?)
              (define h1 (my-hash (if x7 'a 'z) 1))
              (define h2 (my-hash (if x8 'a 'w) 2))
              (define u (my-hash-union h1 h2))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-has-key? u 'a) (|| x7 x8))))

   (test-case "my-hash-intersect keeps only common keys"
              (define h1 (my-hash 'a 1 'b 2))
              (define h2 (my-hash 'b 3 'c 4))
              (define i (my-hash-intersect h1 h2))
              (check-false (my-hash-has-key? i 'a))
              (check-true (my-hash-has-key? i 'b))
              (check-false (my-hash-has-key? i 'c)))

   (test-case "my-hash-intersect ANDs guards"
              (define-symbolic x9 x10 boolean?)
              (define h1 (my-hash (if x9 'a 'z) 1))
              (define h2 (my-hash (if x10 'a 'w) 2))
              (define i (my-hash-intersect h1 h2))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-has-key? i 'a) (&& x9 x10))))

   (test-case "my-hash-keys-subset? true case"
              (define h1 (my-hash 'a 1))
              (define h2 (my-hash 'a 1 'b 2))
              (check-true (my-hash-keys-subset? h1 h2)))

   (test-case "my-hash-keys-subset? false: missing key"
              (define h1 (my-hash 'a 1 'z 9))
              (define h2 (my-hash 'a 1))
              (check-false (my-hash-keys-subset? h1 h2)))

   (test-case "my-hash-keys-subset? checks guard implication, not just key presence"
              (define-symbolic x11 boolean?)
              (define h1 (my-hash 'a 1))
              (define h2 (my-hash-remove (my-hash 'a 1) (if x11 'a 'z)))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-keys-subset? h1 h2) (! x11))))

   (test-case "my-hash-count on concrete-presence entries"
              (define h (my-hash 'a 1 'b 2 'c 3))
              (check-equal? (my-hash-count h) 3))

   (test-case "my-hash-count sums to a symbolic ite when guards are symbolic"
              (define-symbolic x12 boolean?)
              (define h (my-hash (if x12 'a 'b) 1))
              ;; exactly one of 'a/'b present at a time -> count is always 1
              (with-clean-vc
                  (check-sym-equal! (my-hash-count h) 1)))

   (test-case "my-hash-empty? true for empty hash"
              (check-true (my-hash-empty? (hash))))

   (test-case "my-hash-empty? false when a concrete entry is present"
              (define h (my-hash 'a 1))
              (check-false (my-hash-empty? h)))

   (test-case "my-hash-empty? symbolic: empty iff no branch's guard holds"
              (define-symbolic x13 boolean?)
              (define h (my-hash-remove (my-hash 'a 1) (if x13 'a 'z)))
              ;; empty exactly when x13 holds (that's when 'a actually got removed)
              (with-clean-vc
                  (check-formula-equiv! (my-hash-empty? h) x13)))

   (test-case "merge-symbolic-hash on a union of two hashes"
              (define-symbolic x14 boolean?)
              (define h1 (my-hash 'a 1 'only1 10))
              (define h2 (my-hash 'a 2 'only2 20))
              (define merged (merge-symbolic-hash (if x14 h1 h2)))
              (with-clean-vc
                  (check-sym-equal! (force-ref merged 'a) (if x14 1 2)))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-has-key? merged 'only1) x14))
              (with-clean-vc
                  (check-formula-equiv! (my-hash-has-key? merged 'only2) (! x14))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set test suite

(define set-tests
  (test-suite
   "sym-set"

   (test-case "concrete membership"
              (define s (set 1 2 3))
              (check-true (set-member? s 1))
              (check-false (set-member? s 4)))

   (test-case "symbolic membership"
              (define-symbolic x15 boolean?)
              (define s (set 1 2))
              (with-clean-vc
                  (check-formula-equiv! (set-member? s (if x15 1 4)) x15)))

   (test-case "set-add introduces a new element"
              (define s (set 1 2))
              (define s2 (set-add s 3))
              (check-true (set-member? s2 3)))

   (test-case "set-add with symbolic element doesn't disturb existing ones"
              (define-symbolic x16 boolean?)
              (define s (set 1))
              (define s2 (set-add s (if x16 1 2)))
              ;; 1 should remain unconditionally present regardless of x16
              (check-true (set-member? s2 1)))

   (test-case "set-add with a guard inserts conditionally"
              (define-symbolic x19 boolean?)
              (define s (set))
              (define s2 (set-add s 1 x19))
              (with-clean-vc
                  (check-formula-equiv! (set-member? s2 1) x19))
              (with-clean-vc
                  (check-sym-equal! (set-count s2) (if x19 1 0))))

   (test-case "for/sym-set with a single-value body"
              (define s (for/sym-set ([i (in-range 3)]) i))
              (check-true (set-member? s 0))
              (check-true (set-member? s 1))
              (check-true (set-member? s 2)))

   (test-case "for/sym-set with a single value that is itself symbolic"
              (define-symbolic x21 boolean?)
              (define s (for/sym-set ([i (in-list (list 1))]) (if x21 1 2)))
              ;; the element inserted is (if x21 1 2), not the guard —
              ;; both 1 and 2 should be present, each under the
              ;; appropriate branch of x21
              (with-clean-vc
                  (check-formula-equiv! (set-member? s 1) x21))
              (with-clean-vc
                  (check-formula-equiv! (set-member? s 2) (! x21))))

   (test-case "for/sym-set with a two-value body (element, guard)"
              (define-symbolic x22 boolean?)
              (define s (for/sym-set ([i (in-list (list 1 2 3))])
                          (values i (if (= i 2) x22 #t))))
              ;; 1 and 3 are unconditional; 2 is present only when x22
              (check-true (set-member? s 1))
              (check-true (set-member? s 3))
              (with-clean-vc
                  (check-formula-equiv! (set-member? s 2) x22)))

   (test-case "for/sym-set with both element and guard symbolic"
              (define-symbolic x23 boolean?)
              (define s (for/sym-set ([i (in-list (list 1))])
                          (values (if x23 10 20) x23)))
              ;; 10 is inserted only under guard x23; since the element
              ;; ITSELF is also (if x23 10 20), 10 should be present
              ;; exactly when x23 holds (both conditions coincide here)
              (with-clean-vc
                  (check-formula-equiv! (set-member? s 10) x23)))

   (test-case "set-count"
              (define s (set 1 2 3))
              (check-equal? (set-count s) 3))

   (test-case "set-empty?"
              (check-true (set-empty? (set)))
              (check-false (set-empty? (set 1))))

   (test-case "set-remove narrows membership"
              (define-symbolic x17 boolean?)
              (define s (set 1))
              (define s2 (set-remove s (if x17 1 2)))
              (with-clean-vc
                  (check-formula-equiv! (set-member? s2 1) (! x17))))

   (test-case "set-union"
              (define s1 (set 1 2))
              (define s2 (set 2 3))
              (define u (set-union s1 s2))
              (check-true (set-member? u 1))
              (check-true (set-member? u 2))
              (check-true (set-member? u 3)))

   (test-case "set-intersect"
              (define s1 (set 1 2 3))
              (define s2 (set 2 3 4))
              (define i (set-intersect s1 s2))
              (check-false (set-member? i 1))
              (check-true (set-member? i 2))
              (check-true (set-member? i 3))
              (check-false (set-member? i 4)))

   (test-case "set-subtract removes every element of the argument sets"
              (define s1 (set 1 2 3))
              (define s2 (set 2))
              (define s3 (set-subtract s1 s2))
              (check-true (set-member? s3 1))
              (check-false (set-member? s3 2))
              (check-true (set-member? s3 3)))

   (test-case "subset? true and false cases"
              (define s1 (set 1 2))
              (define s2 (set 1 2 3))
              (check-true (subset? s1 s2))
              (check-false (subset? s2 s1)))

   (test-case "merge-symbolic-set on a union of two sets"
              (define-symbolic x18 boolean?)
              (define s1 (set 1 2))
              (define s2 (set 2 3))
              (define merged (merge-symbolic-set (if x18 s1 s2)))
              (check-true (set-member? merged 2)) ; common to both, unconditional
              (with-clean-vc
                  (check-formula-equiv! (set-member? merged 1) x18))
              (with-clean-vc
                  (check-formula-equiv! (set-member? merged 3) (! x18))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test

(define (run-all-tests)
  (run-tests hash-tests)
  (run-tests set-tests))