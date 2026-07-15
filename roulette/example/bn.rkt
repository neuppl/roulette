#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide make-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rosette assert)
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         rosette
         syntax/readerr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct entry (depends tree))

(define DEFAULT (gensym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexer

(define-tokens non-empty-tokens
  (word decimal floating keyword))

(define-empty-tokens empty-tokens
  (end eof lp rp lc rc lb rb))

(define-lex-abbrevs
  [word (:- (:+ (:or letter digit)) keyword)]
  [letter (:or (:/ "a" "z") (:/ "A" "Z") "_" "-")]
  [digit (:/ "0" "9")]
  [decimal (:: (:/ "1" "9") (:* digit))]
  [floating (:or (:: (:+ digit) "." (:* digit) (:? exponent))
                 (:: "." (:+ digit) (:? exponent))
                 (:: (:+ digit) (:? exponent)))]
  [exponent (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit))]
  [keyword (:: (:or "network" "variable" "probability" "property"
                    "type" "discrete" "default" "table"))]
  [space (:or #\space #\tab #\newline #\, #\|)])

(define bif-lex
  (lexer-src-pos
   [word (token-word (string->symbol lexeme))]
   [decimal (token-decimal (string->number lexeme))]
   [floating (token-decimal (string->number lexeme))]
   [keyword (token-keyword (string->symbol lexeme))]
   ["(" (token-lp)] [")" (token-rp)]
   ["{" (token-lc)] ["}" (token-rc)]
   ["[" (token-lb)] ["]" (token-rb)]
   [";" (token-end)]
   [space (return-without-pos (bif-lex input-port))]
   [(eof) (token-eof)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser

(define (bif-parser src-name)
  (parser
   (start blocks)
   (end eof)
   (tokens non-empty-tokens empty-tokens)
   (src-pos)
   (error (make-read-error src-name))
   (grammar
    (blocks [(block blocks) (cons $1 $2)]
            [() null])
    (block [(keyword lp exprs rp lc properties rc) (list* $1 $3 $6)]
           [(keyword word lc properties rc) (list* $1 $2 $4)]
           [(keyword lc properties rc) (cons $1 $3)])
    (exprs [(expr exprs) (cons $1 $2)]
           [() null])
    (expr [(word) $1]
          [(decimal) $1]
          [(floating) $1]
          [(keyword) $1]
          [(group) $1])
    (group [(lp exprs rp) $2]
           [(lc exprs rc) $2]
           [(lb exprs rb) $2])
    (properties [(exprs end properties) (cons $1 $3)]
                [() null]))))

(define (make-read-error src-name)
  (λ (tok-ok? tok-name tok-value start end)
    (match-define (position start-offset start-line start-col) start)
    (match-define (position end-offset end-line end-col) end)
    (define span (and start-offset end-offset (- end-offset start-offset)))
    (raise-read-error "read" src-name start-line start-col start-offset span)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interpreter

(define ((make-run e) query-vars [var-order #f])
  (let go ([e e] [vars (hash)] [probs (hash)])
    (match e
      [(list)
       (define result (eval-blks vars probs var-order))
       (apply values (map (curry hash-ref result) query-vars))]
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
;; bin-cat

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
;; reader

(module+ reader
  (provide
   (rename-out [bn-read read]
               [bn-read-syntax read-syntax])))

(define (bn-read in)
  (syntax->datum (bn-read-syntax #f in)))

(define (bn-vars forms)
  (match forms
    [(list) null]
    [(cons `(variable ,name . ,_) rst)
     (cons name (bn-vars rst))]
    [(cons _ rst)
     (bn-vars rst)]))

(define (bn-read-syntax src in)
  (define forms (bn-parse src in))
  (with-syntax ([(var ...) (bn-vars forms)])
    #`(module _ roulette/example/disrupt
        (provide var ...)
        (require (only-in roulette/example/bn))
        (define run (make-run '#,forms))
        (define-values (var ...)
          (run '(var ...))))))

(define (bn-parse [source-name (object-name (current-input-port))]
                  [port (current-input-port)])
  (define parse (bif-parser source-name))
  (parse (λ () (bif-lex port))))
