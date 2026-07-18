#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 #%module-begin
 #%app
 #%datum
 fun
 int
 discrete
 == != && \|\| <=> ^
 (rename-out
  [not !]
  [bvadd +]
  [bvsub -]
  [bvlt <]
  [bvleq <=]
  [bvgt >]
  [bvgeq >=]
  [bvmul *]
  [bvdiv /]
  [bvmod %]
  [nth-bit nth_bit]

  [kons cons]
  [kons-car car]
  [kons-cdr cdr]
  [kons-list list]
  [kons-length length])
 <<
 >>
 not
 uniform
 binomial
 iterate
 flip
 observe!
 if
 let
 wrap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in : parser-tools/lex-sre)
         (only-in rosette assert)
         rosette/lib/destruct
         parser-tools/lex
         parser-tools/yacc
         syntax/readerr
         syntax/strip-context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; not implemented

(define (nth-bit _x) (error 'nth_bit "not implemented"))
(define (kons-length _x) (error 'length "not implemented"))
(define (bvmul _x _y) (error '* "not implemented"))
(define (bvdiv _x _y) (error '/ "not implemented"))
(define (bvmod _x _y) (error '% "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time

(define (wrap x)
  (cond
    [(list? x) (unint x)]
    [(mt? x) '()]
    [(kons? x) (cons (wrap (kons-car x)) (wrap (kons-cdr x)))]
    [else x]))

(define-syntax fun
  (syntax-parser
    [(_ (name:id [arg:id type] ...) body:expr)
     #'(define (name arg ...) body)]))

(define (int size k)
  (cond
    [(zero? size) '()]
    [else (cons (odd? k) (int (sub1 size) (truncate (/ k 2))))]))

(define (discrete . xs)
  (bin-cat
   (map cons (range (length xs)) xs)
   (bits-needed (length xs))))

(define (bits-needed x)
  (if (zero? x) 0 (inexact->exact (ceiling (log x 2)))))

(define (uniform size min-incl max-excl)
  (bin-cat
   (for/list ([k (in-range min-incl max-excl)])
     (cons k (/ 1 (- max-excl min-incl))))
   size))

(define (binomial size n p)
  (cond
    [(zero? n) (int size 0)]
    [else
     (define result (binomial size (sub1 n) p))
     (if (flip p) (bvadd result (int size 1)) result)]))

(define (iterate f val n)
  (if (zero? n)
      val
      (f (iterate f val (sub1 n)))))

(define == equal?)
(define != (compose not equal?))
(define (&& x y) (and x y))
(define (\|\| x y) (or x y))
(define (<=> x y) (equal? x y))
(define (^ x y) (xor x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list operations

(struct mt () #:transparent)
(struct kons (car cdr) #:transparent)

(define (kons-list . xs)
  (if (empty? xs)
      (mt)
      (kons (car xs) (apply kons-list (cdr xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arithmetic operations

(define (bvsub x y)
  (bvadd x (bvadd (map not y) (int (length x) 1))))

(define (bvgt xs ys) (bvlt ys xs))
(define (bvgeq xs ys) (or (bvlt ys xs) (equal? xs ys)))
(define (bvleq xs ys) (or (bvlt xs ys) (equal? xs ys)))
(define (bvlt xs ys)
  (let go ([xs (reverse xs)] [ys (reverse ys)])
    (match* (xs ys)
      [('() '()) #f]
      [((cons x xt) (cons y yt))
       (cond
         [(and (not x) y) #t]
         [(and x (not y)) #f]
         [else (go xt yt)])])))

(define (⊖ x y)
  (if (< x y) 0 (- x y)))

(define (>> x y)
  (define n (length x))
  (define amt (if (> y n) n y))
  (append
   (drop x amt)
   (map (const #f) (range amt))))

(define (<< x y)
  (define n (length x))
  (define amt (if (> y n) n y))
  (append
   (map (const #f) (range amt))
   (drop-right x amt)))

(define (unint xs)
  (destruct xs
    [(list) 0]
    [(cons x xt) (+ (if x 1 0) (* 2 (unint xt)))]))

(define (bvadd xs ys)
  (let go ([xs xs] [ys ys] [cin #f])
    (match* (xs ys)
      [('() '()) '()]
      [((cons x xt) (cons y yt))
       (define x⊕y (xor x y))
       (cons (xor x⊕y cin) (go xt yt (or (and x y) (and cin x⊕y))))])))

(define (bin-cat xs size)
  (match xs
    [(list) (assert #f)]
    [(list (cons x _)) (int size x)]
    [_
     (define-values (left right)
       (split-at xs (floor (/ (length xs) 2))))
     (define left-sum (foldl + 0 (map cdr left)))
     (if (flip left-sum)
         (bin-cat (renormalize left left-sum) size)
         (bin-cat (renormalize right (- 1 left-sum)) size))]))

(define (renormalize xs n)
  (for/list ([x+y (in-list xs)])
    (cons (car x+y) (/ (cdr x+y) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexer

(define-tokens non-empty-tokens
  (ident number keyword shift cmp arith-inner arith-outer))

(define-empty-tokens empty-tokens
  (eof fun comma colon semicolon bool int listt truet falset discrete fst snd flip
       observe ift then elset lett eq in iterate uniform binomial head tail length
       or and not iff xor cons
       lb rb lp rp lc rc))

(define-lex-abbrevs
  [ident (:- (:: letter (:* (:or letter digit))) keyword)]
  [letter (:or (:/ "a" "z") (:/ "A" "Z") "_")]
  [digit (:/ "0" "9")]
  [number (:or (:: (:+ digit) "." (:* digit))
               (:: "." (:+ digit))
               (:+ digit))]
  [space (:or whitespace comment)]
  [comment (:: "//" (:* (char-complement #\newline)) (:? #\newline))]
  [keyword (:or "fun" "bool" "int" "list" "true" "false" "discrete" "fst" "snd"
                "flip" "observe" "if" "then" "else" "let" "in"
                "iterate" "uniform" "binomial" "head" "tail" "length")])

(define keywords
  (hash "fun" token-fun
        "bool" token-bool
        "int" token-int
        "list" token-listt
        "true" token-truet
        "false" token-falset
        "discrete" token-discrete
        "fst" token-fst
        "snd" token-snd
        "flip" token-flip
        "observe" token-observe
        "if" token-ift
        "then" token-then
        "else" token-elset
        "let" token-lett
        "in" token-in
        "iterate" token-iterate
        "uniform" token-uniform
        "binomial" token-binomial
        "head" token-head
        "tail" token-tail
        "length" token-length))

(define dice-lex
  (lexer-src-pos
   [ident (token-ident (string->symbol lexeme))]
   [keyword ((hash-ref keywords lexeme))]
   [number (token-number (string->number lexeme))]
   ["||" (token-or)]
   ["&&" (token-and)]
   ["!" (token-not)]
   ["<=>" (token-iff)]
   ["^" (token-xor)]
   [(:or "<<" ">>") (token-shift (string->symbol lexeme))]
   [(:or "<=" ">=" "<" ">" "!=") (token-cmp (string->symbol lexeme))]
   ["::" (token-cons)]
   [(:or "+" "-" "==") (token-arith-inner (string->symbol lexeme))]
   [(:or "*" "/" "%") (token-arith-outer (string->symbol lexeme))]
   ["," (token-comma)]
   [":" (token-colon)]
   [";" (token-semicolon)]
   ["=" (token-eq)]
   ["[" (token-lb)] ["]" (token-rb)]
   ["(" (token-lp)] [")" (token-rp)]
   ["{" (token-lc)] ["}" (token-rc)]
   [space (return-without-pos (dice-lex input-port))]
   [(eof) (token-eof)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser

(define (dice-parser src-name)
  (parser
   (start program)
   (end eof)
   (tokens non-empty-tokens empty-tokens)
   (src-pos)
   (error (make-read-error src-name))
   (suppress)
   (precs
    [left or]
    [left and]
    [left not]
    [left iff]
    [left xor]
    [left cmp]
    [right cons]
    [left arith-inner]
    [left arith-outer])
   (grammar
    (program [(expr) `((wrap ,$1))]
             [(function program) (cons $1 $2)])
    (function [(fun ident lp params rp lc expr rc) `(fun (,$2 . ,$4) ,$7)]
              [(fun ident lp params rp colon type lc expr rc) `(fun (,$2 . ,$4) ,$9)])
    (params [(param comma params) (cons $1 $3)]
            [(param) (list $1)])
    (param [(ident colon type) (list $1 $3)])
    (type [(bool) 'bool]
          [(lp type comma type rp) `(pair ,$2 ,$4)]
          [(int lp number rp) `(int ,$3)]
          [(listt lp type rp) `(list ,$3)])
    (expr [(ident lp args rp) (cons $1 $3)]
          [(ident) $1]
          [(lp expr comma expr rp) `(cons ,$2 ,$4)]
          [(lp expr rp) $2]
          [(truet) #t]
          [(falset) #f]
          [(int lp number comma number rp) `(int ,$3 ,$5)]
          [(discrete lp probs rp) `(discrete . ,$3)]
          [(expr or expr) `(\|\| ,$1 ,$3)]
          [(expr and expr) `(&& ,$1 ,$3)]
          [(expr iff expr) `(<=> ,$1 ,$3)]
          [(expr xor expr) `(^ ,$1 ,$3)]
          [(expr cmp expr) `(,$2 ,$1 ,$3)]
          [(expr cons expr) `(cons ,$1 ,$3)]
          [(expr arith-inner expr) `(,$2 ,$1 ,$3)]
          [(expr arith-outer expr) `(,$2 ,$1 ,$3)]
          [(expr shift number) `(,$2 ,$1 ,$3)]
          [(fst expr) `(car ,$2)]
          [(snd expr) `(cdr ,$2)]
          [(not expr) `(not ,$2)]
          [(flip number) `(flip ,$2)]
          [(flip lp number rp) `(flip ,$3)]
          [(observe expr) `(observe! ,$2)]
          [(ift expr then expr elset expr) `(if ,$2 ,$4 ,$6)]
          [(lett ident eq expr in expr) `(let ([,$2 ,$4]) ,$6)]
          [(head expr) `(car ,$2)]
          [(tail expr) `(cdr ,$2)]
          [(length expr) `(length ,$2)]
          [(lb rb colon type) '(list)]
          [(lb args rb) `(list ,@$2)]
          [(iterate lp ident comma expr comma number rp) `(iterate ,$3 ,$5 ,$7)]
          [(uniform lp number comma number comma number rp) `(uniform ,$3 ,$5 ,$7)]
          [(binomial lp number comma number comma number rp) `(binomial ,$3 ,$5 ,$7)])
    (args [(expr comma args) (cons $1 $3)]
          [(expr) (list $1)])
    (probs [(number comma probs) (cons $1 $3)]
           [(number) (list $1)]))))

(define (make-read-error src-name)
  (λ (tok-ok? tok-name tok-value start end)
    (match-define (position start-offset start-line start-col) start)
    (match-define (position end-offset end-line end-col) end)
    (define span (and start-offset end-offset (- end-offset start-offset)))
    (raise-read-error "read" src-name start-line start-col start-offset span)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module+ reader
  (provide
   (rename-out [dice-read read]
               [dice-read-syntax read-syntax])))

(define (dice-read in)
  (syntax->datum (dice-read-syntax #f in)))

(define (dice-read-syntax src in)
  (define forms (dice-parse src in))
  (strip-context
   #`(module _ roulette/example/dice #,@forms)))

(define (dice-parse [source-name (object-name (current-input-port))]
                    [port (current-input-port)])
  (define parse (dice-parser source-name))
  (parse (λ () (dice-lex port))))
