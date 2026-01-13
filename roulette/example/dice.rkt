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
 + - * / < <= > >= == != && || <=> ^
 cons
 car
 cdr
 not
 flip
 observe!
 if
 let)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in : parser-tools/lex-sre)
         parser-tools/lex
         parser-tools/yacc
         syntax/readerr
         syntax/strip-context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time

(define-syntax fun
  (syntax-parser
    [(_ (name:id [arg:id type] ...) body:expr)
     #'(define (name arg ...) body)]))

(define (int size val)
  val)

(define (discrete . probs)
  (bin-cat
   (for/list ([k (in-naturals)]
              [prob (in-list probs)]
              #:unless (zero? prob))
     (cons k prob))))

(define == equal?)
(define != (compose negate equal?))
(define (&& x y) (and x y))
(define (|| x y) (or x y))
(define (<=> x y) (equal? x y))
(define (^ x y) (xor x y))

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
;; lexer

(define-tokens non-empty-tokens
  (ident binop number keyword))

(define-empty-tokens empty-tokens
  (eof fun comma colon bool int truet falset discrete fst snd bang flip
       observe ift then elset lett eq in lp rp lc rc))

(define-lex-abbrevs
  [ident (:- (:: letter (:* (:or letter digit))) keyword)]
  [letter (:or (:/ "a" "z") (:/ "A" "Z") "_" "-")]
  [digit (:/ "0" "9")]
  [number (:or (:: (:+ digit) "." (:* digit))
               (:: "." (:+ digit))
               (:: (:/ "1" "9") (:* digit)))]
  [binop (:or "+" "-" "*" "/" "<" "<=" ">" ">=" "==" "!=" "&&" "||" "<=>" "^")]
  [space (:or comment #\space #\tab #\newline)]
  [comment (:: "//" (:* (char-complement #\newline)) (:? #\newline))]
  [keyword (:or "fun" "bool" "int" "true" "false" "discrete" "fst" "snd"
                "flip" "observe" "if" "then" "else" "let" "in")])

(define keywords
  (hash "fun" token-fun
        "bool" token-bool
        "int" token-int
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
        "in" token-in))

(define dice-lex
  (lexer-src-pos
   [ident (token-ident (string->symbol lexeme))]
   [keyword ((hash-ref keywords lexeme))]
   [binop (token-binop (string->symbol lexeme))]
   [number (token-number (string->number lexeme))]
   ["," (token-comma)]
   [":" (token-colon)]
   ["!" (token-bang)]
   ["=" (token-eq)]
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
   (precs [left binop])
   (grammar
    (program [(expr) (list $1)]
             [(function program) (cons $1 $2)])
    (function [(fun ident lp params rp lc expr rc) `(fun (,$2 . ,$4) ,$7)])
    (params [(param comma params) (cons $1 $3)]
            [(param) (list $1)])
    (param [(ident colon type) (list $1 $3)])
    (type [(bool) 'bool]
          [(lp type comma type rp) `(pair ,$2 ,$4)]
          [(int lp number rp) `(int ,$3)])
    (expr [(ident lp args rp) (cons $1 $3)]
          [(ident) $1]
          [(lp expr comma expr rp) `(cons ,$2 ,$4)]
          [(lp expr rp) $2]
          [(truet) #t]
          [(falset) #f]
          [(int lp number comma number rp) `(int ,$3 ,$5)]
          [(discrete lp probs rp) `(discrete . ,$3)]
          [(expr binop expr) `(,$2 ,$1 ,$3)]
          [(fst expr) `(car ,$2)]
          [(snd expr) `(cdr ,$2)]
          [(bang expr) `(not ,$2)]
          [(flip number) `(flip ,$2)]
          [(observe expr) `(observe! ,$2)]
          [(ift expr then expr elset expr) `(if ,$2 ,$4 ,$6)]
          [(lett ident eq expr in expr) `(let ([,$2 ,$4]) ,$6)])
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
