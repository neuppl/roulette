#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 #%module-begin
 #%app
 #%datum
 blog-parse
 blog-lex
 type
 distinct
 fixed
 random
 call
 BooleanDistrib
 Categorical
 UniformInt
 Bernoulli
 + - * < <= > >=
 if
 =>
 !=
 obs 
(rename-out
 [blog-null null]
 [blog-case case]
 [blog-index index]
 [blog-map map]
 [blog-entry entry]
 [blog-query query]
 [blog-forall forall]
 [blog-exists exists]
 [equal? ==]
 [not !]
 [and &]
 [or \|])
 )

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
         syntax/strip-context
         (only-in racket/base [eq? base:eq?])
         (only-in roulette/private/util flatten-symbolic))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time

;; The descriptor is opaque; its name is for display and errors.

(struct blog-null-value ()
  #:methods gen:custom-write
  [(define (write-proc value port mode) (display "null" port))])
(define blog-null (blog-null-value))

;; Bind the selector once; branch expressions remain inside their conditions.
(define-syntax blog-case
  (syntax-parser
    [(_ selector:expr ((~datum entry) key:expr value:expr) ...)
     #'(let ([selected selector])
         (blog-case-branches selected (key value) ...))]))

(define-syntax blog-case-branches
  (syntax-parser
    [(_ selected) #'blog-null]
    [(_ selected (key value) rest ...)
     #'(if (equal? selected key)
           value
           (blog-case-branches selected rest ...))]))

(struct blog-type (name [objects #:mutable]))
(struct blog-object (type name))

(define-syntax blog-forall
  (syntax-parser
    [(_ ty:id variable:id body:expr)
     #'(for/fold ([result #t])
                 ([variable (in-list (blog-type-objects ty))])
         (and result body))]))

(define-syntax blog-exists
  (syntax-parser
    [(_ ty:id variable:id body:expr)
     #'(for/fold ([result #f])
                 ([variable (in-list (blog-type-objects ty))])
         (or result body))]))

(define-syntax type
  (syntax-parser
    [(_ name:id)
     #'(define name (blog-type 'name '()))]))

(define (register-object! ty name)
  (unless (blog-type? ty)
    (raise-argument-error 'distinct "blog-type?" ty))
  (define obj (blog-object ty name))
  (set-blog-type-objects!
   ty
   (append (blog-type-objects ty) (list obj)))
  obj)

(define-syntax distinct
  (syntax-parser
    [(_ ty:id declaration ...+)
     #'(begin
         (distinct-one ty declaration)
         ...)]))

;; Keep indexed families separate from individual objects in the type domain.
(struct blog-object-family (objects))

(define-syntax distinct-one
  (syntax-parser
    [(_ ty:id name:id)
     #'(define name (register-object! ty 'name))]
    [(_ ty:id ((~datum indexed) name:id count:exact-nonnegative-integer))
     #'(define name
         (begin
           (unless (blog-type? ty)
             (raise-argument-error 'distinct "blog-type?" ty))
           (blog-object-family
            (for/vector ([i (in-range count)])
              (register-object! ty (string->symbol (format "~a[~a]" 'name i)))))))]))

(define (blog-index family index)
  (unless (blog-object-family? family)
    (raise-argument-error 'index "indexed distinct object family" family))
  (define objects (blog-object-family-objects family))
  ;; Validate outside symbolic branches so invalid alternatives raise an error.
  (for ([(i guard) (in-hash (flatten-symbolic index))]
        #:unless (base:eq? guard #f))
    (unless (and (exact-integer? i) (<= 0 i) (< i (vector-length objects)))
      (raise-arguments-error 'index "index outside the distinct object family"
                             "index" i "size" (vector-length objects))))
  (for/all ([i index #:exhaustive])
    (vector-ref objects i)))

(define-syntax fixed
  (syntax-parser
    ;; No arguments: define a constant.
    [(_ result-type:id name:id () body:expr)
     #'(define name body)]

    ;; One or more arguments: define a function.
    [(_ result-type:id name:id
        ((arg-type:id arg:id) ...+) body:expr)
     #'(define (name arg ...) body)]))

;; Ground random functions over the objects declared so far. Objects and parent
;; functions must precede their dependents; later declarations are not included.
;; Return annotations are currently unchecked, as for fixed declarations.
(define-syntax random
  (syntax-parser
    [(_ result-type:id name:id params (~datum ~) body:expr)
     #'(define-random name params (draw-distribution body))]
    [(_ result-type:id name:id params (~datum =) body:expr)
     #'(define-random name params body)]))

(define-syntax define-random
  (syntax-parser
    [(_ name:id () body:expr) #'(define name body)]
    [(_ name:id ((arg-type:id arg:id) ...+) body:expr)
     #'(define name
         (make-random-function 'name (list arg-type ...)
                               (lambda (arg ...) body)))]))

(define (make-random-function name types instantiate)
  (define domains
    (for/list ([ty (in-list types)])
      (unless (blog-type? ty)
        (raise-argument-error name "blog-type?" ty))
      (blog-type-objects ty)))
  (define (tuples domains)
    (if (null? domains)
        (list '())
        (let ([tails (tuples (cdr domains))])
          (for*/list ([obj (in-list (car domains))]
                      [tail (in-list tails)])
            (cons obj tail)))))
  ;; Allocate once per concrete tuple, before any symbolic lookups.
  (define table
    (for/hash ([args (in-list (tuples domains))])
      (values args (apply instantiate args))))
  (procedure-reduce-arity
   (lambda args
     (let lookup ([remaining args] [reversed '()])
       (if (null? remaining)
           (hash-ref table (reverse reversed)
                     (lambda ()
                       (raise-arguments-error
                        name "arguments are outside the grounded object domains"
                        "arguments" args)))
           (for/all ([arg (car remaining) #:exhaustive])
             (lookup (cdr remaining) (cons arg reversed))))))
   (length types)))

;; Zero-argument BLOG functions are represented by values, including constants.
(define-syntax call
  (syntax-parser
    [(_ f:expr)
     #'(let ([value f]) (if (procedure? value) (value) value))]
    [(_ f:expr arg:expr ...+) #'(f arg ...)]))

(define (!= a b) (not (equal? a b)))

;; Distribution expressions describe deferred symbolic draws, not samples.
(struct blog-distribution (draw))

(define (draw-distribution distribution)
  (for/all ([distribution distribution #:exhaustive])
    (unless (or (blog-null-value? distribution) (blog-distribution? distribution))
      (raise-argument-error 'random "BLOG distribution" distribution))
    (if (blog-null-value? distribution)
        blog-null
        ((blog-distribution-draw distribution)))))

(define (check-probabilities who probabilities)
  (for ([(ps guard) (in-hash (flatten-symbolic probabilities))]
        #:unless (base:eq? guard #f))
    (unless (andmap (lambda (p) (and (real? p) (<= 0 p 1))) ps)
      (raise-argument-error who "probabilities between 0 and 1" ps))))

(define (BooleanDistrib probability)
  (check-probabilities 'BooleanDistrib (list probability))
  (blog-distribution (lambda () (flip probability))))

(define (Bernoulli probability)
  (check-probabilities 'Bernoulli (list probability))
  (blog-distribution (lambda () (if (flip probability) 1 0))))

;; Bounds may depend on finite random choices; validate correlated pairs together.
(define (UniformInt lo hi)
  (for ([(bounds guard) (in-hash (flatten-symbolic (list lo hi)))]
        #:unless (base:eq? guard #f))
    (define lower (car bounds))
    (define upper (cadr bounds))
    (unless (and (exact-integer? lower) (exact-integer? upper) (<= lower upper))
      (raise-arguments-error 'UniformInt "expected integer bounds with lo <= hi"
                             "lo" lower "hi" upper)))
  (for*/all ([lo lo #:exhaustive]
             [hi hi #:exhaustive])
    (blog-distribution
     (lambda ()
       (make-categorical
        (for/list ([value (in-range lo (add1 hi))])
          (cons value (/ 1 (+ 1 (- hi lo))))))))))

(struct blog-map-value (entries))
(define (blog-entry key value) (cons key value))
(define (blog-map . entries) (blog-map-value entries))

(define (Categorical mapping)
  (unless (blog-map-value? mapping)
    (raise-argument-error 'Categorical "BLOG map" mapping))
  (define entries (blog-map-value-entries mapping))
  (when (null? entries)
    (raise-arguments-error 'Categorical "empty distribution"))
  (define probabilities (map cdr entries))
  (check-probabilities 'Categorical probabilities)
  ;; Preserve correlations between weights, and remove impossible combinations
  ;; before validating or invoking Disrupt's concrete categorical constructor.
  (define cases
    (for/list ([(ps guard) (in-hash (flatten-symbolic probabilities))]
               #:unless (base:eq? guard #f))
      (cons guard ps)))
  (for ([case (in-list cases)])
    (define ps (cdr case))
    (define total (apply + ps))
    (unless (if (exact? total) (= total 1) (<= (abs (- total 1)) 1e-12))
      (raise-arguments-error 'Categorical "probabilities must sum to 1"
                            "sum" total)))
  (define (draw-weights ps)
    (define total (apply + ps))
    (make-categorical
     (map cons (map car entries) (map (lambda (p) (/ p total)) ps))))
  (blog-distribution
   (lambda ()
     (let draw ([cases cases])
       (match cases
         [(list (cons _ ps)) (draw-weights ps)]
         [(cons (cons guard ps) rest)
          (if guard (draw-weights ps) (draw rest))])))))

(define (obs actual expected)
  (observe! (equal? actual expected)))

(define-syntax-rule (blog-query expression)
  expression)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexer

(define-tokens non-empty-tokens
  (ident integer real))

(define-empty-tokens empty-tokens
  (eof type distinct fixed random obs query
       ift then elset caset int nullt truet falset
       comma semicolon eq distrib arrow
       equal unequal not and or implies minus plus times lt le gt ge uminus
       forall exists
       lb rb lp rp lc rc))

(define keywords
  (hash "type" token-type
        "distinct" token-distinct
        "fixed" token-fixed
        "random" token-random
        "obs" token-obs
        "query" token-query
        "forall" token-forall
        "exists" token-exists
        "case" token-caset
        "in" token-int
        "null" token-nullt
        "if" token-ift
        "then" token-then
        "else" token-elset
        "true" token-truet
        "false" token-falset))

(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ "A" "Z") "_")]
  [digit (:/ "0" "9")]
  [ident (:: letter (:* (:or letter digit)))]
  [exponent (:: (:or "e" "E")
                (:? (:or "+" "-"))
                (:+ digit))]
  [decimal (:or (:: (:+ digit) "." (:* digit))
                (:: "." (:+ digit)))]
  [real (:or (:: decimal (:? exponent))
             (:: (:+ digit) exponent))]
  [line-comment (:: "//" (:* (char-complement #\newline)))])

(define current-blog-source (make-parameter #f))

(define (lex-error message start end)
  (raise-read-error
   message
   (current-blog-source)
   (position-line start)
   (position-col start)
   (position-offset start)
   (and (position-offset start)
        (position-offset end)
        (- (position-offset end) (position-offset start)))))

;; Block comments do not nest.
(define (skip-block-comment in start)
  (let loop ([star? #f])
    (define c (read-char in))
    (cond
      [(eof-object? c)
       (define-values (line col offset) (port-next-location in))
       (lex-error "unterminated block comment"
                  start
                  (position offset line col))]
      [(and star? (char=? c #\/)) (void)]
      [else (loop (char=? c #\*))])))

(define blog-lex
  (lexer-src-pos
   [ident
    (let ([keyword (hash-ref keywords lexeme #f)])
      (if keyword
          (keyword)
          (token-ident (string->symbol lexeme))))]

   [(:+ digit) (token-integer (string->number lexeme))]
   [real (token-real (string->number lexeme))]

   ["(" (token-lp)] [")" (token-rp)]
   ["[" (token-lb)] ["]" (token-rb)]
   ["{" (token-lc)] ["}" (token-rc)]

   ["," (token-comma)]
   [";" (token-semicolon)]
   ["=" (token-eq)]
   ["~" (token-distrib)]
   ["->" (token-arrow)]

   ["==" (token-equal)]
   ["!=" (token-unequal)]
   ["!" (token-not)]
   ["&" (token-and)]
   ["|" (token-or)]
   ["=>" (token-implies)]
   ["-" (token-minus)]
   ["+" (token-plus)] ["*" (token-times)]
   ["<" (token-lt)] ["<=" (token-le)]
   [">" (token-gt)] [">=" (token-ge)]

   [(:+ whitespace)
    (return-without-pos (blog-lex input-port))]
   [line-comment
    (return-without-pos (blog-lex input-port))]
   ["/*"
    (begin
      (skip-block-comment input-port start-pos)
      (return-without-pos (blog-lex input-port)))]

   [(eof) (token-eof)]
   [any-char
    (lex-error (format "unexpected character ~s" lexeme)
               start-pos end-pos)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser

(define (blog-parser src-name)
  (parser
   (start program)
   (end eof)
   (tokens non-empty-tokens empty-tokens)
   (src-pos)
   (error (make-read-error src-name))

   ;; Lowest to highest precedence.
   ;; Choose right-associative implication for this subset.
   (precs
    [nonassoc forall exists]
    [nonassoc then]
    [nonassoc elset]
    [right implies]
    [left or]
    [left and]
    [nonassoc equal unequal]
    [nonassoc lt le gt ge]
    [left plus minus]
    [left times]
    [right not uminus])

   (grammar
    (program
     [() '()]
     [(statement program) (cons $1 $2)])

    (statement
     [(type ident semicolon)
      `(type ,$2)]

     [(distinct ident names semicolon)
      `(distinct ,$2 ,@$3)]

     [(fixed ident ident parameters eq expr semicolon)
      `(fixed ,$2 ,$3 ,$4 ,$6)]

     [(random ident ident parameters dependency expr semicolon)
      `(random ,$2 ,$3 ,$4 ,$5 ,$6)]

     [(obs expr eq expr semicolon)
      `(obs ,$2 ,$4)]

     [(query expr semicolon)
      `(query ,$2)])

    (dependency
     [(distrib) '~]
     [(eq) '=])

    (names
     [(name) (list $1)]
     [(name comma names) (cons $1 $3)])

    (name
     [(ident) $1]
     [(ident lb integer rb)
      `(indexed ,$1 ,$3)])

    (parameters
     [() '()]
     [(lp rp) '()]
     [(lp params rp) $2])

    (params
     [(param) (list $1)]
     [(param comma params) (cons $1 $3)])

    (param
     [(ident ident) (list $1 $2)])

    (expr
     [(atom) $1]
     [(lp expr rp) $2]

     [(ift expr then expr)
      (prec then)
      `(if ,$2 ,$4 null)]

     [(caset expr int lc entries rc)
      `(case ,$2 ,@$5)]

     [(ift expr then expr elset expr)
      `(if ,$2 ,$4 ,$6)]

     [(expr plus expr) `(+ ,$1 ,$3)]
     [(expr minus expr) `(- ,$1 ,$3)]
     [(expr times expr) `(* ,$1 ,$3)]
     [(expr lt expr) `(< ,$1 ,$3)]
     [(expr le expr) `(<= ,$1 ,$3)]
     [(expr gt expr) `(> ,$1 ,$3)]
     [(expr ge expr) `(>= ,$1 ,$3)]
     [(minus expr) (prec uminus)
      (if (number? $2) (- $2) `(- ,$2))]
     [(not expr) `(! ,$2)]
     [(expr and expr) `(& ,$1 ,$3)]
     [(expr or expr) `(,(string->symbol "|") ,$1 ,$3)]
     [(expr implies expr) `(=> ,$1 ,$3)]
     [(expr equal expr) `(== ,$1 ,$3)]
     [(expr unequal expr) `(!= ,$1 ,$3)]
     [(forall ident ident expr)
      (prec forall)
      `(forall ,$2 ,$3 ,$4)]
     [(exists ident ident expr)
      (prec exists)
      `(exists ,$2 ,$3 ,$4)]
     )

    (atom
     [(ident) $1]
     [(integer) $1]
     [(real) $1]
     [(nullt) 'null]
     [(truet) #t]
     [(falset) #f]

     [(ident lp arguments rp)
      `(call ,$1 ,@$3)]

     [(ident lb expr rb)
      `(index ,$1 ,$3)]

     [(lc entries rc)
      `(map ,@$2)])

    (arguments
     [() '()]
     [(args) $1])

    (args
     [(expr) (list $1)]
     [(expr comma args) (cons $1 $3)])

    (entries
     [(entry) (list $1)]
     [(entry comma entries) (cons $1 $3)])

    (entry
     [(expr arrow expr)
      `(entry ,$1 ,$3)]))))

(define (make-read-error src-name)
  (λ (tok-ok? tok-name tok-value start end)
    (match-define (position start-offset start-line start-col) start)
    (match-define (position end-offset end-line end-col) end)
    (define span
      (and start-offset end-offset (- end-offset start-offset)))
    (raise-read-error
     (if (eq? tok-name 'eof)
         "unexpected end of BLOG input"
         (format "unexpected BLOG token ~a~a"
                 tok-name
                 (if tok-value (format " (~s)" tok-value) "")))
     src-name start-line start-col start-offset span)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(module+ reader
  (provide
   (rename-out [blog-read read]
               [blog-read-syntax read-syntax])))

(define (blog-read in)
  (syntax->datum (blog-read-syntax #f in)))

(define (blog-read-syntax src in)
  (define forms (blog-parse src in))
  (strip-context
   #`(module _ roulette/example/blog #,@forms)))

(define (blog-parse [source-name (object-name (current-input-port))]
                    [port (current-input-port)])
  (port-count-lines! port)
  (parameterize ([current-blog-source source-name])
    (define parse (blog-parser source-name))
    (parse (λ () (blog-lex port)))))
