#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 #%module-begin
 #%app
 #%datum
 blog-parse
 blog-run
 (struct-out blog-query-result)
 blog-lex
 blog-list
 type
 distinct
 fixed
 random
 call
 BooleanDistrib
 Categorical
 UniformInt
 Bernoulli
 transpose toReal toInt
 eye zeros ones diag repmat hstack vstack sum trace det inv
 modulo expt < <= > >=
 if
 =>
 !=
 obs
(rename-out
 [blog+ +] [blog- -] [blog* *] [blog/ /]
 [blog-abs abs] [blog-exp exp]
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
 [or \|]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in : parser-tools/lex-sre)
         parser-tools/lex
         parser-tools/yacc
         syntax/readerr
         syntax/strip-context
         (only-in racket/base [eq? base:eq?]
                  [truncate base:truncate]
                  [exp base:exp]
                  [exact->inexact base:exact->inexact]
                  [inexact->exact base:inexact->exact])
         (only-in roulette/private/util flatten-symbolic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time

;; The descriptor is opaque; its name is for display and errors.
(struct blog-null-value ()
  #:methods gen:custom-write
  [(define (write-proc value port mode)
     (display "null" port))])

(define blog-null (blog-null-value))

;; List literals are values in their own right, rather than matrices or indexed
;; distinct-object families. Keep their elements in immutable Racket lists.
;; The parser emits blog-list calls for both bracket literal spellings.
(struct blog-list-value (elements) #:transparent)
(define (blog-list . elements) (blog-list-value elements))

;; A numeric matrix has explicit dimensions and immutable row-major storage.
;; Transparent, immutable structures let Roulette merge symbolic fields and
;; flatten them into concrete query results, preserving value equality.
;; Only the checked constructor below is used to create matrices.
(struct blog-matrix-value (rows columns elements) #:transparent)

(define (check-matrix-element element)
  ;; Check finite-choice alternatives without concretizing the stored value.
  ;; Checking each cell separately avoids enumerating the product of all cells.
  (for ([(value guard) (in-hash (flatten-symbolic element))]
        #:unless (base:eq? guard #f))
    (unless (real? value)
      (raise-argument-error 'make-blog-matrix "real numeric matrix element" value))))

;; make-blog-matrix converts an internal BLOG list into a validated matrix value.
;; Example: (make-blog-matrix (blog-list 10 20 30)) returns (blog-matrix-value 1 3 '(10 20 30))
;; desiderata:
;; A flat BLOG list becomes a row matrix; Nested BLOG lists become matrix rows;
;; An existing matrix is returned unchanged; [] is a 0-by-0 matrix;
;; Unequal row lengths are rejected; Nonnumeric elements are rejected;
;; Mixed scalars and rows are rejected; Nesting deeper than two levels is rejected.
(define (make-blog-matrix source)
  (for/all ([source source #:exhaustive])
    (cond
      [(blog-matrix-value? source) source]
      [(blog-list-value? source)
       (define elements (blog-list-value-elements source))
       (cond
         [(null? elements) (blog-matrix-value 0 0 '())]
         [(andmap blog-list-value? elements)
          (define rows (map blog-list-value-elements elements))
          (define columns (length (car rows)))
          (unless (andmap (lambda (row) (= (length row) columns)) rows)
            (raise-arguments-error 'make-blog-matrix
                                   "matrix rows must have equal lengths"
                                   "rows" source))
          (define cells (apply append rows))
          (for-each check-matrix-element cells)
          (blog-matrix-value (length rows) columns cells)]
         [else
          ;; Also rejects mixed scalar/row lists and nesting deeper than two.
          (for-each check-matrix-element elements)
          (blog-matrix-value 1 (length elements) elements)])]
      [else
       (raise-argument-error 'make-blog-matrix "BLOG list or numeric matrix" source)])))

(define-syntax blog-case
  (syntax-parser
    #:datum-literals (entry)
    [(_ selector:expr (entry key:expr value:expr) ...)
     #'(let ([selected selector])
         (cond
           [(equal? selected key) value] ...
           [else blog-null]))]))

(struct blog-null-value ()
  #:methods gen:custom-write
  [(define (write-proc value port mode)
     (display "null" port))])

(define blog-null (blog-null-value))

(define-syntax blog-case
  (syntax-parser
    #:datum-literals (entry)
    [(_ selector:expr (entry key:expr value:expr) ...)
     #'(let ([selected selector])
         (cond
           [(equal? selected key) value] ...
           [else blog-null]))]))

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

;; Inspect only dimensions, never all numeric cells, when validating an index.
;; Flattening the limit and index together preserves their correlated guards.
(define (blog-index-limit value)
  (for/all ([value value #:exhaustive])
    (cond
      [(blog-object-family? value)
       (vector-length (blog-object-family-objects value))]
      [(blog-list-value? value) (length (blog-list-value-elements value))]
      [(blog-matrix-value? value)
       (define rows (blog-matrix-value-rows value))
       (define columns (blog-matrix-value-columns value))
       (if (= rows 1) columns rows)]
      [else #f])))

(define (blog-index value index)
  ;; Validate before symbolic dispatch so an invalid reachable alternative
  ;; raises an error instead of silently eliminating that possible outcome.
  (for ([(choice guard)
         (in-hash (flatten-symbolic (list (blog-index-limit value) index)))]
        #:unless (base:eq? (&& guard (vc-assumes (vc))) #f))
    (define limit (car choice))
    (define i (cadr choice))
    (unless limit
      (raise-argument-error 'index "indexed object family, BLOG list, or matrix" value))
    (unless (and (exact-integer? i) (<= 0 i) (< i limit))
      (raise-arguments-error 'index "index outside the indexed value"
                             "index" i "size" limit)))
  (for*/all ([value value #:exhaustive]
             [i index #:exhaustive])
    (cond
      [(blog-object-family? value)
       (vector-ref (blog-object-family-objects value) i)]
      [(blog-list-value? value)
       (list-ref (blog-list-value-elements value) i)]
      [else
       (for*/all ([rows (blog-matrix-value-rows value) #:exhaustive]
                  [columns (blog-matrix-value-columns value) #:exhaustive])
         (define cells (blog-matrix-value-elements value))
         (if (or (= rows 1) (= columns 1))
             (blog-matrix-value 1 1 (list (list-ref cells i)))
             (blog-matrix-value 1 columns
                                (take (drop cells (* i columns)) columns))))])))

(define (blog-matrix-shape value)
  (for/all ([value value #:exhaustive])
    (and (blog-matrix-value? value)
         (list (blog-matrix-value-rows value)
               (blog-matrix-value-columns value)))))

(define (transpose value)
  (for ([(shape guard) (in-hash (flatten-symbolic (blog-matrix-shape value)))]
        #:unless (base:eq? (&& guard (vc-assumes (vc))) #f))
    (unless shape
      (raise-argument-error 'transpose "BLOG matrix" value)))
  (for*/all ([value value #:exhaustive]
             [rows (blog-matrix-value-rows value) #:exhaustive]
             [columns (blog-matrix-value-columns value) #:exhaustive])
    (define cells (blog-matrix-value-elements value))
    (blog-matrix-value
     columns rows
     (for*/list ([column (in-range columns)] [row (in-range rows)])
       (list-ref cells (+ (* row columns) column))))))

;; Scalar conversions accept numeric/Boolean scalars as well as 1-by-1
;; matrices. Validate shape without enumerating unrelated symbolic cells.
(define (blog-scalar-source who value)
  (define description
    (for/all ([value value #:exhaustive])
      (cond
        [(blog-matrix-value? value) (blog-matrix-shape value)]
        [(or (real? value) (boolean? value)) 'scalar]
        [else #f])))
  (for ([(description guard) (in-hash (flatten-symbolic description))]
        #:unless (base:eq? (&& guard (vc-assumes (vc))) #f))
    (unless (or (eq? description 'scalar) (equal? description '(1 1)))
      (raise-argument-error who "real/Boolean scalar or 1-by-1 matrix" value)))
  (for/all ([value value #:exhaustive])
    (if (blog-matrix-value? value)
        (car (blog-matrix-value-elements value))
        value)))

(define (blog-convert-scalar who value convert)
  ;; Flatten only the extracted scalar, including finite arithmetic terms.
  (define cases
    (for/list ([(scalar guard) (in-hash (flatten-symbolic (blog-scalar-source who value)))]
               #:unless (base:eq? (&& guard (vc-assumes (vc))) #f))
      (cons guard (convert scalar))))
  (let select ([cases cases])
    (match cases
      [(list (cons _ scalar)) scalar]
      [(cons (cons guard scalar) rest)
       (if guard scalar (select rest))])))

(define (toReal value)
  (blog-convert-scalar
   'toReal value
   (lambda (scalar)
     (base:exact->inexact (if (boolean? scalar) (if scalar 1 0) scalar)))))

(define (toInt value)
  (blog-convert-scalar
   'toInt value
   (lambda (scalar)
     (define number (if (boolean? scalar) (if scalar 1 0) scalar))
     (unless (rational? number)
       (raise-argument-error 'toInt "finite real scalar" number))
     (base:inexact->exact (base:truncate number)))))

;; Validate correlated metadata before doing any guarded computation. Shapes
;; stay separate from entries so most operations never enumerate whole matrices.
(define (blog-check-cases who description valid? expected)
  (for ([(value guard) (in-hash (flatten-symbolic description))]
        #:unless (base:eq? (&& guard (vc-assumes (vc))) #f))
    (unless (valid? value)
      (raise-arguments-error who expected "arguments" value))))

(define (blog-merge-cases cases)
  (match cases
    [(list (cons _ value)) value]
    [(cons (cons guard value) rest)
     (if guard value (blog-merge-cases rest))]))

;; Use for finite dimensions or concrete numeric algorithms (exp, elimination).
;; Conversion is done before merging so errors in reachable cases propagate.
(define (blog-finite-apply value compute)
  (blog-merge-cases
   (for/list ([(concrete guard) (in-hash (flatten-symbolic value))]
              #:unless (base:eq? (&& guard (vc-assumes (vc))) #f))
     (cons guard (compute concrete)))))

(define (blog-number-shape value)
  (for/all ([value value #:exhaustive])
    (if (real? value) 'scalar (blog-matrix-shape value))))

;; Split references and dimensions, retaining symbolic entries. Joint checks
;; have already ruled out invalid combinations before this dispatch runs.
(define (blog-with-arguments arguments compute)
  (let loop ([remaining arguments] [reversed '()])
    (if (null? remaining)
        (apply compute (reverse reversed))
        (for/all ([value (car remaining) #:exhaustive])
          (if (blog-matrix-value? value)
              (for*/all ([rows (blog-matrix-value-rows value) #:exhaustive]
                         [columns (blog-matrix-value-columns value) #:exhaustive])
                (loop (cdr remaining)
                      (cons (blog-matrix-value rows columns
                                               (blog-matrix-value-elements value))
                            reversed)))
              (loop (cdr remaining) (cons value reversed)))))))

(define (blog-map-matrix function matrix)
  (blog-matrix-value (blog-matrix-value-rows matrix)
                     (blog-matrix-value-columns matrix)
                     (map function (blog-matrix-value-elements matrix))))

(define (blog-unary who function value)
  (blog-check-cases who (blog-number-shape value)
                    (lambda (shape) (and shape #t)) "expected a real number or matrix")
  (blog-with-arguments
   (list value)
   (lambda (value)
     (if (blog-matrix-value? value) (blog-map-matrix function value) (function value)))))

(define (blog-binary who function left right)
  (define (valid? shapes)
    (match shapes
      [(list 'scalar 'scalar) #t]
      [(list (list lr lc) (list rr rc))
       (if (eq? who '*) (= lc rr)
           (and (memq who '(+ -)) (= lr rr) (= lc rc)))]
      ;; Scalars add/subtract every cell; operand order matters for subtraction.
      [(list 'scalar (list _ _)) (and (memq who '(+ - *)) #t)]
      [(list (list _ _) 'scalar) (and (memq who '(+ - * /)) #t)]
      [_ #f]))
  (blog-check-cases who (list (blog-number-shape left) (blog-number-shape right))
                    valid? "incompatible arithmetic operand types or matrix dimensions")
  (when (eq? who '/)
    (blog-check-cases '/ right (lambda (x) (and (real? x) (not (zero? x))))
                      "expected a nonzero scalar divisor"))
  (blog-with-arguments
   (list left right)
   (lambda (left right)
     (cond
       [(and (blog-matrix-value? left) (blog-matrix-value? right))
        (define rows (blog-matrix-value-rows left))
        (define inner (blog-matrix-value-columns left))
        (define columns (blog-matrix-value-columns right))
        (define a (blog-matrix-value-elements left))
        (define b (blog-matrix-value-elements right))
        (if (eq? who '*)
            (blog-matrix-value
             rows columns
             (for*/list ([i (in-range rows)] [j (in-range columns)])
               (for/fold ([total 0]) ([k (in-range inner)])
                 (+ total (* (list-ref a (+ (* i inner) k))
                             (list-ref b (+ (* k columns) j)))))))
            (blog-matrix-value rows inner (map function a b)))]
       [(blog-matrix-value? left)
        (blog-map-matrix (lambda (cell) (function cell right)) left)]
       [(blog-matrix-value? right)
        (blog-map-matrix (lambda (cell) (function left cell)) right)]
       [else (function left right)]))))

(define (blog+ . arguments)
  (if (null? arguments) 0
      (for/fold ([result (blog-unary '+ values (car arguments))])
                ([argument (in-list (cdr arguments))])
        (blog-binary '+ + result argument))))

(define (blog- first . rest)
  (if (null? rest) (blog-unary '- - first)
      (for/fold ([result first]) ([argument (in-list rest)])
        (blog-binary '- - result argument))))

(define (blog* . arguments)
  (if (null? arguments) 1
      (for/fold ([result (blog-unary '* values (car arguments))])
                ([argument (in-list (cdr arguments))])
        (blog-binary '* * result argument))))

(define (blog/ first . rest)
  (if (null? rest) (blog-binary '/ / 1 first)
      (for/fold ([result first]) ([argument (in-list rest)])
        (blog-binary '/ / result argument))))

(define (blog-abs value) (blog-unary 'abs abs value))
(define (blog-exp value)
  (blog-unary 'exp (lambda (cell) (blog-finite-apply cell base:exp)) value))

(define (blog-matrix-operation who value shape-valid? compute)
  (blog-check-cases who (blog-matrix-shape value)
                    (lambda (shape) (and shape (apply shape-valid? shape)))
                    "expected a matrix with compatible dimensions")
  (blog-with-arguments (list value) compute))

(define (blog-make-filled who rows columns entry)
  (blog-check-cases who (list rows columns)
                    (lambda (dims) (andmap exact-nonnegative-integer? dims))
                    "expected nonnegative integer dimensions")
  (blog-finite-apply
   (list rows columns)
   (lambda (dims)
     (match-define (list rows columns) dims)
     (blog-matrix-value rows columns
                        (for*/list ([i (in-range rows)] [j (in-range columns)])
                          (entry i j))))))

(define (zeros rows columns) (blog-make-filled 'zeros rows columns (lambda (i j) 0)))
(define (ones rows columns) (blog-make-filled 'ones rows columns (lambda (i j) 1)))
(define (eye dimension)
  (blog-make-filled 'eye dimension dimension (lambda (i j) (if (= i j) 1 0))))

(define (diag value)
  (blog-matrix-operation
   'diag value (lambda (rows columns) (= columns 1))
   (lambda (matrix)
     (define n (blog-matrix-value-rows matrix))
     (define cells (blog-matrix-value-elements matrix))
     (blog-matrix-value n n
                        (for*/list ([i (in-range n)] [j (in-range n)])
                          (if (= i j) (list-ref cells i) 0))))))

(define (repmat value row-count column-count)
  (blog-check-cases
   'repmat (list (blog-matrix-shape value) row-count column-count)
   (lambda (description)
     (and (car description) (andmap exact-nonnegative-integer? (cdr description))))
   "expected a matrix and nonnegative integer repetition counts")
  (blog-finite-apply
   (list row-count column-count)
   (lambda (counts)
     (blog-with-arguments
      (list value)
      (lambda (matrix)
        (define rows (blog-matrix-value-rows matrix))
        (define columns (blog-matrix-value-columns matrix))
        (define cells (blog-matrix-value-elements matrix))
        (define out-rows (* rows (car counts)))
        (define out-columns (* columns (cadr counts)))
        (blog-matrix-value
         out-rows out-columns
         (for*/list ([i (in-range out-rows)] [j (in-range out-columns)])
           (list-ref cells (+ (* (modulo i rows) columns) (modulo j columns))))))))))

(define (blog-stack who arguments)
  (when (null? arguments)
    (raise-arguments-error who "expected at least one scalar or matrix"))
  (define shapes
    (map (lambda (value)
           (define shape (blog-number-shape value))
           (if (eq? shape 'scalar) '(1 1) shape)) arguments))
  (blog-check-cases
   who shapes
   (lambda (shapes)
     (and (andmap list? shapes)
          (apply = (map (if (eq? who 'hstack) car cadr) shapes))))
   "stacked matrices must have matching row or column counts")
  (blog-with-arguments
   arguments
   (lambda matrices
     (define normalized
       (map (lambda (value) (if (real? value) (blog-matrix-value 1 1 (list value)) value))
            matrices))
     (if (eq? who 'vstack)
         (blog-matrix-value (apply + (map blog-matrix-value-rows normalized))
                            (blog-matrix-value-columns (car normalized))
                            (apply append (map blog-matrix-value-elements normalized)))
         (let ([rows (blog-matrix-value-rows (car normalized))])
           (blog-matrix-value
            rows (apply + (map blog-matrix-value-columns normalized))
            (apply append
                   (for/list ([i (in-range rows)])
                     (apply append
                            (for/list ([matrix (in-list normalized)])
                              (define columns (blog-matrix-value-columns matrix))
                              (take (drop (blog-matrix-value-elements matrix) (* i columns))
                                    columns)))))))))))

(define (hstack . arguments) (blog-stack 'hstack arguments))
(define (vstack . arguments) (blog-stack 'vstack arguments))

(define (sum value)
  (blog-matrix-operation
   'sum value (lambda (rows columns) #t)
   (lambda (matrix)
     (define rows (blog-matrix-value-rows matrix))
     (define columns (blog-matrix-value-columns matrix))
     (define cells (blog-matrix-value-elements matrix))
     (blog-matrix-value
      1 columns
      (for/list ([j (in-range columns)])
        (for/fold ([total 0]) ([i (in-range rows)])
          (+ total (list-ref cells (+ (* i columns) j)))))))))

(define (trace value)
  (blog-matrix-operation
   'trace value =
   (lambda (matrix)
     (define n (blog-matrix-value-rows matrix))
     (define cells (blog-matrix-value-elements matrix))
     (for/fold ([total 0]) ([i (in-range n)])
       (+ total (list-ref cells (+ (* i n) i)))))))

;; Pivoted Gaussian elimination uses concrete finite alternatives, preserving
;; exact arithmetic. Other matrix operations keep cells symbolic. Elimination
;; can be expensive when a matrix has many independent random entries.
(define (blog-eliminate who matrix inverse?)
  (define n (blog-matrix-value-rows matrix))
  (define cells (blog-matrix-value-elements matrix))
  (unless (andmap rational? cells)
    (raise-argument-error who "matrix with finite real entries" matrix))
  (define width (if inverse? (* 2 n) n))
  (define work
    (for/vector ([i (in-range n)])
      (list->vector
       (append (take (drop cells (* i n)) n)
               (if inverse? (for/list ([j (in-range n)]) (if (= i j) 1 0)) '())))))
  (let/ec return
    (define determinant 1)
    (for ([k (in-range n)])
      (define pivot
        (for/fold ([best k]) ([i (in-range (add1 k) n)])
          (if (> (abs (vector-ref (vector-ref work i) k))
                 (abs (vector-ref (vector-ref work best) k))) i best)))
      (when (zero? (vector-ref (vector-ref work pivot) k))
        (if inverse?
            (raise-arguments-error who "matrix is singular" "matrix" matrix)
            (return 0)))
      (unless (= pivot k)
        (define old (vector-ref work k))
        (vector-set! work k (vector-ref work pivot))
        (vector-set! work pivot old)
        (set! determinant (- determinant)))
      (define pivot-row (vector-ref work k))
      (define divisor (vector-ref pivot-row k))
      (set! determinant (* determinant divisor))
      (for ([j (in-range k width)])
        (vector-set! pivot-row j (/ (vector-ref pivot-row j) divisor)))
      (for ([i (in-range (if inverse? 0 (add1 k)) n)] #:unless (= i k))
        (define row (vector-ref work i))
        (define factor (vector-ref row k))
        (for ([j (in-range k width)])
          (vector-set! row j (- (vector-ref row j) (* factor (vector-ref pivot-row j)))))))
    (if inverse?
        (blog-matrix-value n n
                           (for*/list ([i (in-range n)] [j (in-range n width)])
                             (vector-ref (vector-ref work i) j)))
        determinant)))

(define (blog-square-eliminate who value inverse?)
  (blog-check-cases who (blog-matrix-shape value)
                    (lambda (shape) (and shape (apply = shape)))
                    "expected a square matrix")
  (blog-finite-apply value (lambda (matrix) (blog-eliminate who matrix inverse?))))

(define (det value) (blog-square-eliminate 'det value #f))
(define (inv value) (blog-square-eliminate 'inv value #t))

;; Only RealMatrix return annotations are checked for now. Match the type name
;; as syntax, just as other built-in annotations need no runtime binding.
(define-syntax blog-result-warpper
  (syntax-parser
    [(_ (~datum RealMatrix) body:expr) #'(make-blog-matrix body)]
    [(_ result-type:id body:expr) #'body]))

(define-syntax fixed
  (syntax-parser
    ;; No arguments: define a constant.
    [(_ result-type:id name:id () body:expr)
     #'(define name (blog-result-warpper result-type body))]

    ;; One or more arguments: define a function.
    [(_ result-type:id name:id
        ((arg-type:id arg:id) ...+) body:expr)
     #'(define (name arg ...) (blog-result-warpper result-type body))]))

;; Ground random functions over the objects declared so far. Objects and parent
;; functions must precede their dependents; later declarations are not included.
;; Matrix conversion runs once per grounded tuple, after drawing a distribution
;; or evaluating a deterministic dependency, so repeated calls reuse the value.
(define-syntax random
  (syntax-parser
    [(_ result-type:id name:id params (~datum ~) body:expr)
     #'(define-random name params
         (blog-result-warpper result-type (draw-distribution body)))]
    [(_ result-type:id name:id params (~datum =) body:expr)
     #'(define-random name params (blog-result-warpper result-type body))]))

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

;; A single query returns its value; comma-separated expressions form one joint
;; outcome in source order. Use a Racket list as the joint tuple, distinct from
;; a BLOG list literal supplied as one query expression.
(define-syntax blog-query
  (syntax-parser
    [(_ expression:expr) #'expression]
    [(_ first:expr second:expr rest:expr ...)
     #'(list first second rest ...)]))

;; The programmatic runner returns frozen marginal/joint PMF hashes in order.
(struct blog-query-result (number expressions probabilities) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexer

(define-tokens non-empty-tokens
  (ident integer real))

(define-empty-tokens empty-tokens
  (eof type distinct fixed random obs query
       ift then elset caset int nullt truet falset
       comma semicolon eq distrib arrow
       equal unequal not and or implies minus plus times div mod pow lt le gt ge uminus
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
   ["/" (token-div)]
   ["%" (token-mod)]
   ["^" (token-pow)]
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
    [left pow]
    [left mod]
    [left div]
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

     [(query args semicolon)
      `(query ,@$2)])

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
     [(postfix) $1]

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
     [(expr div expr) `(/ ,$1 ,$3)]
     [(expr mod expr) `(modulo ,$1 ,$3)]
     [(expr pow expr) `(expt ,$1 ,$3)]
     [(expr lt expr) `(< ,$1 ,$3)]
     [(expr le expr) `(<= ,$1 ,$3)]
     [(expr gt expr) `(> ,$1 ,$3)]
     [(expr ge expr) `(>= ,$1 ,$3)]
     [(minus expr) (prec uminus)
      (if (number? $2) (- $2) `(- ,$2))]
     [(plus expr) (prec uminus) `(+ ,$2)]
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

    ;; Postfix indexing binds more tightly than unary and binary operators.
    ;; Parentheses let any expression serve as the indexed value.
    (postfix
     [(atom) $1]
     [(postfix lb expr rb)
      `(index ,$1 ,$3)])

    (atom
     [(ident) $1]
     [(integer) $1]
     [(real) $1]
     [(nullt) 'null]
     [(truet) #t]
     [(falset) #f]
     [(lp expr rp) $2]

     [(ident lp arguments rp)
      `(call ,$1 ,@$3)]

     [(lb arguments rb)
      `(blog-list ,@$2)]

     [(lb list-rows rb)
      `(blog-list ,@(map (lambda (row) `(blog-list ,@row)) $2))]

     [(lc entries rc)
      `(map ,@$2)])

    ;; At least two nonempty rows distinguish shorthand from a flat list.
    ;; Nested [] literals can still represent empty rows explicitly.
    (list-rows
     [(args semicolon args) (list $1 $3)]
     [(args semicolon list-rows) (cons $1 $3)])

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

(define-namespace-anchor blog-namespace)

;; Like blog-parse, accepts raw BLOG text from a port (without a #lang line).
;; Keep the model in one scope, but infer each query at its statement position.
;; A fresh namespace and inference session isolate definitions and observations
;; across calls without creating independent model draws for different queries.
(define (blog-run [source-name (object-name (current-input-port))]
                  [port (current-input-port)])
  (define forms (blog-parse source-name port))
  (define ns (make-base-namespace))
  (namespace-attach-module (namespace-anchor->namespace blog-namespace)
                           'roulette/example/blog ns)
  (parameterize ([current-namespace ns])
    (namespace-require 'roulette/example/blog)
    (call-with-query-session
     (lambda (infer)
       (define reversed-results '())
       (define query-number 0)
       (define record-name (gensym 'record-query))
       (namespace-set-variable-value!
        record-name
        (lambda (expressions thunk)
          (define probabilities (pmf-hash (infer thunk)))
          (set! query-number (add1 query-number))
          (set! reversed-results
                (cons (blog-query-result query-number
                                         expressions probabilities)
                      reversed-results)))
        #t)
       (eval
        `(let ()
           ,@(for/list ([form (in-list forms)])
               (match form
                 [(list 'query expressions ...)
                  `(,record-name ',expressions (lambda () ,form))]
                 [_ form]))
           (void)))
       (reverse reversed-results)))))

;; Internal helpers exposed only for the separate runtime tests.
(module+ test-support
  (provide blog-list
           make-blog-matrix
           blog-matrix-value
           blog-matrix-value-rows
           blog-matrix-value-columns
           blog-matrix-value-elements
           blog-null))
