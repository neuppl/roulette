#lang racket/base
(require "lexer.rkt")
(provide parse-probalog)

;; Parses the entire port and returns a list of syntax objects, each
;; wrapping one of:
;;   (#%probalog-fact-entry (fact 'Name (list arg ...)) prob)
;;   (#%probalog-rule-entry (rule (fact 'Name (list arg ...))
;;                                 (list (fact 'Name (list arg ...)) ...))
;;                          scaffold)
;;   (#%probalog-query-entry (fact 'Name (list arg ...)))
;;   (#%probalog-observe-entry (fact 'Name (list arg ...)) polarity)
;;     polarity is #t for positive observations (! Foo(args).)
;;              and #f for negative observations (! ~Foo(args).)
;;   (#%probalog-scaffold scaffold)
;; These head symbols are recognized by probalog/expander.rkt's
;; #%module-begin, which consumes and rewrites them — they're never
;; actually bound to real functions/macros.
;;
;; Each form carries the source location of the statement it came
;; from, so that runtime errors and Check Syntax annotations point
;; back into the original program text. The `scaffold`s are dead code
;; whose only purpose is to give variables and predicate names real
;; binding structure for Check Syntax; see `rule-scaffold` and
;; `predicate-scaffold` below.
;;
;; `scaffold?` is false when reading a single statement at the
;; interactions prompt, where there is no file for Check Syntax to
;; annotate and the trailing scaffold form would just be noise.
(define (parse-probalog port [src #f] #:scaffold? [scaffold? #t])
  (define toks (tokenize port src))
  (define st (pstate (make-hash) (box '())))
  (let loop ([toks toks] [acc '()])
    (cond
      [(eq? (token-type (car toks)) 'eof)
       (define forms (reverse acc))
       (if scaffold?
           (append forms (list (predicate-scaffold st)))
           forms)]
      [else
       (let-values ([(form rest) (parse-statement toks st)])
         (loop rest (cons form acc)))])))

;; Parser state threaded through the whole file, since both of the
;; things it tracks are file-wide rather than per-statement:
;;   arities : predicate name -> argument count, checked for agreement
;;   occs    : every predicate-name occurrence, in source order, as
;;             (vector symbol token definition?), reversed
(struct pstate (arities occs) #:transparent)

;; --- token stream helpers -------------------------------------------

(define (peek toks) (car toks))
(define (peek-type toks) (token-type (car toks)))
(define (peek-loc toks) (token-srcloc (car toks)))

;; `what` names what was expected, for the positions where the token
;; type alone is uninformative: every identifier the grammar expects
;; is a predicate name, but reporting "an identifier" leaves the
;; reader to work out which of the two kinds was wanted.
(define (expect toks type #:what [what #f])
  (unless (eq? (peek-type toks) type)
    (probalog-read-error (peek-loc toks)
                         "expected ~a but got ~a~a"
                         (or what (describe type))
                         (describe (peek-type toks))
                         (if (token-value (peek toks))
                             (format " (~a)" (token-value (peek toks)))
                             "")))
  (values (peek toks) (cdr toks)))

(define (expect-predicate-name toks)
  (expect toks 'ident #:what "a predicate name"))

(define (describe type)
  (case type
    [(ident) "an identifier"]
    [(string) "a string"]
    [(number) "a number"]
    [(lparen) "'('"]
    [(rparen) "')'"]
    [(comma) "','"]
    [(period) "'.'"]
    [(colon-dash) "':-'"]
    [(coloncolon) "'::'"]
    [(question) "'?'"]
    [(bang) "'!'"]
    [(tilde) "'~'"]
    [(eof) "end of file"]
    [else (format "~a" type)]))

;; --- syntax construction ---------------------------------------------

;; A parsed argument: `form` is the datum to embed in the emitted
;; program, `var` is the variable's symbol (or #f for a constant), and
;; `loc` locates the argument in the source.
(struct parg (form var loc) #:transparent)

;; An identifier that Check Syntax will treat as if the user had
;; written it, at the given location. Check Syntax only annotates
;; identifiers it considers "original"; syntax built by a reader
;; doesn't qualify automatically, so it's marked explicitly.
(define (original-id sym loc)
  (syntax-property (datum->syntax #f sym loc) 'original-for-check-syntax #t))

;; The same treatment for predicate names, but file-wide rather than
;; per-rule: a predicate is bound by the first statement that defines
;; it — a fact declaration or a rule head — and used by every body
;; clause, query, and observation that names it, as well as by any
;; later definition. A predicate that is only ever used gets its first
;; occurrence as the binding one, so its uses still group together.
(define (predicate-scaffold st)
  (define occs (reverse (unbox (pstate-occs st))))
  (define binder-toks ; predicate name -> the occurrence that binds it
    (for/fold ([h (hash)]) ([occ occs])
      (define sym (vector-ref occ 0))
      (define existing (hash-ref h sym #f))
      (cond
        [(not existing) (hash-set h sym occ)]
        ;; a definition supersedes a use as the binding occurrence
        [(and (vector-ref occ 2) (not (vector-ref existing 2)))
         (hash-set h sym occ)]
        [else h])))
  (define-values (binders uses)
    (for/fold ([binders '()] [uses '()]
               #:result (values (reverse binders) (reverse uses)))
              ([occ occs])
      (define id (original-id (vector-ref occ 0) (token-srcloc (vector-ref occ 1))))
      (if (eq? occ (hash-ref binder-toks (vector-ref occ 0)))
          (values (cons id binders) uses)
          (values binders (cons id uses)))))
  (datum->syntax
   #f
   `(#%probalog-scaffold (when #f (lambda ,binders (void ,@uses))))
   #f))

;; Every variable occurrence in a rule, as located identifiers, split
;; into the occurrence that binds it and the ones that refer to it. A
;; variable is bound by its first occurrence in the body (which is
;; what actually ranges over the database) and used everywhere else,
;; including in the head. Range restriction guarantees every head
;; variable has a binding occurrence in the body.
(define (rule-scaffold head-args body-argss)
  ;; Every occurrence, tagged with whether it sits in the body and so
  ;; is eligible to be the binding one.
  (define occurrences
    (append (for/list ([a head-args] #:when (parg-var a)) (cons a #f))
            (for*/list ([args body-argss] [a args] #:when (parg-var a)) (cons a #t))))
  (define bound (make-hasheq))
  (define-values (binders uses)
    (for/fold ([binders '()] [uses '()]
               #:result (values (reverse binders) (reverse uses)))
              ([occ occurrences])
      (define a (car occ))
      (define in-body? (cdr occ))
      (define id (original-id (parg-var a) (parg-loc a)))
      (cond
        [(and in-body? (not (hash-ref bound (parg-var a) #f)))
         (hash-set! bound (parg-var a) #t)
         (values (cons id binders) uses)]
        [else (values binders (cons id uses))])))
  ;; Not evaluated: `when #f` keeps this out of the runtime program
  ;; while still having it expanded, which is all Check Syntax needs.
  (datum->syntax #f `(when #f (lambda ,binders (void ,@uses))) #f))

;; --- grammar ----------------------------------------------------------
;; statement := fact-or-rule-decl | query-decl | observe-decl
;; fact-or-rule-decl := NAME '(' arglist ')' ( '::' NUMBER | ':-' body | ε ) '.'
;;   -- omitting '::' NUMBER entirely (i.e. just NAME(args).) declares
;;      a fact with an implicit probability of 1 (a certain fact).
;;   -- NUMBER after '::' must be in [0, 1].
;;   -- every variable in the head must also appear in some body clause
;;      (range restriction) — otherwise it could never be bound.
;; query-decl        := '?' NAME '(' arglist ')' '.'
;; observe-decl      := '!' [ '~' ] NAME '(' arglist ')' '.'
;; body              := clause (',' clause)*
;; clause            := NAME '(' arglist ')'
;; arglist           := arg (',' arg)* | ε
;; arg               := STRING | NUMBER | IDENT
;;   -- an IDENT arg must start with a lowercase letter (a variable);
;;      an uppercase-starting IDENT here is almost always a forgotten
;;      quote around a constant, so it's rejected rather than silently
;;      treated as a variable.
;;   -- every use of a given NAME (fact, rule head, body clause, or
;;      query) across the whole file must agree on argument count.

(define (parse-statement toks st)
  (case (peek-type toks)
    [(question) (parse-query toks st)]
    [(bang)     (parse-observe toks st)]
    [else       (parse-fact-or-rule toks st)]))

(define (parse-query toks0 st)
  (define start (peek toks0))
  (define toks (cdr toks0))
  (define-values (name-tok toks1) (expect-predicate-name toks))
  (check-predicate-name! name-tok)
  (define name-sym (string->symbol (token-value name-tok)))
  (define-values (args toks2) (parse-parenthesized-arglist toks1))
  (check-ground! "query" name-sym args)
  (check-arity! st name-tok args)
  (define-values (end toks3) (expect toks2 'period))
  (values (datum->syntax
           #f
           `(#%probalog-query-entry ,(fact-form name-sym args))
           (token-loc-span start end))
          toks3))

;; Observation syntax:
;;   ! Foo(args).    -- observe that Foo(args) is TRUE
;;   ! ~Foo(args).   -- observe that Foo(args) is FALSE
(define (parse-observe toks0 st)
  (define start (peek toks0))
  (define toks (cdr toks0))
  (define negated? (eq? (peek-type toks) 'tilde))
  (define toks* (if negated? (cdr toks) toks))
  (define-values (name-tok toks1) (expect-predicate-name toks*))
  (check-predicate-name! name-tok)
  (define name-sym (string->symbol (token-value name-tok)))
  (define-values (args toks2) (parse-parenthesized-arglist toks1))
  (check-ground! "observation" name-sym args)
  (check-arity! st name-tok args)
  (define-values (end toks3) (expect toks2 'period))
  (values (datum->syntax
           #f
           `(#%probalog-observe-entry ,(fact-form name-sym args) ,negated?)
           (token-loc-span start end))
          toks3))

(define (parse-fact-or-rule toks st)
  (define start (peek toks))
  (define-values (name-tok toks1) (expect-predicate-name toks))
  (check-predicate-name! name-tok)
  (define name-sym (string->symbol (token-value name-tok)))
  (define-values (args toks2) (parse-parenthesized-arglist toks1))
  ;; Whether this turns out to be a fact or a rule, the name here is
  ;; what introduces the predicate.
  (check-arity! st name-tok args #:definition? #t)
  (define (fact-entry prob toks-after end)
    (values (datum->syntax
             #f
             `(#%probalog-fact-entry ,(fact-form name-sym args) ,prob)
             (token-loc-span start end))
            toks-after))
  (cond
    [(eq? (peek-type toks2) 'coloncolon)
     (check-ground! "fact" name-sym args)
     (check-leading-zero! (cdr toks2))
     (define-values (num-tok toks4) (expect (cdr toks2) 'number))
     (check-probability! num-tok)
     (define-values (end toks5) (expect toks4 'period))
     (fact-entry (token-value num-tok) toks5 end)]
    [(eq? (peek-type toks2) 'period)
     ;; No probability annotation at all — defaults to 1 (a certain fact).
     (check-ground! "fact" name-sym args)
     (define-values (end toks3) (expect toks2 'period))
     (fact-entry 1 toks3 end)]
    [(eq? (peek-type toks2) 'colon-dash)
     (define-values (clauses body-argss toks4) (parse-body (cdr toks2) st))
     (check-range-restricted! name-sym args body-argss)
     (define-values (end toks5) (expect toks4 'period))
     (values (datum->syntax
              #f
              `(#%probalog-rule-entry
                (rule ,(fact-form name-sym args) (list ,@clauses))
                ,(rule-scaffold args body-argss))
              (token-loc-span start end))
             toks5)]
    [else
     (probalog-read-error
      (peek-loc toks2)
      "expected '::', ':-', or '.' after ~a(...), got ~a"
      (token-value name-tok) (describe (peek-type toks2)))]))

;; Returns (values clauses body-argss toks) — clauses is the list of
;; parsed clause forms, body-argss is the argument list of each clause
;; (used for range restriction and for the Check Syntax scaffold).
(define (parse-body toks st)
  (define-values (c cargs toks1) (parse-clause toks st))
  (let loop ([toks toks1] [acc (list c)] [argss (list cargs)])
    (if (eq? (peek-type toks) 'comma)
        (let-values ([(c2 cargs2 toks2) (parse-clause (cdr toks) st)])
          (loop toks2 (cons c2 acc) (cons cargs2 argss)))
        (values (reverse acc) (reverse argss) toks))))

;; Returns (values clause-form clause-args toks).
(define (parse-clause toks st)
  (define-values (name-tok toks1) (expect-predicate-name toks))
  (check-predicate-name! name-tok)
  (define name-sym (string->symbol (token-value name-tok)))
  (define-values (args toks2) (parse-parenthesized-arglist toks1))
  (check-arity! st name-tok args)
  (values (fact-form name-sym args) args toks2))

(define (fact-form name-sym args)
  `(fact ',name-sym (list ,@(map parg-form args))))

(define (parse-parenthesized-arglist toks)
  (define-values (_ toks1) (expect toks 'lparen))
  (if (eq? (peek-type toks1) 'rparen)
      (values '() (cdr toks1))
      (let loop ([toks toks1] [acc '()])
        (define-values (a toks2) (parse-arg toks))
        (cond
          [(eq? (peek-type toks2) 'comma)
           (loop (cdr toks2) (cons a acc))]
          ;; Another argument where the closing paren should be: the
          ;; separator is what's missing, not the paren.
          [(memq (peek-type toks2) '(string number ident))
           (probalog-read-error
            (peek-loc toks2)
            "expected ',' or ')' but got ~a — arguments are separated by commas"
            (describe (peek-type toks2)))]
          [else
           (let-values ([(_ toks3) (expect toks2 'rparen)])
             (values (reverse (cons a acc)) toks3))]))))

(define (parse-arg toks)
  (define tok (peek toks))
  (define loc (token-srcloc tok))
  (case (token-type tok)
    [(string number) (values (parg (token-value tok) #f loc) (cdr toks))]
    [(ident)
     (check-variable-name! tok)
     (define sym (string->symbol (token-value tok)))
     (values (parg `(quote ,sym) sym loc) (cdr toks))]
    [else (probalog-read-error loc "expected an argument, got ~a"
                               (describe (token-type tok)))]))

;; Predicate names must start with an uppercase letter, matching the
;; convention used throughout — variables (lowercase-starting
;; identifiers) are only valid in argument position, never as the
;; head of a fact/rule/clause/query.
(define (check-predicate-name! tok)
  (define s (token-value tok))
  (unless (and (> (string-length s) 0) (char-upper-case? (string-ref s 0)))
    (probalog-read-error
     (token-srcloc tok)
     "predicate names must start with an uppercase letter: ~a" s)))

;; An identifier used as an argument must start with a lowercase
;; letter (a variable). An uppercase-starting identifier here is
;; almost always a forgotten quote around a constant (e.g. writing
;; Edge instead of "Edge"), so it's rejected rather than silently
;; treated as a pattern variable.
(define (check-variable-name! tok)
  (define s (token-value tok))
  (when (and (> (string-length s) 0) (char-upper-case? (string-ref s 0)))
    (probalog-read-error
     (token-srcloc tok)
     "'~a' starts with an uppercase letter, which is not allowed for a variable — did you mean to quote it as a string constant?"
     s)))

;; A probability written as `.5` lexes as a period followed by a
;; number, which would otherwise be reported as a missing probability
;; — true, but unhelpful when the intent is obvious.
(define (check-leading-zero! toks)
  (when (and (eq? (peek-type toks) 'period)
             (pair? (cdr toks))
             (eq? (token-type (cadr toks)) 'number))
    (probalog-read-error
     (peek-loc toks)
     "probabilities need a leading zero: write 0.~a rather than .~a"
     (token-value (cadr toks)) (token-value (cadr toks)))))

;; Probabilities must lie in [0, 1].
(define (check-probability! tok)
  (define p (token-value tok))
  (unless (and (>= p 0) (<= p 1))
    (probalog-read-error
     (token-srcloc tok)
     "probability must be between 0 and 1, got ~a" p)))

;; Facts and queries must be ground: every argument must be a
;; constant, never a variable. A "fact" with a variable isn't
;; actually a fact — standard Datalog treats it as a
;; universally-quantified rule, which breaks the finiteness/
;; termination assumptions bottom-up evaluation relies on. A query
;; with a variable makes no sense either, since a query has no body
;; to bind that variable against — set-member? does a literal lookup,
;; not unification, so an unbound variable there could never match
;; anything meaningfully.
(define (check-ground! kind name args)
  (define vars (filter parg-var args))
  (unless (null? vars)
    (probalog-read-error
     (parg-loc (car vars))
     "~a '~a' contains the variable ~a — every argument of a ~a must be a constant"
     kind name (parg-var (car vars)) kind)))

;; A rule's head variables must all appear in its body (range
;; restriction) — otherwise a head variable could never be bound to
;; any value, and the rule could never actually fire.
(define (check-range-restricted! name head-args body-argss)
  (define body-vars (for*/list ([args body-argss] [a args] #:when (parg-var a))
                      (parg-var a)))
  (for ([a head-args] #:when (parg-var a))
    (unless (memq (parg-var a) body-vars)
      (probalog-read-error
       (parg-loc a)
       "variable '~a' in the head of rule '~a' does not appear in its body"
       (parg-var a) name))))

;; Every use of a predicate name (as a fact, a rule head, a body
;; clause, or a query) across the whole file must agree on argument
;; count. A mismatch is almost always a typo, and would otherwise fail
;; silently — a clause with the wrong arity just never unifies, with
;; no diagnostic pointing at why a rule never fires.
;;
;; Every occurrence passes through here, so this is also where they
;; are recorded for the Check Syntax scaffold. `definition?` marks the
;; ones that introduce the predicate — a fact declaration or a rule
;; head — as opposed to the ones that merely refer to it.
(define (check-arity! st name-tok args #:definition? [definition? #f])
  (define name (string->symbol (token-value name-tok)))
  (define occs (pstate-occs st))
  (set-box! occs (cons (vector name name-tok definition?) (unbox occs)))
  (define arities (pstate-arities st))
  (define n (length args))
  (define existing (hash-ref arities name #f))
  (cond
    [(not existing) (hash-set! arities name (cons n name-tok))]
    [(not (= (car existing) n))
     (probalog-read-error
      (token-srcloc name-tok)
      "'~a' is used with ~a argument~a here, but with ~a on line ~a"
      name n (if (= n 1) "" "s") (car existing)
      (srcloc-line (token-srcloc (cdr existing))))]))
