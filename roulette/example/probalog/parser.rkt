#lang racket/base
(require "lexer.rkt")
(provide parse-probalog)

;; Parses the entire port and returns a list of S-expressions, each
;; one of:
;;   (#%probalog-fact-entry (fact 'Name (list arg ...)) prob)
;;   (#%probalog-rule-entry (rule (fact 'Name (list arg ...))
;;                                 (list (fact 'Name (list arg ...)) ...)))
;;   (#%probalog-query-entry (fact 'Name (list arg ...)))
;; These head symbols are recognized by probalog/expander.rkt's
;; #%module-begin, which consumes and rewrites them — they're never
;; actually bound to real functions/macros.
(define (parse-probalog port)
  (define toks (tokenize port))
  (define arities (make-hash)) ; predicate name -> arity, checked across the whole file
  (let loop ([toks toks] [acc '()])
    (if (eq? (token-type (car toks)) 'eof)
        (reverse acc)
        (let-values ([(form rest) (parse-statement toks arities)])
          (loop rest (cons form acc))))))

;; --- token stream helpers -------------------------------------------

(define (peek toks) (car toks))
(define (peek-type toks) (token-type (car toks)))

(define (expect toks type)
  (unless (eq? (peek-type toks) type)
    (error 'probalog-parser "expected ~a but got ~a (~a)"
           type (peek-type toks) (token-value (peek toks))))
  (values (peek toks) (cdr toks)))

;; --- grammar ----------------------------------------------------------
;; statement := fact-or-rule-decl | query-decl
;; fact-or-rule-decl := NAME '(' arglist ')' ( '::' NUMBER | ':-' body | ε ) '.'
;;   -- omitting '::' NUMBER entirely (i.e. just NAME(args).) declares
;;      a fact with an implicit probability of 1 (a certain fact).
;;   -- NUMBER after '::' must be in [0, 1].
;;   -- every variable in the head must also appear in some body clause
;;      (range restriction) — otherwise it could never be bound.
;; query-decl        := '?' NAME '(' arglist ')' '.'
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

(define (parse-statement toks arities)
  (if (eq? (peek-type toks) 'question)
      (parse-query (cdr toks) arities)
      (parse-fact-or-rule toks arities)))

(define (parse-query toks arities)
  (define-values (name-tok toks1) (expect toks 'ident))
  (check-predicate-name! (token-value name-tok))
  (define name-sym (string->symbol (token-value name-tok)))
  (define-values (args toks2) (parse-parenthesized-arglist toks1))
  (check-ground! "query" name-sym args)
  (check-arity! arities name-sym args)
  (define-values (_ toks3) (expect toks2 'period))
  (values `(#%probalog-query-entry
             (fact ',name-sym (list ,@args)))
          toks3))

(define (parse-fact-or-rule toks arities)
  (define-values (name-tok toks1) (expect toks 'ident))
  (check-predicate-name! (token-value name-tok))
  (define name-sym (string->symbol (token-value name-tok)))
  (define-values (args toks2) (parse-parenthesized-arglist toks1))
  (check-arity! arities name-sym args)
  (cond
    [(eq? (peek-type toks2) 'coloncolon)
     (check-ground! "fact" name-sym args)
     (define-values (_ toks3) (values #f (cdr toks2)))
     (define-values (num-tok toks4) (expect toks3 'number))
     (check-probability! (token-value num-tok))
     (define-values (__ toks5) (expect toks4 'period))
     (values `(#%probalog-fact-entry
                (fact ',name-sym (list ,@args))
                ,(token-value num-tok))
             toks5)]
    [(eq? (peek-type toks2) 'period)
     ;; No probability annotation at all — defaults to 1 (a certain fact).
     (check-ground! "fact" name-sym args)
     (define-values (_ toks3) (expect toks2 'period))
     (values `(#%probalog-fact-entry
                (fact ',name-sym (list ,@args))
                1)
             toks3)]
    [(eq? (peek-type toks2) 'colon-dash)
     (define-values (_ toks3) (values #f (cdr toks2)))
     (define-values (clauses body-vars toks4) (parse-body toks3 arities))
     (check-range-restricted! name-sym args body-vars)
     (define-values (__ toks5) (expect toks4 'period))
     (values `(#%probalog-rule-entry
                (rule (fact ',name-sym (list ,@args))
                      (list ,@clauses)))
             toks5)]
    [else
     (error 'probalog-parser
            "expected '::', ':-', or '.' after ~a(...), got ~a"
            (token-value name-tok) (peek-type toks2))]))

;; Returns (values clauses body-vars toks) — clauses is the list of
;; parsed clause forms, body-vars is every variable symbol appearing
;; in any of their argument lists (used to check range restriction).
(define (parse-body toks arities)
  (define-values (c cargs toks1) (parse-clause toks arities))
  (let loop ([toks toks1] [acc (list c)] [vars (arg-vars cargs)])
    (if (eq? (peek-type toks) 'comma)
        (let-values ([(c2 cargs2 toks2) (parse-clause (cdr toks) arities)])
          (loop toks2 (cons c2 acc) (append (arg-vars cargs2) vars)))
        (values (reverse acc) vars toks))))

;; Returns (values clause-form clause-args toks).
(define (parse-clause toks arities)
  (define-values (name-tok toks1) (expect toks 'ident))
  (check-predicate-name! (token-value name-tok))
  (define name-sym (string->symbol (token-value name-tok)))
  (define-values (args toks2) (parse-parenthesized-arglist toks1))
  (check-arity! arities name-sym args)
  (values `(fact ',name-sym (list ,@args)) args toks2))

(define (parse-parenthesized-arglist toks)
  (define-values (_ toks1) (expect toks 'lparen))
  (if (eq? (peek-type toks1) 'rparen)
      (values '() (cdr toks1))
      (let loop ([toks toks1] [acc '()])
        (define-values (a toks2) (parse-arg toks))
        (if (eq? (peek-type toks2) 'comma)
            (loop (cdr toks2) (cons a acc))
            (let-values ([(_ toks3) (expect toks2 'rparen)])
              (values (reverse (cons a acc)) toks3))))))

(define (parse-arg toks)
  (case (peek-type toks)
    [(string) (values (token-value (peek toks)) (cdr toks))]
    [(number) (values (token-value (peek toks)) (cdr toks))]
    [(ident)
     (define name (token-value (peek toks)))
     (check-variable-name! name)
     (values `(quote ,(string->symbol name)) (cdr toks))]
    [else (error 'probalog-parser "expected an argument, got ~a" (peek-type toks))]))

;; Predicate names must start with an uppercase letter, matching the
;; convention used throughout — variables (lowercase-starting
;; identifiers) are only valid in argument position, never as the
;; head of a fact/rule/clause/query.
(define (check-predicate-name! s)
  (unless (and (> (string-length s) 0) (char-upper-case? (string-ref s 0)))
    (error 'probalog-parser
           "predicate names must start with an uppercase letter: ~a" s)))

;; An identifier used as an argument must start with a lowercase
;; letter (a variable). An uppercase-starting identifier here is
;; almost always a forgotten quote around a constant (e.g. writing
;; Edge instead of "Edge"), so it's rejected rather than silently
;; treated as a pattern variable.
(define (check-variable-name! s)
  (when (and (> (string-length s) 0) (char-upper-case? (string-ref s 0)))
    (error 'probalog-parser
           "'~a' starts with an uppercase letter, which is not allowed for a variable — did you mean to quote it as a string constant?"
           s)))

;; Probabilities must lie in [0, 1].
(define (check-probability! p)
  (unless (and (>= p 0) (<= p 1))
    (error 'probalog-parser "probability must be between 0 and 1, got ~a" p)))

;; Every variable symbol appearing in a list of parsed argument forms
;; (a variable arg is represented as (quote sym); constants are plain
;; self-evaluating strings/numbers).
(define (arg-vars args)
  (for/list ([a args] #:when (and (pair? a) (eq? (car a) 'quote)))
    (cadr a)))

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
  (define vars (arg-vars args))
  (unless (null? vars)
    (error 'probalog-parser
           "~a '~a' contains variable(s) ~a — every argument of a ~a must be a constant"
           kind name vars kind)))

;; A rule's head variables must all appear in its body (range
;; restriction) — otherwise a head variable could never be bound to
;; any value, and the rule could never actually fire.
(define (check-range-restricted! name head-args body-vars)
  (for ([v (arg-vars head-args)])
    (unless (memq v body-vars)
      (error 'probalog-parser
             "variable '~a' in the head of rule '~a' does not appear in its body"
             v name))))

;; Every use of a predicate name (as a fact, a rule head, a body
;; clause, or a query) across the whole file must agree on argument
;; count. A mismatch is almost always a typo, and would otherwise fail
;; silently — a clause with the wrong arity just never unifies, with
;; no diagnostic pointing at why a rule never fires.
(define (check-arity! arities name args)
  (define n (length args))
  (define existing (hash-ref arities name #f))
  (cond
    [(not existing) (hash-set! arities name n)]
    [(not (= existing n))
     (error 'probalog-parser
            "'~a' is used with ~a argument(s) here, but with ~a argument(s) elsewhere in the file"
            name n existing)]))
