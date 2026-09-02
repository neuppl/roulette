#lang racket/base
(require (for-syntax racket/base syntax/parse racket/path)
         roulette/example/probalog/probalog-core
         roulette/example/probalog/probalog-set-equal)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [probalog-module-begin #%module-begin])
         (all-from-out roulette/example/probalog/probalog-core)
         (all-from-out roulette/example/probalog/probalog-set-equal)
         #%probalog-fact-entry
         #%probalog-rule-entry
         #%probalog-query-entry
         #%probalog-observe-entry
         #%probalog-scaffold)

;; #%module-begin for the probalog language. Facts and rules are
;; collected first (the database must be fully built before anything
;; can be queried or observed). Queries and observations are then
;; emitted in their original source order relative to each other, so
;; queries before the first observation report priors and queries
;; after report posteriors conditioned on all preceding observations.
;;
;; Scaffolds are dead code, produced by the parser, that gives
;; variables and predicate names real binding structure so that Check
;; Syntax can draw arrows between their occurrences: one per rule for
;; its variables, and one for the file's predicate names. They are
;; emitted alongside the statements but never run.
(define-syntax (probalog-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     ;; The database is bound with the module body's own lexical
     ;; context rather than this macro's, so that `probalog-result`
     ;; typed in the interactions area — or reached by a REPL
     ;; statement, which the parser produces with no context of its
     ;; own — refers to it. A macro-introduced binding would be
     ;; invisible to both.
     (define result-id (datum->syntax stx 'probalog-result))
     ;; First pass: collect facts and rules.
     (define-values (fact-forms rule-forms scaffolds)
       (for/fold ([facts '()] [rules '()] [scaffolds '()])
                 ([f (syntax->list #'(form ...))])
         (syntax-parse f
           #:datum-literals (#%probalog-fact-entry #%probalog-rule-entry
                             #%probalog-scaffold)
           [(#%probalog-fact-entry fact-expr prob-expr)
            (values (cons #'(cons fact-expr prob-expr) facts) rules scaffolds)]
           [(#%probalog-rule-entry rule-expr scaffold-expr)
            (values facts (cons #'rule-expr rules) (cons #'scaffold-expr scaffolds))]
           [(#%probalog-rule-entry rule-expr)
            (values facts (cons #'rule-expr rules) scaffolds)]
           [(#%probalog-scaffold scaffold-expr)
            (values facts rules (cons #'scaffold-expr scaffolds))]
           [_ (values facts rules scaffolds)])))
     ;; Second pass: build the ordered sequence of query/observe
     ;; statements, preserving source order.
     (define ordered-stmts
       (for/list ([f (syntax->list #'(form ...))])
         (with-syntax ([probalog-result result-id]
                       ;; So a failure at run time — an impossible
                       ;; observation, say — names the statement that
                       ;; caused it, the way a parse error does.
                       [where (where-string f)])
           (syntax-parse f
             #:datum-literals (#%probalog-query-entry #%probalog-observe-entry)
             [(#%probalog-query-entry query-expr)
              (syntax/loc f
                (printf "~a: ~a\n" query-expr
                        (query-result->string
                         (query-fact probalog-result query-expr #:where where))))]
             [(#%probalog-observe-entry fact-expr negated?)
              (if (syntax-e #'negated?)
                  (syntax/loc f (observe-not-fact probalog-result fact-expr #:where where))
                  (syntax/loc f (observe-fact probalog-result fact-expr #:where where)))]
             [_ #'(void)]))))
     (with-syntax ([(fact-e ...)  (reverse fact-forms)]
                    [(rule-e ...)  (reverse rule-forms)]
                    [(scaffold-e ...) (reverse scaffolds)]
                    [(stmt-e ...)  ordered-stmts]
                    [probalog-result result-id])
       #'(#%plain-module-begin
          scaffold-e ...
          (define probalog-result
            (run-datalog (list fact-e ...) (list rule-e ...)))
          (provide probalog-result)
          stmt-e ...))]))

;; "file.pdl:5:0" for a statement, or #f if it has no location (as
;; when it came from the interactions prompt).
(define-for-syntax (where-string stx)
  (define src (syntax-source stx))
  (define line (syntax-line stx))
  (and src line
       (format "~a:~a:~a"
               (if (path? src) (file-name-from-path src) src)
               line
               (syntax-column stx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive use

;; At the REPL the parser's output arrives one statement at a time,
;; with no #%module-begin to rewrite it, so the marker forms are bound
;; as macros too. They query and observe against the `probalog-result`
;; of the module being interacted with; `datum->syntax` on the use
;; site is what lets that reference resolve to the module's binding
;; rather than to anything in this module.
;; A query at the prompt displays its result the same way one in a
;; module body does — a concrete outcome as itself, anything else as a
;; distribution — rather than letting the printer show the underlying
;; pmf structure. The fact isn't echoed back, since the user just
;; typed it.
(define-syntax (#%probalog-query-entry stx)
  (syntax-parse stx
    [(_ query-expr)
     (with-syntax ([result (datum->syntax stx 'probalog-result)])
       (syntax/loc stx
         (displayln (query-result->string (query-fact result query-expr)))))]))

(define-syntax (#%probalog-observe-entry stx)
  (syntax-parse stx
    [(_ fact-expr negated?)
     (with-syntax ([result (datum->syntax stx 'probalog-result)])
       (if (syntax-e #'negated?)
           (syntax/loc stx (observe-not-fact result fact-expr))
           (syntax/loc stx (observe-fact result fact-expr))))]))

;; Facts and rules can't be added interactively: the database is
;; saturated once, when the module is run, and every query is answered
;; against that fixed result.
(define-syntax (#%probalog-fact-entry stx)
  (raise-syntax-error #f "facts can only be declared in a module body" stx))

(define-syntax (#%probalog-rule-entry stx)
  (raise-syntax-error #f "rules can only be declared in a module body" stx))

;; Only meaningful in a module body, where Check Syntax can annotate
;; it; harmless if one reaches the prompt.
(define-syntax (#%probalog-scaffold stx) #'(void))
