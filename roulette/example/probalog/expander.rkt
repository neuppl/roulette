#lang racket/base
(require (for-syntax racket/base syntax/parse)
         roulette/example/probalog/probalog-core
         roulette/example/probalog/probalog-set-equal)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [probalog-module-begin #%module-begin])
         (all-from-out roulette/example/probalog/probalog-core)
         (all-from-out roulette/example/probalog/probalog-set-equal))

;; #%module-begin for the probalog language. Facts and rules are
;; collected first (the database must be fully built before anything
;; can be queried or observed). Queries and observations are then
;; emitted in their original source order relative to each other, so
;; queries before the first observation report priors and queries
;; after report posteriors conditioned on all preceding observations.
(define-syntax (probalog-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     ;; First pass: collect facts and rules.
     (define-values (fact-forms rule-forms)
       (for/fold ([facts '()] [rules '()])
                 ([f (syntax->list #'(form ...))])
         (syntax-parse f
           #:datum-literals (#%probalog-fact-entry #%probalog-rule-entry)
           [(#%probalog-fact-entry fact-expr prob-expr)
            (values (cons #'(cons fact-expr prob-expr) facts) rules)]
           [(#%probalog-rule-entry rule-expr)
            (values facts (cons #'rule-expr rules))]
           [_ (values facts rules)])))
     ;; Second pass: build the ordered sequence of query/observe
     ;; statements, preserving source order.
     (define ordered-stmts
       (for/list ([f (syntax->list #'(form ...))])
         (syntax-parse f
           #:datum-literals (#%probalog-query-entry #%probalog-observe-entry)
           [(#%probalog-query-entry query-expr)
            #'(printf "~a: ~a\n" query-expr
                      (query-fact probalog-result query-expr))]
           [(#%probalog-observe-entry fact-expr negated?)
            #'(if negated?
                  (observe-not-fact probalog-result fact-expr)
                  (observe-fact probalog-result fact-expr))]
           [_ #'(void)])))
     (with-syntax ([(fact-e ...)  (reverse fact-forms)]
                    [(rule-e ...)  (reverse rule-forms)]
                    [(stmt-e ...)  ordered-stmts])
       #'(#%plain-module-begin
          (define probalog-result
            (run-datalog (list fact-e ...) (list rule-e ...)))
          (provide probalog-result)
          stmt-e ...))]))