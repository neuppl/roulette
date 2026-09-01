#lang racket/base
(require (for-syntax racket/base syntax/parse)
         roulette/example/probalog/probalog-core
         roulette/example/probalog/probalog-set-equal)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [probalog-module-begin #%module-begin])
         (all-from-out roulette/example/probalog/probalog-core)
         (all-from-out roulette/example/probalog/probalog-set-equal))

;; #%module-begin for the probalog language. Scans the module's body
;; forms at compile time for the four entry shapes the parser emits,
;; and rewrites the whole body into:
;;   - one `run-datalog` call collecting every fact/rule declared
;;   - one observe-fact/observe-not-fact call per observation, applied
;;     after the database is built so the guard formulas exist
;;   - one printf per query, reporting its (now conditioned) probability
;;
;; Ordering: database is built first, then observations are applied in
;; source order, then queries run. This is the only sensible order:
;; observations condition on guards that only exist after run-datalog,
;; and queries reflect all prior observations.
(define-syntax (probalog-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     (define-values (fact-forms rule-forms observe-forms query-forms other-forms)
       (for/fold ([facts '()] [rules '()] [observes '()] [queries '()] [others '()])
                 ([f (syntax->list #'(form ...))])
         (syntax-parse f
           #:datum-literals (#%probalog-fact-entry #%probalog-rule-entry
                             #%probalog-observe-entry #%probalog-query-entry)
           [(#%probalog-fact-entry fact-expr prob-expr)
            (values (cons #'(cons fact-expr prob-expr) facts) rules observes queries others)]
           [(#%probalog-rule-entry rule-expr)
            (values facts (cons #'rule-expr rules) observes queries others)]
           [(#%probalog-observe-entry fact-expr negated?)
            (values facts rules (cons #'(cons fact-expr negated?) observes) queries others)]
           [(#%probalog-query-entry query-expr)
            (values facts rules observes (cons #'query-expr queries) others)]
           [_ (values facts rules observes queries (cons f others))])))
     (with-syntax ([(fact-e ...)    (reverse fact-forms)]
                    [(rule-e ...)    (reverse rule-forms)]
                    [(observe-e ...) (reverse observe-forms)]
                    [(query-e ...)   (reverse query-forms)]
                    [(other-e ...)   (reverse other-forms)])
       #'(#%plain-module-begin
          other-e ...
          (define probalog-result
            (run-datalog (list fact-e ...) (list rule-e ...)))
          (provide probalog-result)
          (for ([obs (list observe-e ...)])
            (if (cdr obs)
                (observe-not-fact probalog-result (car obs))
                (observe-fact probalog-result (car obs))))
          (for ([q (list query-e ...)])
            (printf "~a: ~a\n" q (query-fact probalog-result q)))))]))