#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "probalog-core.rkt"
         "probalog-set-equal.rkt")

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [probalog-module-begin #%module-begin])
         (all-from-out "probalog-core.rkt")
         (all-from-out "probalog-set-equal.rkt"))

;; #%module-begin for the probalog language. Scans the module's body
;; forms at compile time (not via runtime mutable state, so multiple
;; #lang probalog modules can coexist safely) for the three entry
;; shapes the parser emits, and rewrites the whole body into:
;;   - one `run-datalog` call collecting every fact/rule declared
;;   - one printf per query, reporting its probability
;;
;; Any other body forms (there normally aren't any, since a probalog
;; source file only ever contains fact/rule/query declarations) pass
;; through unchanged, ahead of the generated definitions.
(define-syntax (probalog-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     (define-values (fact-forms rule-forms query-forms other-forms)
       (for/fold ([facts '()] [rules '()] [queries '()] [others '()])
                 ([f (syntax->list #'(form ...))])
         (syntax-parse f
           #:datum-literals (#%probalog-fact-entry #%probalog-rule-entry #%probalog-query-entry)
           [(#%probalog-fact-entry fact-expr prob-expr)
            (values (cons #'(cons fact-expr prob-expr) facts) rules queries others)]
           [(#%probalog-rule-entry rule-expr)
            (values facts (cons #'rule-expr rules) queries others)]
           [(#%probalog-query-entry query-expr)
            (values facts rules (cons #'query-expr queries) others)]
           [_ (values facts rules queries (cons f others))])))
     (with-syntax ([(fact-e ...) (reverse fact-forms)]
                   [(rule-e ...) (reverse rule-forms)]
                   [(query-e ...) (reverse query-forms)]
                   [(other-e ...) (reverse other-forms)])
       #'(#%plain-module-begin
          other-e ...
          (define probalog-result
            (run-datalog (list fact-e ...) (list rule-e ...)))
          (provide probalog-result)
          (for ([q (list query-e ...)])
            (printf "~a: ~a\n" q (query-fact probalog-result q)))))]))
