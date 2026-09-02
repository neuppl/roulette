#lang racket/base
(require "../parser.rkt" "../lexer.rkt")
(provide configure)

;; Reading for the interactions area. Without this, a REPL submission
;; would be read as an S-expression, which Probalog statements are
;; not; `current-read-interaction` replaces that with the language's
;; own parser.
(define (configure data)
  (current-read-interaction read-one-statement))

;; `current-read-interaction` hands back one form per call, but the
;; parser consumes a whole port at once (statements are delimited by
;; periods, and the parser checks arity agreement across everything it
;; sees). So a submission is parsed once and its statements are handed
;; out one at a time. The port is weakly held: DrRacket makes a fresh
;; one per submission, and nothing needs to outlive it.
(define pending (make-weak-hasheq))

(define (read-one-statement src in)
  (define forms (hash-ref pending in '()))
  (cond
    [(pair? forms)
     (hash-set! pending in (cdr forms))
     (car forms)]
    ;; Racket expressions are also accepted at the prompt, since the
    ;; language exports racket/base and the saturated database is
    ;; bound to `probalog-result`.
    [(eq? (peek-statement-kind in) 'racket) (read-syntax src in)]
    ;; Only whitespace and comments left. Racket's reader would take
    ;; the '%' of a comment for a symbol, so don't hand it the port.
    [(not (peek-statement-kind in)) eof]
    [else
     (define parsed (parse-probalog in src #:scaffold? #f))
     (cond
       [(null? parsed) eof]
       [else
        (hash-set! pending in (cdr parsed))
        (car parsed)])]))
