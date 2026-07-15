#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide bif-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "bif-lex.rkt"
         racket/match
         parser-tools/lex
         parser-tools/yacc
         syntax/readerr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader

(define (bif-parse [source-name (object-name (current-input-port))]
                   [port (current-input-port)])
  (define parse (bayes-parser source-name))
  (parse (λ () (bayes-lex port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser

(define (bayes-parser src-name)
  (parser
   (start blocks)
   (end eof)
   (tokens non-empty-tokens empty-tokens)
   (src-pos)
   (error (make-read-error src-name))
   (grammar
    (blocks [(block blocks) (cons $1 $2)]
            [() null])
    (block [(keyword lp exprs rp lc properties rc) (list* $1 $3 $6)]
           [(keyword word lc properties rc) (list* $1 $2 $4)]
           [(keyword lc properties rc) (cons $1 $3)])
    (exprs [(expr exprs) (cons $1 $2)]
           [() null])
    (expr [(word) $1]
          [(decimal) $1]
          [(floating) $1]
          [(keyword) $1]
          [(group) $1])
    (group [(lp exprs rp) $2]
           [(lc exprs rc) $2]
           [(lb exprs rb) $2])
    (properties [(exprs end properties) (cons $1 $3)]
                [() null]))))

(define (make-read-error src-name)
  (λ (tok-ok? tok-name tok-value start end)
    (match-define (position start-offset start-line start-col) start)
    (match-define (position end-offset end-line end-col) end)
    (define span (and start-offset end-offset (- end-offset start-offset)))
    (raise-read-error "read" src-name start-line start-col start-offset span)))
