#lang racket/base
(require "../lexer.rkt")
(provide get-syntax-token)

;; Syntax coloring for DrRacket, supplied through the language's
;; `color-lexer` info key. It runs on the same scanner the parser
;; uses, so the two can't disagree about what a token is; the scanner
;; never raises, reporting malformed input as an 'error token instead.
;;
;; The mapping is chosen so the three kinds of thing a Datalog reader
;; needs to tell apart are visually distinct: predicate names read as
;; keywords, variables as plain symbols, and constants as constants.
(define (get-syntax-token in)
  (define tok (next-token in))
  (define loc (token-srcloc tok))
  (define start (srcloc-position loc))
  (define span (srcloc-span loc))
  (define type (token-type tok))
  (values (lexeme tok)
          (color-of tok)
          (paren-of type)
          start
          (if (eq? type 'eof)
              (and start span (+ start span))
              (+ start span))))

(define (color-of tok)
  (case (token-type tok)
    [(white-space) 'white-space]
    [(comment) 'comment]
    [(string) 'string]
    [(number) 'constant]
    [(ident) (if (predicate-name? (token-value tok)) 'keyword 'symbol)]
    [(lparen rparen comma period) 'parenthesis]
    [(coloncolon colon-dash question bang tilde) 'parenthesis]
    [(error) 'error]
    [(eof) 'eof]
    [else 'other]))

;; Only `(` and `)` participate in paren matching; the statement
;; terminator and the operators are punctuation, not delimiters.
(define (paren-of type)
  (case type
    [(lparen) '|(|]
    [(rparen) '|)|]
    [else #f]))

(define (predicate-name? s)
  (and (string? s)
       (> (string-length s) 0)
       (char-upper-case? (string-ref s 0))))

;; The token's source text, reconstructed. DrRacket does not use this
;; for coloring, so an approximation is fine for the tokens whose
;; original text isn't retained.
(define (lexeme tok)
  (case (token-type tok)
    [(lparen) "("]
    [(rparen) ")"]
    [(comma) ","]
    [(period) "."]
    [(coloncolon) "::"]
    [(colon-dash) ":-"]
    [(question) "?"]
    [(bang) "!"]
    [(tilde) "~"]
    [(ident) (token-value tok)]
    [(string) (format "~s" (token-value tok))]
    [(number) (format "~a" (token-value tok))]
    [else ""]))
