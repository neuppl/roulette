#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (prefix-in : parser-tools/lex-sre)
         parser-tools/lex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexer

(define-tokens non-empty-tokens
  (word decimal floating keyword))

(define-empty-tokens empty-tokens
  (end eof lp rp lc rc lb rb))

(define-lex-abbrevs
  [word (:- (:+ (:or letter digit)) keyword)]
  [letter (:or (:/ "a" "z") (:/ "A" "Z") "_" "-")]
  [digit (:/ "0" "9")]
  [decimal (:: (:/ "1" "9") (:* digit))]
  [floating (:or (:: (:+ digit) "." (:* digit) (:? exponent))
                 (:: "." (:+ digit) (:? exponent))
                 (:: (:+ digit) (:? exponent)))]
  [exponent (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit))]
  [keyword (:: (:or "network" "variable" "probability" "property"
                    "type" "discrete" "default" "table"))]
  [space (:or #\space #\tab #\newline #\, #\|)])

(define bayes-lex
  (lexer-src-pos
   [word (token-word (string->symbol lexeme))]
   [decimal (token-decimal (string->number lexeme))]
   [floating (token-decimal (string->number lexeme))]
   [keyword (token-keyword (string->symbol lexeme))]
   ["(" (token-lp)] [")" (token-rp)]
   ["{" (token-lc)] ["}" (token-rc)]
   ["[" (token-lb)] ["]" (token-rb)]
   [";" (token-end)]
   [space (return-without-pos (bayes-lex input-port))]
   [(eof) (token-eof)]))