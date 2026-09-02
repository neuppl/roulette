#lang s-exp syntax/module-reader
roulette/example/probalog/expander
#:read probalog-read
#:read-syntax probalog-read-syntax
#:whole-body-readers? #t
#:info probalog-info
#:language-info '#(roulette/example/probalog/lang/lang-info get-info #f)

(require "../parser.rkt")

;; Whole-body reader for plain `read`: consumes the entire port and
;; returns the list of parsed forms as data.
(define (probalog-read port)
  (map syntax->datum (parse-probalog port)))

;; Whole-body reader for `read-syntax`: the parser builds the syntax
;; objects itself, so that each form carries the source location of
;; the statement it came from and each variable occurrence carries its
;; own. The forms have no lexical context of their own and so pick up
;; the module language's bindings when spliced into the module body.
(define (probalog-read-syntax src port)
  (parse-probalog port src))

;; Editor support. DrRacket consults these keys directly; other tools
;; (including racket-langserver, and so VS Code) read the same
;; information through `read-language`.
(define (probalog-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'roulette/example/probalog/tool/syntax-color 'get-syntax-token)]
    [(drracket:submit-predicate)
     (dynamic-require 'roulette/example/probalog/tool/submit 'repl-submit?)]
    [(drracket:indentation)
     (dynamic-require 'roulette/example/probalog/tool/indentation 'indent-probalog)]
    ;; Statements end at a period, and only parentheses nest, so the
    ;; s-expression defaults for these would be actively wrong.
    [(drracket:comment-delimiters) '((line "%" " "))]
    [(drracket:paren-matches) '((|(| |)|))]
    [(drracket:quote-matches) '(#\")]
    [(drracket:default-filters) '(["Probalog Sources" "*.pdl"])]
    [(drracket:default-extension) "pdl"]
    [else (default-filter key default)]))
