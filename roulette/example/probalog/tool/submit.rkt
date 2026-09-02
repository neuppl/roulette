#lang racket/base
(require "../lexer.rkt")
(provide repl-submit?)

;; Decides whether what the user has typed into the interactions area
;; is ready to run, supplied through the `drracket:submit-predicate`
;; info key. A Probalog statement ends at a period rather than at a
;; balanced parenthesis, so the s-expression default is wrong here.
;;
;; Scanning with the real lexer (rather than looking for a trailing
;; `.` in the text) is what makes `Edge("a", "b") :: 0.5.` submit at
;; the right point: the period inside `0.5` is part of a number token,
;; not a terminator.
(define (repl-submit? in has-white-space?)
  (case (peek-statement-kind in)
    [(probalog) (probalog-complete? in)]
    [(racket) (racket-complete? in)]
    ;; nothing but whitespace and comments: not something to run
    [else #f]))

;; A Racket expression at the prompt is submitted on the usual
;; s-expression rule: it's ready when it reads without running out of
;; input. Anything that fails to read for some other reason is
;; submitted too, so the user sees the error instead of a prompt that
;; refuses to accept anything.
(define (racket-complete? in)
  (with-handlers ([exn:fail:read:eof? (lambda (e) #f)]
                  [exn:fail:read? (lambda (e) #t)])
    (let loop ([any? #f])
      (define v (read in))
      (if (eof-object? v) any? (loop #t)))))

(define (probalog-complete? in)
  (let loop ([last #f] [depth 0] [any? #f])
    (define tok (next-token in))
    (case (token-type tok)
      [(white-space comment) (loop last depth any?)]
      ;; Malformed input is submitted so the user gets the parser's
      ;; error message rather than an interactions area that silently
      ;; refuses to accept anything.
      [(error) #t]
      [(eof) (and any? (eq? last 'period) (zero? depth))]
      [(lparen) (loop 'lparen (add1 depth) #t)]
      [(rparen) (loop 'rparen (max 0 (sub1 depth)) #t)]
      [else (loop (token-type tok) depth #t)])))

(module+ test
  (require rackunit)
  (define (try str) (repl-submit? (open-input-string str) #t))
  (check-false (try ""))
  (check-false (try "  \n"))
  (check-false (try "% just a comment\n"))
  (check-false (try "? Path(\"a\", \"b\")"))
  (check-true  (try "? Path(\"a\", \"b\")."))
  (check-true  (try "? Path(\"a\", \"b\").\n  "))
  (check-false (try "? Path(\"a\", \"b\").\n? Edge(\"a\""))
  (check-true  (try "? Path(\"a\", \"b\").\n? Edge(\"a\", \"b\")."))
  ;; the period inside a probability is not a terminator
  (check-false (try "Edge(\"a\", \"b\") :: 0.5"))
  (check-true  (try "Edge(\"a\", \"b\") :: 0.5."))
  ;; but a trailing period after a whole number is one
  (check-true  (try "Edge(\"a\", \"b\") :: 0."))
  ;; a period inside a string or a comment is not a terminator
  (check-false (try "Edge(\"a.b\", \"c\")"))
  (check-false (try "? Path(\"a\", \"b\") % done.\n"))
  ;; unbalanced parentheses keep the statement open
  (check-false (try "? Path(\"a\", \"b\"."))
  (check-true  (try "! ~Path(\"a\", \"c\")."))

  ;; Racket expressions are submitted on the s-expression rule, since
  ;; they are also accepted at the prompt.
  (check-true  (try "probalog-result"))
  (check-true  (try "(set-count probalog-result)"))
  (check-false (try "(set-count probalog-result"))
  (check-true  (try "(query-fact probalog-result (fact 'Path (list \"a\" \"b\")))"))
  ;; a Racket expression is never waiting for a period
  (check-true  (try "(+ 1 2)")))
