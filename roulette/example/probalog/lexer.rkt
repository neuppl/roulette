#lang racket/base
(require syntax/readerr)
(provide tokenize next-token (struct-out token)
         probalog-read-error token-loc-span
         peek-statement-kind)

;; type is a symbol: 'ident 'string 'number 'lparen 'rparen 'comma
;; 'period 'colon-dash 'coloncolon 'question 'bang 'tilde 'eof
;; value is the token's payload (a string, a number, or #f for
;; punctuation tokens where the type alone is enough).
;; srcloc locates the token in the source, for error reporting and for
;; the source locations attached to the syntax the parser emits.
;;
;; `next-token` additionally produces 'white-space and 'comment tokens
;; (which `tokenize` discards) and 'error tokens (which `tokenize`
;; turns into read errors). The syntax colorer consumes all of them.
(struct token (type value srcloc) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Errors

;; Signals a read error at the given location. DrRacket (and any
;; language server speaking the same protocol) highlights the reported
;; span, so parse failures point at the offending text rather than at
;; the internals of this parser.
(define (probalog-read-error loc fmt . args)
  (raise-read-error (apply format fmt args)
                    (srcloc-source loc)
                    (srcloc-line loc)
                    (srcloc-column loc)
                    (srcloc-position loc)
                    (srcloc-span loc)))

;; The location covering everything from the start of `from` to the
;; end of `to`, for reporting against a whole statement.
(define (token-loc-span from to)
  (define a (token-srcloc from))
  (define b (token-srcloc to))
  (srcloc (srcloc-source a)
          (srcloc-line a)
          (srcloc-column a)
          (srcloc-position a)
          (and (srcloc-position a) (srcloc-position b) (srcloc-span b)
               (- (+ (srcloc-position b) (srcloc-span b))
                  (srcloc-position a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive input

;; What's next in the port: 'probalog for a statement, 'racket for
;; anything else, or #f if only whitespace and comments remain.
;;
;; At the interactions prompt both kinds of input are legitimate — the
;; language exports racket/base, so the database can be examined
;; directly — and they have to be told apart before either is read,
;; since neither reader can recover from the other's syntax. A
;; statement always starts with '?', '!', or a predicate name, and
;; predicate names are capitalized; nothing anyone would usefully type
;; as Racket at this prompt starts that way.
;;
;; Peeks rather than reads, so the caller's reader still sees the
;; whole port. Note that the leading whitespace and comments skipped
;; here are Probalog's: '%' starts a comment in this language but is
;; an ordinary symbol character in Racket, so a submission of nothing
;; but comments has to be recognized as empty here rather than handed
;; to the Racket reader, which would see a symbol.
(define (peek-statement-kind in)
  ;; The skip is atomic: without (?>...) the matcher would give back
  ;; part of a trailing comment so that (.) could succeed, and a
  ;; submission of nothing but a comment would look like input.
  (define m (regexp-match-peek #px"^(?>(?:[[:space:]]|%[^\n]*)*)(.)" in))
  (cond
    [(not (and m (cadr m))) #f]
    [else
     (define b (bytes-ref (cadr m) 0))
     (if (or (= b (char->integer #\?))
             (= b (char->integer #\!))
             (and (< b 128) (char-upper-case? (integer->char b))))
         'probalog
         'racket)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scanner

;; Reads one token, never raising: anything unrecognized comes back as
;; an 'error token whose value is a message, and the offending
;; character is consumed so that a caller scanning for coloring always
;; makes progress. Always consumes at least one character unless the
;; port is at eof.
(define (next-token port [src #f])
  (port-count-lines! port)
  (define-values (line col pos) (port-next-location port))
  (define (finish type value)
    (define-values (_line _col end) (port-next-location port))
    (token type value
           (srcloc src line col pos (and pos end (- end pos)))))
  (define c (peek-char port))
  (cond
    [(eof-object? c) (finish 'eof #f)]
    [(char-whitespace? c) (skip-whitespace port) (finish 'white-space #f)]
    [(char=? c #\%) (skip-line-comment port) (finish 'comment #f)]
    [(char-alphabetic? c) (finish 'ident (read-ident port))]
    [(char-numeric? c) (finish 'number (read-number port))]
    [(and (char=? c #\-) (digit-follows? port 1)) (finish 'number (read-number port))]
    [(char=? c #\")
     (define s (read-string-lit port))
     (if s
         (finish 'string s)
         (finish 'error "unterminated string literal"))]
    [(char=? c #\() (read-char port) (finish 'lparen #f)]
    [(char=? c #\)) (read-char port) (finish 'rparen #f)]
    [(char=? c #\,) (read-char port) (finish 'comma #f)]
    [(char=? c #\?) (read-char port) (finish 'question #f)]
    [(char=? c #\!) (read-char port) (finish 'bang #f)]
    [(char=? c #\~) (read-char port) (finish 'tilde #f)]
    [(char=? c #\.) (read-char port) (finish 'period #f)]
    [(char=? c #\:)
     (read-char port)
     (define c2 (peek-char port))
     (cond
       [(and (char? c2) (char=? c2 #\-)) (read-char port) (finish 'colon-dash #f)]
       [(and (char? c2) (char=? c2 #\:)) (read-char port) (finish 'coloncolon #f)]
       [else (finish 'error "expected ':-' or '::' after ':'")])]
    [else
     (read-char port)
     (finish 'error (format "unexpected character: ~a" c))]))

;; Read the entire port and return a list of tokens ending in an 'eof
;; token, discarding whitespace and comments. Raises a read error, at
;; the location of the offending text, on any character that doesn't
;; start a valid token.
(define (tokenize port [src #f])
  (let loop ([acc '()])
    (define tok (next-token port src))
    (case (token-type tok)
      [(white-space comment) (loop acc)]
      [(error) (probalog-read-error (token-srcloc tok) "~a" (token-value tok))]
      [(eof) (reverse (cons tok acc))]
      [else (loop (cons tok acc))])))

(define (digit-follows? port k)
  (define c (peek-char port k))
  (and (char? c) (char-numeric? c)))

(define (skip-whitespace port)
  (let loop ()
    (define c (peek-char port))
    (when (and (char? c) (char-whitespace? c))
      (read-char port)
      (loop))))

(define (skip-line-comment port)
  (let loop ()
    (define c (read-char port))
    (unless (or (eof-object? c) (char=? c #\newline))
      (loop))))

;; Reads an identifier: a letter followed by letters/digits/underscores.
;; Classification (predicate name vs. variable) happens in the parser,
;; based on the case of the first character.
(define (read-ident port)
  (define out (open-output-string))
  (let loop ()
    (define c (peek-char port))
    (when (and (char? c)
               (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))
      (write-char (read-char port) out)
      (loop)))
  (get-output-string out))

;; Reads a number: an optional leading '-', then digits, optionally
;; followed by '.' + more digits — but only consumes the '.' if a
;; digit actually follows it, so that e.g. "0.8." (probability
;; immediately followed by the statement terminator) correctly lexes
;; as NUMBER("0.8") then PERIOD, not as a malformed number swallowing
;; the terminator.
(define (read-number port)
  (define out (open-output-string))
  (when (char=? (peek-char port) #\-)
    (write-char (read-char port) out))
  (let loop ()
    (define c (peek-char port))
    (when (and (char? c) (char-numeric? c))
      (write-char (read-char port) out)
      (loop)))
  (define c (peek-char port))
  (when (and (char? c) (char=? c #\.) (digit-follows? port 1))
    (write-char (read-char port) out) ; consume '.'
    (let loop ()
      (define c3 (peek-char port))
      (when (and (char? c3) (char-numeric? c3))
        (write-char (read-char port) out)
        (loop))))
  (string->number (get-output-string out)))

;; Reads a double-quoted string literal, or returns #f if the line (or
;; the port) ends before the closing quote. No escape-sequence support
;; (not needed for the constants used in these programs so far).
;;
;; A literal stops at a newline so that a missing closing quote is
;; reported against the line that opened it. Letting one run on would
;; swallow the rest of the program up to some later quote, and blame
;; whatever line that turned out to be.
(define (read-string-lit port)
  (read-char port) ; opening quote
  (define out (open-output-string))
  (let loop ()
    (define c (peek-char port))
    (cond
      [(eof-object? c) #f]
      [(char=? c #\newline) #f] ; left unconsumed: the next line is code
      [else
       (read-char port)
       (cond
         [(char=? c #\") (get-output-string out)]
         [else (write-char c out) (loop)])])))
