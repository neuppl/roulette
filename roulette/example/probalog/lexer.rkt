#lang racket/base
(provide tokenize (struct-out token))

;; type is a symbol: 'ident 'string 'number 'lparen 'rparen 'comma
;; 'period 'colon-dash 'coloncolon 'question 'eof
;; value is the token's payload (a string, a number, or #f for
;; punctuation tokens where the type alone is enough).
(struct token (type value) #:transparent)

;; Read the entire port and return a list of tokens ending in an 'eof
;; token. Errors on any character that doesn't start a valid token.
(define (tokenize port)
  (let loop ([acc '()])
    (skip-whitespace-and-comments port)
    (define c (peek-char port))
    (cond
      [(eof-object? c) (reverse (cons (token 'eof #f) acc))]
      [(char-alphabetic? c) (loop (cons (read-ident port) acc))]
      [(char-numeric? c) (loop (cons (read-number port) acc))]
      [(and (char=? c #\-) (let ([c2 (peek-char port 1)]) (and (char? c2) (char-numeric? c2))))
       (loop (cons (read-number port) acc))]
      [(char=? c #\") (loop (cons (read-string-lit port) acc))]
      [(char=? c #\() (read-char port) (loop (cons (token 'lparen #f) acc))]
      [(char=? c #\)) (read-char port) (loop (cons (token 'rparen #f) acc))]
      [(char=? c #\,) (read-char port) (loop (cons (token 'comma #f) acc))]
      [(char=? c #\?) (read-char port) (loop (cons (token 'question #f) acc))]
      [(char=? c #\.) (read-char port) (loop (cons (token 'period #f) acc))]
      [(char=? c #\:) (loop (cons (read-colon-token port) acc))]
      [else (error 'probalog-lexer "unexpected character: ~a" c)])))

(define (skip-whitespace-and-comments port)
  (let loop ()
    (define c (peek-char port))
    (cond
      [(eof-object? c) (void)]
      [(char-whitespace? c) (read-char port) (loop)]
      [(char=? c #\%)
       (let skip-line ()
         (define c2 (read-char port))
         (unless (or (eof-object? c2) (char=? c2 #\newline))
           (skip-line)))
       (loop)]
      [else (void)])))

;; Reads an identifier: a letter followed by letters/digits/underscores.
;; Classification (predicate name vs. variable) happens in the parser,
;; based on the case of the first character.
(define (read-ident port)
  (define out (open-output-string))
  (let loop ()
    (define c (peek-char port))
    (when (and (not (eof-object? c))
               (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))
      (write-char (read-char port) out)
      (loop)))
  (token 'ident (get-output-string out)))

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
    (when (and (not (eof-object? c)) (char-numeric? c))
      (write-char (read-char port) out)
      (loop)))
  (define c (peek-char port))
  (when (char=? c #\.)
    (define c2 (peek-char port 1))
    (when (and (char? c2) (char-numeric? c2))
      (write-char (read-char port) out) ; consume '.'
      (let loop ()
        (define c3 (peek-char port))
        (when (and (not (eof-object? c3)) (char-numeric? c3))
          (write-char (read-char port) out)
          (loop)))))
  (token 'number (string->number (get-output-string out))))

;; Reads a double-quoted string literal. No escape-sequence support
;; (not needed for the constants used in these programs so far).
(define (read-string-lit port)
  (read-char port) ; opening quote
  (define out (open-output-string))
  (let loop ()
    (define c (read-char port))
    (cond
      [(eof-object? c) (error 'probalog-lexer "unterminated string literal")]
      [(char=? c #\") (void)]
      [else (write-char c out) (loop)]))
  (token 'string (get-output-string out)))

;; Disambiguates ':-' (rule arrow) from '::' (probability annotation).
;; A bare ':' is not valid in this grammar.
(define (read-colon-token port)
  (read-char port) ; consume ':'
  (define c (peek-char port))
  (cond
    [(and (char? c) (char=? c #\-)) (read-char port) (token 'colon-dash #f)]
    [(and (char? c) (char=? c #\:)) (read-char port) (token 'coloncolon #f)]
    [else (error 'probalog-lexer "expected ':-' or '::' after ':'")]))
