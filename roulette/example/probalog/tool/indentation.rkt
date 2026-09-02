#lang racket/base
(require racket/class racket/list "../lexer.rkt")
(provide indent-probalog)

;; Line indentation for DrRacket, supplied through the
;; `drracket:indentation` info key. Statements are delimited by
;; periods rather than by parentheses, so the s-expression default
;; has nothing useful to say about a Probalog program.
;;
;; The rule: a line that starts a new statement sits at the left
;; margin, and a line continuing one lines up under the statement's
;; first body clause, so a rule broken across lines reads as
;;
;;   Path(x, z) :- Path(x, y),
;;                 Edge(y, z).
;;
;; A statement with no `:-` yet is indented one step in from wherever
;; the statement started.
(define continuation-indent 2)

(define (indent-probalog txt pos)
  (define para (send txt position-paragraph pos))
  (define line-start (send txt paragraph-start-position para))
  (cond
    [(zero? line-start) 0]
    [else
     ;; Everything before this line determines what, if anything, is
     ;; still open; the line being indented is deliberately excluded.
     (define stmt (statement-in-progress (send txt get-text 0 line-start)))
     (cond
       [(null? stmt) 0]
       [else
        (define arrow (memf (lambda (t) (eq? (token-type t) 'colon-dash)) stmt))
        (cond
          [(and arrow (pair? (cdr arrow)))
           (column-of txt (cadr arrow))]
          [arrow
           (+ (column-of txt (car arrow)) 3)]
          [else
           (+ (column-of txt (car stmt)) continuation-indent)])])]))

;; The tokens of the statement the given text ends in the middle of:
;; everything after the last statement terminator. Empty when the text
;; ends at a statement boundary, meaning the next line starts fresh.
(define (statement-in-progress text)
  (define in (open-input-string text))
  (port-count-lines! in)
  (define toks
    (let loop ([acc '()])
      (define tok (next-token in))
      (case (token-type tok)
        [(eof) (reverse acc)]
        [(white-space comment error) (loop acc)]
        [else (loop (cons tok acc))])))
  (takef-right toks (lambda (t) (not (eq? (token-type t) 'period)))))

;; The column a token starts at. Lexer positions are 1-based offsets
;; into the text scanned, which started at the beginning of the
;; buffer, so subtracting one recovers the buffer offset.
(define (column-of txt tok)
  (define offset (sub1 (srcloc-position (token-srcloc tok))))
  (define para (send txt position-paragraph offset))
  (- offset (send txt paragraph-start-position para)))

(module+ test
  (require rackunit)

  ;; The methods of color-textoid<%> that indent-probalog uses, over a
  ;; fixed string.
  (define stub-text%
    (class object%
      (init-field text)
      (super-new)
      (define/public (get-text start end) (substring text start end))
      (define/public (last-position) (string-length text))
      (define/public (position-paragraph pos)
        (for/sum ([c (in-string text 0 pos)]) (if (char=? c #\newline) 1 0)))
      (define/public (paragraph-start-position para)
        (let loop ([i 0] [seen 0])
          (cond
            [(= seen para) i]
            [(= i (string-length text)) i]
            [(char=? (string-ref text i) #\newline) (loop (add1 i) (add1 seen))]
            [else (loop (add1 i) seen)])))))

  ;; The indentation computed for the first character of each line.
  (define (indents-of str)
    (define txt (new stub-text% [text str]))
    (define starts
      (cons 0 (for/list ([c (in-string str)] [i (in-naturals)]
                         #:when (char=? c #\newline)
                         #:unless (= (add1 i) (string-length str)))
                (add1 i))))
    (for/list ([s starts]) (indent-probalog txt s)))

  ;; The first line of a file is always a statement start.
  (check-equal? (indents-of "Edge(\"a\", \"b\").") '(0))

  ;; A line after a completed statement starts fresh at the margin.
  (check-equal? (indents-of "Edge(\"a\", \"b\").\nPath(x, y) :- Edge(x, y).")
                '(0 0))

  ;; A rule broken after the comma lines up under its first clause:
  ;; column 14, just past ":- ".
  (check-equal? (indents-of "Path(x, z) :- Path(x, y),\nEdge(y, z).")
                '(0 14))

  ;; ... and the line after the rule's terminating period is a
  ;; statement start again.
  (check-equal? (indents-of "Path(x, z) :- Path(x, y),\nEdge(y, z).\nQ(1).")
                '(0 14 0))

  ;; With nothing after ":-" yet, indent one past the arrow.
  (check-equal? (indents-of "Path(x, z) :-\nPath(x, y).")
                '(0 14))

  ;; A statement with no ":-" indents one step in from its start.
  (check-equal? (indents-of "Edge(\"a\",\n\"b\") :: 0.5.")
                '(0 2))

  ;; An indented statement's continuation follows it in.
  (check-equal? (indents-of "  Edge(\"a\",\n\"b\") :: 0.5.")
                '(0 4))

  ;; Periods inside comments, strings, and probabilities don't end a
  ;; statement, so the following lines are still continuations.
  (check-equal? (indents-of "Path(x, z) :- % first.\nPath(x, y).")
                '(0 14))
  (check-equal? (indents-of "Edge(\"a.b\",\n\"c\") :: 0.5.")
                '(0 2))
  (check-equal? (indents-of "Edge(\"a\", \"b\") :: 0.5\n.")
                '(0 2)))
