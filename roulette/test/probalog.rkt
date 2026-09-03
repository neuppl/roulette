#lang at-exp racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/port
         racket/string
         rackunit
         (only-in roulette/example/probalog/parser parse-probalog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; running programs

(define ϵ 0.000001)

;; A program's text, from the pieces an @-expression body produces.
;; Joined with "" rather than " " so that line structure survives:
;; `%` comments run to end of line, and read errors report line numbers.
(define (program-text lines)
  (apply string-append "#lang roulette/example/probalog\n" lines))

;; Runs `src` as a module and returns the lines it printed.
;;
;; Each program gets a fresh namespace, so it gets its own instance of
;; the engine. That matters because `observe!` conditions Roulette's
;; global state: sharing a namespace would let one program's
;; observations change the next program's answers.
(define (run-program src)
  (parameterize ([current-namespace (make-base-namespace)]
                 [read-accept-reader #t]
                 [read-accept-lang #t])
    (define stx (read-syntax 'probalog-test (open-input-string src)))
    (define name (syntax-case stx () [(_ n . _) (syntax-e #'n)]))
    (define out (open-output-string))
    (parameterize ([current-output-port out])
      (eval stx)
      (dynamic-require `',name #f))
    (string-split (get-output-string out) "\n")))

;; The answer on one line of query output: #t or #f for a certain
;; result, which probalog prints as itself, or the probability that
;; the fact holds when the result is a distribution.
(define (parse-answer line)
  (cond
    [(regexp-match #px": #t$" line) #t]
    [(regexp-match #px": #f$" line) #f]
    [(regexp-match #px"#<pmf: \\[#t ([0-9.e+-]+)\\]" line)
     => (λ (m) (string->number (cadr m)))]
    [else (error 'parse-answer "unrecognized query output: ~a" line)]))

;; Checks the answers to a program's queries, in source order. Each
;; expected value is a probability, or #t/#f for a certain answer --
;; and the two are not interchangeable, since printing a certain
;; result as itself rather than as a one-outcome distribution is part
;; of what's being tested.
(define (check-queries expected . lines)
  (define src (program-text lines))
  (define answers (map parse-answer (run-program src)))
  (with-check-info (['program src] ['expected expected] ['actual answers])
    (check-equal? (length answers) (length expected) "wrong number of queries")
    (for ([e expected] [a answers] [i (in-naturals)])
      (with-check-info (['query i])
        (if (boolean? e)
            (check-equal? a e)
            (check-true (and (real? a) (< (abs (- e a)) ϵ))
                        (format "expected ~a, got ~a" e a)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; errors

;; Checks that a program is rejected while being read, with a message
;; matching `rx`. Goes straight to the parser: read errors need no
;; namespace, and this keeps the check on the message itself.
(define (check-read-error rx . lines)
  ;; No `#lang` line here: the reader consumes that before the parser
  ;; ever sees the port, so `parse-probalog` is given the statements
  ;; alone -- which also means line 1 is the first statement.
  (define src (apply string-append lines))
  (define msg
    (with-handlers ([exn:fail? exn-message])
      (parse-probalog (open-input-string src) "test.pdl")
      #f))
  (with-check-info (['program src] ['pattern rx] ['message (or msg "none")])
    (check-true (and msg (regexp-match? rx msg) #t)
                (if msg "wrong message" "the program parsed"))))

;; Checks that a program reads and saturates fine but fails when its
;; statements run -- which for probalog means an observation of
;; something with probability 0.
(define (check-run-error rx . lines)
  (define src (program-text lines))
  (define msg
    (with-handlers ([exn:fail? exn-message])
      (run-program src)
      #f))
  (with-check-info (['program src] ['pattern rx] ['message (or msg "none")])
    (check-true (and msg (regexp-match? rx msg) #t)
                (if msg "wrong message" "the program ran"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; facts

(module+ test
  ;; a probability annotation, and the two ways to write certainty
  @check-queries['(0.5 #t #t #f)]{
    Coin("c") :: 0.5.
    Sure("a").
    Sure("b") :: 1.
    Never("n") :: 0.
    ? Coin("c").
    ? Sure("a").
    ? Sure("b").
    ? Never("n").
  }

  ;; nullary and higher-arity predicates
  @check-queries['(0.75 0.9)]{
    Ready() :: 0.75.
    Reading("s", 3, -12.5, "celsius") :: 0.9.
    ? Ready().
    ? Reading("s", 3, -12.5, "celsius").
  }

  ;; constants are compared with equal?, so 1 and 1.0 differ
  @check-queries['(0.6 #f)]{
    Point(1, 2) :: 0.6.
    ? Point(1, 2).
    ? Point(1.0, 2.0).
  }

  ;; a fact no statement declares, and a predicate only ever used
  @check-queries['(#f #f)]{
    Known("a") :: 0.5.
    Derived(x) :- NeverDeclared(x).
    ? Known("b").
    ? Derived("a").
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

(module+ test
  ;; a join, a projected body variable, and a constant in the head.
  ;; Any() ranges over both A facts, so it is 1 - 0.6*0.6.
  @check-queries['(0.12 0.64 0.4)]{
    A("x") :: 0.4.
    A("y") :: 0.4.
    B("x") :: 0.3.
    Both(v) :- A(v), B(v).
    Any() :- A(v).
    Named() :- A("x").
    ? Both("x").
    ? Any().
    ? Named().
  }

  ;; a clause matching only self-loops, via a repeated variable
  @check-queries['(0.7 #f)]{
    Link("a", "a") :: 0.7.
    Link("b", "c") :: 0.7.
    SelfLooping(x) :- Link(x, x).
    ? SelfLooping("a").
    ? SelfLooping("b").
  }

  ;; constants in clause position, which the value index narrows on
  @check-queries['(0.9 #f 0.81)]{
    Link(0, 1) :: 0.9.
    Link(1, 2) :: 0.9.
    FromZero(y) :- Link(0, y).
    Chain() :- Link(0, 1), Link(1, 2).
    ? FromZero(1).
    ? FromZero(2).
    ? Chain().
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exact inference
;;
;; The heart of it: a derived fact's guard is a formula over the base
;; facts' random variables, so derivations that share base facts come
;; out right. Each of these has a different answer from what a
;; rule-at-a-time engine assuming independence would give.

(module+ test
  ;; two routes over disjoint edges: independent, so 1 - (1 - 0.25)^2
  @check-queries['(0.4375)]{
    Edge("s", "a") :: 0.5.
    Edge("a", "t") :: 0.5.
    Edge("s", "b") :: 0.5.
    Edge("b", "t") :: 0.5.
    Path(x, y) :- Edge(x, y).
    Path(x, z) :- Path(x, y), Edge(y, z).
    ? Path("s", "t").
  }

  ;; two routes that both go through one uncertain edge: 0.5, where
  ;; assuming independence would give 1 - 0.5^2 = 0.75
  @check-queries['(0.5)]{
    Edge("s", "m") :: 0.5.
    Edge("m", "a").
    Edge("a", "t").
    Edge("m", "b").
    Edge("b", "t").
    Path(x, y) :- Edge(x, y).
    Path(x, z) :- Path(x, y), Edge(y, z).
    ? Path("s", "t").
  }

  ;; partial sharing: a common bridge, then a private edge each
  @check-queries['(0.672)]{
    Edge("s",  "b1").
    Edge("b1", "b2") :: 0.8.
    Edge("b2", "l")  :: 0.6.
    Edge("b2", "r")  :: 0.6.
    Edge("l",  "t").
    Edge("r",  "t").
    Path(x, y) :- Edge(x, y).
    Path(x, z) :- Path(x, y), Edge(y, z).
    ? Path("s", "t").
  }

  ;; a repeated clause is idempotent, not squared
  @check-queries['(0.4)]{
    Risk("h") :: 0.4.
    Double(x) :- Risk(x), Risk(x).
    ? Double("h").
  }

  ;; two declarations of one fact are two independent flips, unioned
  @check-queries['(0.75)]{
    Twice("x") :: 0.5.
    Twice("x") :: 0.5.
    ? Twice("x").
  }

  ;; likewise a rule that re-derives a base fact
  @check-queries['(0.75)]{
    Direct("y")   :: 0.5.
    Indirect("y") :: 0.5.
    Direct(x) :- Indirect(x).
    ? Direct("y").
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursion
;;
;; Cycles are what make evaluation a fixpoint computation, and what
;; makes the probabilistic version need a solver: going round adds
;; syntactically new disjuncts to a guard long after it stops meaning
;; anything new, so termination depends on checking equivalence rather
;; than syntactic equality.

(module+ test
  ;; transitive closure over a chain
  @check-queries['(0.9 0.81 0.729)]{
    Edge(0, 1) :: 0.9.
    Edge(1, 2) :: 0.9.
    Edge(2, 3) :: 0.9.
    Reach(x, y) :- Edge(x, y).
    Reach(x, z) :- Reach(x, y), Edge(y, z).
    ? Reach(0, 1).
    ? Reach(0, 2).
    ? Reach(0, 3).
  }

  ;; a directed cycle: every node reaches every other, including
  ;; itself, and nothing reaches the node outside it
  @check-queries['(0.6561 0.729 0.729 #f)]{
    Edge(0, 1) :: 0.9.
    Edge(1, 2) :: 0.9.
    Edge(2, 3) :: 0.9.
    Edge(3, 0) :: 0.9.
    Reach(x, y) :- Edge(x, y).
    Reach(x, z) :- Reach(x, y), Edge(y, z).
    ? Reach(0, 0).
    ? Reach(0, 3).
    ? Reach(3, 2).
    ? Reach(4, 0).
  }

  ;; an isolated self-loop, whose guard would keep growing
  ;; syntactically forever without equivalence checking
  @check-queries['(0.7)]{
    Edge("a", "a") :: 0.7.
    Reach(x, y) :- Edge(x, y).
    Reach(x, z) :- Reach(x, y), Edge(y, z).
    ? Reach("a", "a").
  }

  ;; non-linear recursion: both body clauses recursive, so semi-naive
  ;; evaluation has to try each position as the delta one
  @check-queries['(0.6561 0.729 0.729)]{
    Hop(0, 1) :: 0.9.
    Hop(1, 2) :: 0.9.
    Hop(2, 3) :: 0.9.
    Hop(3, 0) :: 0.9.
    Trans(x, y) :- Hop(x, y).
    Trans(x, z) :- Trans(x, y), Trans(y, z).
    ? Trans(0, 0).
    ? Trans(0, 3).
    ? Trans(1, 0).
  }

  ;; mutual recursion: neither predicate is complete until both are
  @check-queries['(#t #t #t 0.6 0.6 0.3 0.3 #f #f)]{
    Step(0, 1).
    Step(1, 2).
    Step(2, 3) :: 0.6.
    Step(3, 4).
    Step(4, 5) :: 0.5.
    Step(5, 6).
    Even(0).
    Odd(y)  :- Even(x), Step(x, y).
    Even(y) :- Odd(x),  Step(x, y).
    ? Even(0).
    ? Odd(1).
    ? Even(2).
    ? Odd(3).
    ? Even(4).
    ? Odd(5).
    ? Even(6).
    ? Even(1).
    ? Odd(2).
  }

  ;; a rule whose body joins three predicates, recursively, around a
  ;; cyclic relation -- the friends-and-smokers shape. Smokes("a") is
  ;; Stress(a) or (Smokes(b) and Influences(b,a)); going round again
  ;; adds only disjuncts already implied, which is what the fixpoint
  ;; check has to see. So 1 - 0.8*(1 - 0.2*0.3) = 0.248.
  @check-queries['(0.248 0.248)]{
    Friend("a", "b").
    Friend("b", "a").
    Stress("a") :: 0.2.
    Stress("b") :: 0.2.
    Influences("a", "b") :: 0.3.
    Influences("b", "a") :: 0.3.
    Smokes(x) :- Stress(x).
    Smokes(x) :- Friend(x, y), Smokes(y), Influences(y, x).
    ? Smokes("a").
    ? Smokes("b").
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; observations

(module+ test
  ;; Queries before the first observation report priors, queries after
  ;; report posteriors. Path("a","c") needs both edges, so observing
  ;; it makes each of them certain too.
  @check-queries['(0.3 #t #t)]{
    Edge("a", "b") :: 0.5.
    Edge("b", "c") :: 0.6.
    Path(x, y) :- Edge(x, y).
    Path(x, z) :- Path(x, y), Edge(y, z).
    ? Path("a", "c").
    ! Path("a", "c").
    ? Path("a", "c").
    ? Edge("a", "b").
  }

  ;; a positive observation propagates back to a cause: P(a) rises
  ;; from 0.4 to 0.4/(1 - 0.6*0.6)
  @check-queries['(0.625 0.625)]{
    A("x") :: 0.4.
    B("x") :: 0.4.
    Either(v) :- A(v).
    Either(v) :- B(v).
    ! Either("x").
    ? A("x").
    ? B("x").
  }

  ;; a negative observation rules its causes out entirely
  @check-queries['(#f #f)]{
    A("x") :: 0.4.
    B("x") :: 0.4.
    Either(v) :- A(v).
    Either(v) :- B(v).
    ! ~Either("x").
    ? A("x").
    ? B("x").
  }

  ;; observations accumulate, and later ones condition on earlier ones
  @check-queries['(0.625 #f #t)]{
    A("x") :: 0.4.
    B("x") :: 0.4.
    Either(v) :- A(v).
    Either(v) :- B(v).
    ! Either("x").
    ? A("x").
    ! ~B("x").
    ? B("x").
    ? A("x").
  }

  ;; Explaining away: hearing the alarm raises P(burglary) from 0.01
  ;; to 0.62; then learning there was an earthquake accounts for the
  ;; alarm on its own and pushes it back down to 0.03.
  @check-queries['(0.01 0.6233494480121221 0.03250265379366122)]{
    Burglary()   :: 0.01.
    Earthquake() :: 0.02.
    AlarmB() :: 0.95.
    AlarmE() :: 0.29.
    Alarm() :- Burglary(),   AlarmB().
    Alarm() :- Earthquake(), AlarmE().
    ? Burglary().
    ! Alarm().
    ? Burglary().
    ! Earthquake().
    ? Burglary().
  }

  ;; observing something already certain is allowed and changes nothing
  @check-queries['(#t #f 0.5)]{
    Sure("a").
    Never("n") :: 0.
    Coin("c") :: 0.5.
    ! Sure("a").
    ! ~Never("n").
    ? Sure("a").
    ? Never("n").
    ? Coin("c").
  }

  ;; conditioning respects shared evidence: observing the bridge
  ;; leaves the two private edges, which are still independent
  @check-queries['(0.84 0.6)]{
    Edge("s",  "b1").
    Edge("b1", "b2") :: 0.8.
    Edge("b2", "l")  :: 0.6.
    Edge("b2", "r")  :: 0.6.
    Edge("l",  "t").
    Edge("r",  "t").
    Path(x, y) :- Edge(x, y).
    Path(x, z) :- Path(x, y), Edge(y, z).
    ! Edge("b1", "b2").
    ? Path("s", "t").
    ? Edge("b2", "l").
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; layout
;;
;; Whitespace is insignificant, statements end at their period, and
;; `%` comments run to end of line.

(module+ test
  @check-queries['(0.216 0.5 0.5)]{
    % a comment on its own line
    Edge(1, 2) :: 0.6.  % and one after a statement
    Edge(2, 3) :: 0.6.
    Edge(3, 4) :: 0.6.
    Chain(x, w) :-
      Edge(x, y),
      Edge(y, z),
      Edge(z, w).
    Tiny("g") :: 0.5. Tiny("h") :: 0.5.
    ? Chain(1, 4).
    ? Tiny("g").
    ? Tiny("h").
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read errors

(module+ test
  ;; lexical. `@"@"` is how the @-expression reader escapes a literal
  ;; @, which is otherwise its own dispatch character.
  @check-read-error[#rx"unexpected character: @"]{
    Edge("a", "b") @"@" 0.5.
  }

  @check-read-error[#rx"expected ':-' or '::' after ':'"]{
    Edge("a", "b") : 0.5.
  }

  ;; a string literal stops at end of line, so the error is blamed on
  ;; the line that opened it rather than on some later quote
  @check-read-error[#rx"unterminated string literal"]{
    Edge("a, "b").
    Other("c").
  }

  ;; structure
  @check-read-error[#rx"expected '::', ':-', or '\\.' after Edge\\(\\.\\.\\.\\), got end of file"]{
    Edge("a", "b")
  }

  @check-read-error[#rx"expected '::', ':-', or '\\.' after Edge"]{
    Edge("a", "b") 0.5.
  }

  ;; reported as the missing separator, not as a missing close paren
  @check-read-error[#rx"arguments are separated by commas"]{
    Edge("a" "b").
  }

  @check-read-error[#rx"expected '\\)'"]{
    Edge("a", "b".
  }

  @check-read-error[#rx"expected an argument"]{
    Edge("a", ).
  }

  ;; naming: the capitalization convention is enforced, since it is
  ;; what distinguishes a predicate from a variable
  @check-read-error[#rx"predicate names must start with an uppercase letter"]{
    edge("a", "b").
  }

  @check-read-error[#rx"did you mean to quote it as a string constant"]{
    Reaches(x) :- Edge(x, Target).
  }

  ;; probabilities
  @check-read-error[#rx"probability must be between 0 and 1, got 1.5"]{
    Edge("a", "b") :: 1.5.
  }

  @check-read-error[#rx"probability must be between 0 and 1, got -0.5"]{
    Edge("a", "b") :: -0.5.
  }

  @check-read-error[#rx"probabilities need a leading zero"]{
    Edge("a", "b") :: .5.
  }

  @check-read-error[#rx"expected a number"]{
    Edge("a", "b") :: .
  }

  ;; well-formedness: without the arity check, a clause with the wrong
  ;; number of arguments would just never unify, with no diagnostic
  @check-read-error[#rx"'Edge' is used with 1 argument here, but with 2 on line 1"]{
    Edge("a", "b").
    Reaches(x) :- Edge(x).
  }

  ;; facts, queries and observations must be ground
  @check-read-error[#rx"fact 'Edge' contains the variable x"]{
    Edge(x, "b") :: 0.5.
  }

  @check-read-error[#rx"query 'Edge' contains the variable x"]{
    Edge("a", "b") :: 0.5.
    ? Edge(x, "b").
  }

  @check-read-error[#rx"observation 'Edge' contains the variable x"]{
    Edge("a", "b") :: 0.5.
    ! Edge(x, "b").
  }

  ;; range restriction: y has nothing to bind against, so the rule
  ;; would derive one fact per value y might take
  @check-read-error[#rx"variable 'y' in the head of rule 'Reaches' does not appear in its body"]{
    Reaches(x, y) :- Edge(x, "b").
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-time errors
;;
;; Conditioning on an impossible event divides by zero, after which
;; every later query would report nothing rather than failing. So an
;; observation is checked before it is made, and blamed on the
;; statement responsible.

(module+ test
  @check-run-error[#rx"cannot observe Never\\(\"x\"\\): it has probability 0"]{
    Never("x") :: 0.
    ! Never("x").
  }

  ;; a fact that was never derived: guard #f, the same impossibility
  @check-run-error[#rx"cannot observe Path\\(\"b\", \"a\"\\): it has probability 0"]{
    Edge("a", "b") :: 0.5.
    Path(x, y) :- Edge(x, y).
    ! Path("b", "a").
  }

  ;; contradictory observations: caught at the second one, which was
  ;; satisfiable on its own but not once the first had been applied
  @check-run-error[#rx"cannot observe the absence of Path\\(\"a\", \"b\"\\)"]{
    Edge("a", "b") :: 0.5.
    Path(x, y) :- Edge(x, y).
    ! Path("a", "b").
    ! ~Path("a", "b").
  }

  ;; a fact with no annotation holds in every world, so no world is
  ;; left in which it is missing
  @check-run-error[#rx"cannot observe the absence of Edge\\(\"a\", \"b\"\\)"]{
    Edge("a", "b").
    ! ~Edge("a", "b").
  }

  ;; the error names the statement it came from
  @check-run-error[#rx"probalog-test:3:0"]{
    Never("x") :: 0.
    ! Never("x").
  })
