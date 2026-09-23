#lang racket/base

(require rackunit
         parser-tools/lex
         (only-in "../example/blog.rkt" blog-lex)
         (only-in (submod "../example/blog.rkt" reader)
                  [read-syntax blog-read-syntax])
         "blog-test-support.rkt")

(define (parse text)
  (parse-blog text))

(module+ test
  (test-case "finite BLOG example"
    (check-equal?
     (parse #<<BLOG
type Person;
distinct Person Alice, Bob;
fixed Boolean IsAlice(Person p) = p == Alice;
random Boolean Healthy(Person p) ~ BooleanDistrib(0.8);
random Boolean TestPositive(Person p) ~
  if Healthy(p) then BooleanDistrib(0.9)
  else BooleanDistrib(0.1);
random Person Selected ~ Categorical({Alice -> 0.4, Bob -> 0.6});
obs TestPositive(Selected) = true;
query Healthy(Alice);
query Selected;
BLOG
            )
     '((type Person)
       (distinct Person Alice Bob)
       (fixed Boolean IsAlice ((Person p)) (== p Alice))
       (random Boolean Healthy ((Person p)) ~ (call BooleanDistrib 0.8))
       (random Boolean TestPositive ((Person p)) ~
               (if (call Healthy p)
                   (call BooleanDistrib 0.9)
                   (call BooleanDistrib 0.1)))
       (random Person Selected () ~
               (call Categorical (map (entry Alice 0.4) (entry Bob 0.6))))
       (obs (call TestPositive Selected) #t)
       (query (call Healthy Alice))
       (query Selected))))

  (test-case "propositional precedence and implication associativity"
    (check-equal?
     (parse "query !a == b & c | d => e => f;")
     `((query (=> (,(string->symbol "|") (& (== (! a) b) c) d)
                  (=> e f)))))
    (check-equal? (parse "query !(a != b);")
                  '((query (! (!= a b)))))
    (check-equal? (parse "query if a then b else c & d;")
                  '((query (if a b (& c d)))))
    (check-equal? (parse "query (if a then b else c) & d;")
                  '((query (& (if a b c) d)))))

  (test-case "nested conditionals and multiple typed arguments"
    (check-equal?
     (parse "random T r(X x, Y y) ~ if p(x,y) then D(x,y)
             else if q(x) then E() else F;")
     '((random T r ((X x) (Y y)) ~
               (if (call p x y) (call D x y)
                   (if (call q x) (call E) F))))))

  (test-case "constants, deterministic random definitions, indexed names"
    (check-equal?
     (parse "distinct Person Alice, P[2]; fixed Boolean Yes() = true;
             random Boolean R = Yes(); obs R = false; query P[0];")
     '((distinct Person Alice (indexed P 2))
       (fixed Boolean Yes () #t)
       (random Boolean R () = (call Yes))
       (obs R #f)
       (query (index P 0)))))

  (test-case "decimal forms, exponents and signs"
    (check-equal?
     (parse "query D(0, 1., .5, 1e-2, 2E+1, -0.25);")
     '((query (call D 0 1.0 0.5 0.01 20.0 -0.25)))))

  (test-case "comments, keyword boundaries and empty input"
    (check-equal? (parse "// no declarations\n/* comment */") '())
    (check-equal? (parse "/* /* non-nesting */query trueValue; // eof")
                  '((query trueValue)))
    (check-equal? (parse "query iffy(_x2, BooleanDistrib(.5));")
                  '((query (call iffy _x2 (call BooleanDistrib 0.5))))))

  (test-case "lexer uses longest tokens and retains positions"
    (define in (open-input-string "query a!=b => true;"))
    (port-count-lines! in)
    (check-equal?
     (for/list ([i (in-range 8)])
       (token-name (position-token-token (blog-lex in))))
     '(query ident unequal ident implies truet semicolon eof)))

  (test-case "reader wraps parsed expressions in the BLOG language module"
    (define module-syntax
      (blog-read-syntax 'model.blog
                        (open-input-string "// heading\nquery !Healthy(Alice);")))
    (check-true (syntax? module-syntax))
    (check-equal? (syntax->datum module-syntax)
                  '(module _ roulette/example/blog
                     (query (! (call Healthy Alice))))))

  (test-case "malformed and unsupported syntax produces located read errors"
    (for ([text (in-list
                 '("query true" "type ;"
                   "random Boolean f(Person x Person y) ~ D();"
                   "distinct Person A[1.5];" "query Categorical({A 0.5});"
                   "query a && b;" "query true false;"
                   "origin Person Parent(Person);"
                   "query #Person;" "/* unterminated"))])
      (check-exn
       (lambda (exn)
         (and (exn:fail:read? exn)
              (pair? (exn:fail:read-srclocs exn))
              (equal? (srcloc-source (car (exn:fail:read-srclocs exn)))
                      'example.blog)))
       (lambda () (parse text))
       text)))

  (test-case "error location after a multiline comment"
    (check-exn
     (lambda (exn)
       (and (exn:fail:read? exn)
            (let ([loc (car (exn:fail:read-srclocs exn))])
              (and (= (srcloc-line loc) 3) (= (srcloc-column loc) 6)))))
     (lambda () (parse "/* one\n two */\nquery @;")))))
