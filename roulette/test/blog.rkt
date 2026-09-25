#lang at-exp racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in (submod roulette/example/blog reader) read-syntax)
         (only-in "../example/disrupt/core.rkt" query pmf-hash)
         racket/string
         rackunit
         syntax/parse
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-namespace-anchor here)
(define anchored-ns (namespace-anchor->namespace here))
(define ϵ 0.000001)

(define (check-program t . lines)
  (define prog (string-join lines))
  (define stx (read-syntax 'anonymous (open-input-string prog)))
  (define mod
    (syntax-parse stx
      [(_ name lang form ... final)
       #'(module anonymous lang
           (provide result)
           form ...
           (define result final))]))
  (define ns (make-base-namespace))
  (namespace-attach-module anchored-ns "../example/disrupt/core.rkt" ns)
  (void
   (parameterize ([current-namespace ns])
     (eval mod)
     (define result (pmf-hash (query (dynamic-require ''anonymous 'result))))
     (with-check-info (['program prog] ['result result])
       (check-close ϵ result (hash #t t #f (- 1 t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  @check-program[0.254]{
    type Person;
    distinct Person Alice, Bob;

    fixed Boolean AlwaysTrue = true;
    fixed Boolean IsAlice(Person p) = p == Alice;

    random Real prob ~ Categorical({0.1 -> 0.5, 0.9 -> 0.5});

    random Boolean Smokes(Person p) ~ BooleanDistrib(prob);

    random Boolean Healthy(Person p) ~
      if Smokes(p) then BooleanDistrib(0.4)
      else BooleanDistrib(0.9);

    // comments

    random Boolean Infected(Person p) ~ BooleanDistrib(prob);

    random Boolean ExtraHealthy(Person p) ~
      if (exists Person q Infected(q)) then BooleanDistrib(0.2)
      else BooleanDistrib(0.8);

    obs Smokes(Alice) = true;

    query (ExtraHealthy(Alice));
  })
