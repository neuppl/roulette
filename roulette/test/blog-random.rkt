#lang roulette

(require rackunit
         (only-in "../example/disrupt/core.rkt" query pmf-hash)
         (prefix-in blog: "../example/blog.rkt"))

(module+ test
  (test-case "random constant and zero-argument call share one value"
    (check-equal?
     (pmf-hash
      (query
       (blog:random Boolean X () ~ (blog:BooleanDistrib 1/4))
       (list X (blog:call X))))
     (hash '(#t #t) 1/4 '(#f #f) 3/4)))

  (test-case "each object has an independent choice, reused on every call"
    (check-equal?
     (pmf-hash
      (query
       (blog:type Person)
       (blog:distinct Person Alice Bob)
       (blog:random Boolean Healthy ((Person p)) ~ (blog:BooleanDistrib 1/2))
       (list (Healthy Alice) (Healthy Alice) (Healthy Bob))))
     (hash '(#t #t #t) 1/4 '(#t #t #f) 1/4
           '(#f #f #t) 1/4 '(#f #f #f) 1/4)))

  (test-case "conditional distributions preserve parent-child correlation"
    (check-equal?
     (pmf-hash
      (query
       (blog:type Person)
       (blog:distinct Person Alice)
       (blog:random Boolean Smokes ((Person p)) ~ (blog:BooleanDistrib 1/5))
       (blog:random Boolean Healthy ((Person p)) ~
                    (if (Smokes p)
                        (blog:BooleanDistrib 2/5)
                        (blog:BooleanDistrib 9/10)))
       (list (Smokes Alice) (Healthy Alice))))
     (hash '(#t #t) 2/25 '(#t #f) 3/25
           '(#f #t) 18/25 '(#f #f) 2/25)))

  (test-case "multi-argument grounding and deterministic random dependency"
    (check-equal?
     (pmf-hash
      (query
       (blog:type Person)
       (blog:distinct Person Alice Bob)
       (blog:random Boolean R ((Person p) (Person q)) ~
                    (blog:BooleanDistrib 1/2))
       (blog:random Boolean Opposite ((Person p) (Person q)) = (not (R p q)))
       (list (equal? (R Alice Bob) (R Bob Alice))
             (equal? (Opposite Alice Bob) (not (R Alice Bob))))))
     (hash '(#t #t) 1/2 '(#f #t) 1/2)))

  (test-case "uncertain object arguments select existing values"
    (check-equal?
     (pmf-hash
      (query
       (blog:type Person)
       (blog:distinct Person Alice Bob)
       (blog:random Boolean Healthy ((Person p)) ~
                    (blog:BooleanDistrib (if (equal? p Alice) 1/4 3/4)))
       (blog:random Person Selected () ~
                    (blog:Categorical
                     (blog:map (blog:entry Alice 1/2) (blog:entry Bob 1/2))))
       (list (Healthy Selected)
             (equal? (Healthy Selected) (Healthy Selected)))))
     (hash '(#t #t) 1/2 '(#f #t) 1/2)))

  (test-case "conditional categorical distribution and zero weights"
    (check-equal?
     (pmf-hash
      (query
       (blog:random Boolean Parent () ~ (blog:BooleanDistrib 1/2))
       (blog:random Boolean Child () ~
                    (if Parent
                        (blog:Categorical
                         (blog:map (blog:entry #t 1) (blog:entry #f 0)))
                        (blog:Categorical
                         (blog:map (blog:entry #t 1/4) (blog:entry #f 3/4)))))
       (list Parent Child)))
     (hash '(#t #t) 1/2 '(#f #t) 1/8 '(#f #f) 3/8)))

  (test-case "categorical probabilities can depend on a random value"
    (check-equal?
     (pmf-hash
      (query
       (blog:random Boolean Parent () ~ (blog:BooleanDistrib 1/2))
       (blog:random Boolean Child () ~
                    (blog:Categorical
                     (blog:map (blog:entry #t (if Parent 0 3/4))
                               (blog:entry #f (if Parent 1 1/4)))))
       (list Parent Child)))
     (hash '(#t #f) 1/2 '(#f #t) 3/8 '(#f #f) 1/8)))

  (test-case "nested conditional distributions"
    (check-equal?
     (pmf-hash
      (query
       (blog:random Boolean A () ~ (blog:BooleanDistrib 1/2))
       (blog:random Boolean B () ~ (blog:BooleanDistrib 1/2))
       (blog:random Boolean C () ~
                    (if (and A B) (blog:BooleanDistrib 1)
                        (if A (blog:BooleanDistrib 1/2)
                            (blog:BooleanDistrib 0))))
       C))
     (hash #t 3/8 #f 5/8)))

  (test-case "invalid distribution parameters and bodies are rejected"
    (check-exn exn:fail:contract? (lambda () (blog:BooleanDistrib -1)))
    (check-exn exn:fail:contract?
               (lambda ()
                 (blog:Categorical (blog:map (blog:entry #t 1/2)))))
    (check-exn exn:fail:contract?
               (lambda () (blog:random Boolean X () ~ #t) X)))

  (test-case "wrong argument count and unregistered objects are rejected"
    (blog:type Person)
    (blog:distinct Person Alice)
    (blog:random Boolean Healthy ((Person p)) ~ (blog:BooleanDistrib 1))
    (check-exn exn:fail:contract:arity? (lambda () (Healthy)))
    (check-exn exn:fail:contract? (lambda () (Healthy 'Alice)))
    (blog:distinct Person Bob)
    (check-exn exn:fail:contract? (lambda () (Healthy Bob)))))
