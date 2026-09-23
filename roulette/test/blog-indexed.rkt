#lang racket/base

(require rackunit "blog-test-support.rkt")

(define run (make-blog-runner 'indexed.blog))

(module+ test
  (test-case "mixed declarations and stable zero-based identities"
    (check-equal?
     (run "type Person; distinct Person Alice, P[3], Bob, Q[1];
           query P[0] == P[0] & P[0] != P[1] & P[2] != Alice
                 & P[2] != Bob & P[0] != Q[0];")
     (hash #t 1)))
  (test-case "indexed objects populate the quantified domain"
    (check-equal?
     (run "type Person; distinct Person P[3];
           query (forall Person p (p == P[0] | p == P[1] | p == P[2]))
                 & (exists Person p p == P[2]);")
     (hash #t 1)))
  (test-case "indexed objects ground independent reusable random functions"
    (check-equal?
     (run "type Person; distinct Person P[2];
           random Integer X(Person p) ~ UniformInt(0, 1);
           query X(P[0]) == X(P[1]);")
     (hash #t 1/2 #f 1/2))
    (check-equal?
     (run "type Person; distinct Person P[2];
           random Integer X(Person p) ~ UniformInt(0, 1);
           random Integer I ~ UniformInt(0, 1);
           query X(P[I]) == X(P[I]);")
     (hash #t 1)))
  (test-case "random indices preserve correlation and support arithmetic"
    (check-equal?
     (run "type Person; distinct Person P[3];
           random Integer I ~ UniformInt(0, 2); query P[I] == P[1];")
     (hash #t 1/3 #f 2/3))
    (check-equal?
     (run "type Person; distinct Person P[3];
           random Integer I ~ UniformInt(0, 1);
           query P[I] != P[I + 1];")
     (hash #t 1)))
  (test-case "empty families contribute no objects"
    (check-equal?
     (run "type Person; distinct Person P[0]; query exists Person p true;")
     (hash #f 1)))
  (test-case "invalid indices are rejected"
    (for ([expression '("P[-1]" "P[2]" "P[0.5]" "P[true]" "Alice[0]")])
      (check-exn exn:fail:contract?
                 (lambda ()
                   (run (string-append "type Person; distinct Person Alice, P[2]; query "
                                       expression ";")))))
    (check-exn exn:fail:contract?
               (lambda () (run "type Person; distinct Person P[0]; query P[0];")))
    (check-exn exn:fail:contract?
               (lambda ()
                 (run "type Person; distinct Person P[2];
                       random Integer I ~ UniformInt(0, 2); query P[I];"))))
  (test-case "negative sizes are rejected by the parser"
    (check-exn exn:fail:read?
               (lambda () (run "type Person; distinct Person P[-1]; query true;")))))
