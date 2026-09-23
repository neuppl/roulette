#lang racket/base

;; Type and object identity tests exercise the runtime macros directly.
(require rackunit
         (only-in "../example/blog.rkt" type distinct))

(module+ test
  (test-case "type declarations bind distinct descriptors"
    (type Person)
    (type Ball)
    (check-not-false Person)
    (check-eq? Person Person)
    (check-false (eq? Person Ball)))

  (test-case "separate declarations with the same name have distinct identities"
    (define (make-person-type)
      (type Person)
      Person)
    (check-false (eq? (make-person-type) (make-person-type))))

  (test-case "distinct binds separate objects with stable identities"
    (type Person)
    (distinct Person Alice Bob Carol)
    (define alice-again Alice)
    (check-eq? Alice alice-again)
    (check-false (equal? Alice Bob))
    (check-false (equal? Alice Carol))
    (check-false (equal? Bob Carol))
    (check-false (eq? Alice Person)))

  (test-case "multiple distinct declarations can populate one type"
    (type Person)
    (distinct Person Alice)
    (distinct Person Bob Carol)
    (check-false (equal? Alice Bob))
    (check-false (equal? Alice Carol)))

  (test-case "objects from separate types have distinct identities"
    (type Person)
    (type Ball)
    (distinct Person Alice)
    (distinct Ball Red)
    (check-false (equal? Alice Red)))

  (test-case "distinct requires a type descriptor"
    (check-exn
     exn:fail:contract?
     (lambda ()
       (define NotType 'Person)
       (distinct NotType Alice)
       Alice))))
