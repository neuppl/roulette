#lang s-exp syntax/module-reader
roulette/example/probalog/expander
#:read probalog-read
#:read-syntax probalog-read-syntax
#:whole-body-readers? #t

(require "../parser.rkt")

;; Whole-body reader for plain `read`: consumes the entire port and
;; returns the list of parsed S-expression forms directly.
(define (probalog-read port)
  (parse-probalog port))

;; Whole-body reader for `read-syntax`: same parse, but each resulting
;; form is wrapped as a syntax object (associated with the given
;; source location `src`) so it can be spliced into the module body
;; that syntax/module-reader constructs.
(define (probalog-read-syntax src port)
  (for/list ([datum (parse-probalog port)])
    (datum->syntax #f datum)))
