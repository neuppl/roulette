#lang racket/base
(provide get-info)

;; Runtime configuration for modules written in the language, reached
;; through the reader's #:language-info. It exists to install the
;; interactive reader; see configure-runtime.rkt.
(define (get-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#(roulette/example/probalog/lang/configure-runtime configure #f))]
      [else default])))
