#lang racket/base

;;
;; Adapted `syntax/wrap-modbeg` for `#%top-interaction`
;;

(require (for-syntax racket/base)
         rosette/base/form/module)

(provide (for-syntax make-wrapping-top-interaction
                     make-wrapping-module-begin))

(begin-for-syntax
  (define (((make-wrapping-constructor top-form top?) wrapper) stx)
    (if (symbol? (syntax-e stx))
        (raise-syntax-error
         #f
         "bad syntax"
         stx)
        (void))
    (let-values ([(l) (syntax->list stx)])
      (if (or l top?)
          (void)
          (raise-syntax-error
           #f
           "bad syntax (illegal use of `.')"
           stx))
      (datum->syntax
       stx
       (if top?
           (cons top-form (list (quote-syntax do-wrapping-module-begin) wrapper (cdr (syntax-e stx))))
           (cons top-form
                 (map (lambda (e)
                        (list (quote-syntax do-wrapping-module-begin)
                              wrapper
                              e))
                      (cdr l))))
       stx
       stx)))

  ;; Special interposition points from Rosette that handles mutation correctly
  (define make-wrapping-top-interaction
    (make-wrapping-constructor #'@#%top-interaction #t))
  (define make-wrapping-module-begin
    (make-wrapping-constructor #'@#%module-begin #f))
  )

(define-syntaxes (do-wrapping-module-begin)
  (lambda (stx)
    (let-values ([(r) (cdr (syntax-e stx))])
      (let-values ([(r) (if (syntax? r)
                            (syntax-e r)
                            r)])
        (let-values ([(wrapper) (car r)]
                     [(r) (cdr r)])
          (let-values ([(r) (if (syntax? r)
                                (syntax-e r)
                                r)])
            (if (null? r)
                (quote-syntax (void))
                (let-values ([(e) (local-expand (car r)
                                                'module
                                                (syntax->list
                                                 (quote-syntax
                                                  (quote
                                                   quote-syntax #%top
                                                   lambda case-lambda
                                                   let-values letrec-values
                                                   begin begin0 set!
                                                   with-continuation-mark
                                                   if #%app #%expression
                                                   define-values define-syntaxes begin-for-syntax
                                                   module module*
                                                   #%module-begin
                                                   #%require #%provide #%declare
                                                   #%variable-reference))))])
                  ;; `begin' is special...
                  (if (let-values ([(p) (syntax-e e)])
                        (if (pair? p)
                            (if (symbol? (syntax-e (car p)))
                                (if (free-identifier=? (car p) (quote-syntax begin))
                                    (syntax->list e)
                                    #f)
                                #f)
                            #f))
                      ;; splice `begin'
                      (let-values ([(l) (syntax->list e)])
                        (datum->syntax
                         stx
                         (cons (car l)
                               (append
                                (map (lambda (elem)
                                       (list
                                        (quote-syntax do-wrapping-module-begin)
                                        wrapper
                                        (syntax-track-origin elem e (car l))))
                                     (cdr l))
                                (cdr r)))
                         stx))
                      ;; no need to splice
                      (let-values ([(wrap?)
                                    (let-values ([(e) (syntax-e e)])
                                      (if (pair? e)
                                          (let-values ([(a) (car e)])
                                            (if (symbol? (syntax-e a))
                                                (if (ormap (lambda (i)
                                                             (free-identifier=? i a))
                                                           (syntax->list
                                                            (quote-syntax
                                                             (define-values define-syntaxes begin-for-syntax
                                                                            module module*
                                                                            #%module-begin
                                                                            #%require #%provide #%declare))))
                                                    #f
                                                    ;; Also check for calls to `void':
                                                    (if (free-identifier=? a (quote-syntax #%app))
                                                        (let-values ([(e) (cdr e)])
                                                          (let-values ([(e) (if (syntax? e)
                                                                                (syntax-e e)
                                                                                e)])
                                                            (if (pair? e)
                                                                (if (symbol? (syntax-e (car e)))
                                                                    (if (free-identifier=? (car e) (quote-syntax void))
                                                                        #f
                                                                        #t)
                                                                    #t)
                                                                #t)))
                                                        #t))
                                                #t))
                                          #t))])
                        (let-values ([(e) (if wrap?
                                              (datum->syntax
                                               (quote-syntax here)
                                               (list wrapper
                                                     e)
                                               e)
                                              e)])
                          (datum->syntax
                           stx
                           (if (null? (cdr r))
                               (list (quote-syntax begin) e)
                               (list (quote-syntax begin)
                                     e
                                     (list* (quote-syntax do-wrapping-module-begin)
                                            wrapper
                                            (cdr r))))
                           stx))))))))))))
