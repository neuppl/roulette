#lang roulette/example/disrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide cost-lattice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (prefix-in base: racket/base)
         roulette/example/disrupt/private/pmf
         gtp-plot
         gtp-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; profile

(define (cost-lattice val #:iterations [iters 10])
  (define vars (symbolics val))
  (define result
    (for/hash ([var-subset (in-combinations vars)])
      (values (subset->bitstring var-subset vars)
              (sample-symbolic-union val var-subset iters))))
  (define configs
    (for/hash ([(vars runtimes) (in-hash result)])
      (define num-sampled (- (string-length vars) (count-zero-bits vars)))
      (values vars (configuration-info vars num-sampled runtimes))))

  (define units (inexact->exact (log (hash-count configs) 2)))
  (define none (make-string units #\0))
  (define all (make-string units #\1))

  (define pi
    (make-performance-info
     'anonymous
     #:src #f
     #:num-units units
     #:num-configurations (hash-count configs)
     #:baseline-runtime* (configuration-info-runtime* (hash-ref configs none))
     #:untyped-runtime* (configuration-info-runtime* (hash-ref configs none))
     #:typed-runtime* (configuration-info-runtime* (hash-ref configs all))
     #:make-in-configurations
     (λ _ (hash-values configs))))

  (parameterize ([*LATTICE-LINES?* #t])
    (performance-lattice pi)))

(define (subset->bitstring var-subset vars)
  (define chars
    (for/list ([var (in-list vars)])
      (if (base:member var var-subset) #\1 #\0)))
  (apply string chars))

(define (sample-symbolic-union val vars iters)
  (define (make-env)
    (for/hash ([var (in-set vars)])
      (define pr (hash-ref (pmf-hash (query var)) #t 0))
      (values var (< (random) pr))))
  (for/list ([_ (in-range iters)])
    (clear-cache!)
    (query val #:environment (make-env))
    (recursive-calls)))
