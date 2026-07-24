#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out measure)
         measure/c
         measures-set!
         in-measures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/generator
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defns

(struct measure (procedure support density domain)
  #:property prop:procedure 0)

(define (measure/c dom cod)
  (and/c measure? (-> dom cod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; weak queue

(struct weak-queue-mcons (key val rest) #:mutable)
(struct weak-queue (front back) #:mutable)

;; Using a `queue` allows a predictable iteration order for constructing
;; expressions over the measure map, and is critical for distributions whose
;; parameters depend on other distributions.
(define measures (weak-queue #f #f))

(define (measures-set! k v)
  (match-define (weak-queue _front back) measures)
  (define elem (weak-queue-mcons (make-weak-box k) (make-ephemeron k v) #f))
  (cond
    [(not back)
     (set-weak-queue-front! measures elem)
     (set-weak-queue-back! measures elem)]
    [else
     (set-weak-queue-mcons-rest! back elem)
     (set-weak-queue-back! measures elem)]))

(define (in-measures)
  (in-producer
   (make-generator)
   (λ (x y) (not (or x y)))))

(define (make-generator)
  (generator ()
    (let go ([prev #f] [cur (weak-queue-front measures)])
      (cond
        [cur
         (match-define (weak-queue-mcons key-box val-eph rst) cur)
         (define key (weak-box-value key-box))
         (cond
           [key
            (yield key (ephemeron-value val-eph))
            (go cur rst)]
           [else
            (when prev (set-weak-queue-mcons-rest! prev rst))
            (go prev rst)])]
        [else
         (set-weak-queue-back! measures prev)
         (values #f #f)]))))
