#lang benchmark_ci

(define (slower-fn)
  (sleep 1)
  "Completed")

  
(slower-fn)