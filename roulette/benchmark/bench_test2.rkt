#lang benchmark_ci

(define (slower-fn)
  (sleep 2)
  "Completed")

  
(slower-fn)