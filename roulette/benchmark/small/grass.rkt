#lang benchmark_ci

(define cloudy (flip 0.5))
(define rain (if cloudy (flip 0.8) (flip 0.2)))
(define sprinkler (if cloudy (flip 0.1) (flip 0.5)))
(define temp1 (flip 0.7))
(define wet-roof (&& temp1 rain))
(define temp2 (flip 0.9))
(define temp3 (flip 0.9))
(define wet-grass (|| (&& temp2 rain) (&& temp3 sprinkler)))
(observe! wet-grass)
rain
