#lang roulette/example/disrupt 

(define earthquake (flip 0.0001))
(define burglary (flip 0.001))
(define alarm (|| earthquake burglary))
(define phone-working
  (if earthquake (flip 0.7) (flip 0.99)))
(define mary-wakes
  (if alarm
      (if earthquake (flip 0.8) (flip 0.6))
      (flip 0.2)))
(define called (&& mary-wakes phone-working))
(observe! called)
(query burglary)
(query (and (flip 0.5) (flip 0.5) (flip 0.5) (flip 0.5) (flip 0.5) (flip 0.5)))