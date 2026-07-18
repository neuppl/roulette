#lang roulette/example/disrupt

(require "../private/bayes.rkt" "../benchmarking.rkt")
(provide main)


(define (main)
  (benchmark
    (main* "bayesian-networks/insurance.bif"
           'PropCost
           '(Age
             SocioEcon
             OtherCar
             RiskAversion
             AntiTheft
             HomeBase
             SeniorTrain
             DrivingSkill
             DrivHist
             DrivQuality
             MakeModel
             VehicleYear
             Airbag
             Antilock
             RuggedAuto
             Cushioning
             GoodStudent
             Mileage
             CarValue
             Theft
             Accident
             ILiCost
             MedCost
             OtherCarCost
             ThisCarDam
             ThisCarCost
             PropCost))))
