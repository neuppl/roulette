#lang roulette/example/disrupt

(require "private/bayes.rkt")
(provide bn-insurance)


(define (bn-insurance)
      (main "bayesian-networks/insurance.bif"
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
        PropCost)))
