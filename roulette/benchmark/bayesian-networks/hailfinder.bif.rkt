#lang roulette/example/disrupt

(require "private/bayes.rkt")
(provide bn-hailfinder)


(define (bn-hailfinder)
      (main "bayesian-networks/hailfinder.bif"
      'R5Fcst
      '(N0_7muVerMo
        SubjVertMo
        QGVertMotion
        CombVerMo
        AreaMeso_ALS
        SatContMoist
        RaoContMoist
        CombMoisture
        AreaMoDryAir
        VISCloudCov
        IRCloudCover
        CombClouds
        CldShadeOth
        AMInstabMt
        InsInMt
        MountainFcst
        WndHodograph
        CldShadeConv
        OutflowFrMt
        MorningBound
        Boundaries
        CompPlFcst
        CapChange
        LoLevMoistAd
        InsChange
        Date
        Scenario
        WindFieldPln
        WindFieldMt
        WindAloft
        TempDis
        SynForcng
        SfcWndShfDis
        RHRatio
        MvmtFeatures
        MidLLapse
        MeanRH
        LowLLapse
        Dewpoints
        ScnRelPlFcst
        ScenRel3_4
        ScenRelAMIns
        ScenRelAMCIN
        MorningCIN
        AMCINInScen
        CapInScen
        LIfr12ZDENSd
        AMDewptCalPl
        AMInsWliScen
        InsSclInScen
        LatestCIN
        LLIW
        CurPropConv
        PlainsFcst
        N34StarFcst
        R5Fcst)))
