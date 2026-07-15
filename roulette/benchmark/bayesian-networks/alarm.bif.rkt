#lang roulette/example/disrupt

(require "private/bayes.rkt")



(benchmark
      (main "bayesian-networks/alarm.bif"
      'PRESS
      '(HYPOVOLEMIA
        LVFAILURE
        STROKEVOLUME
        LVEDVOLUME
        PCWP
        CVP
        HISTORY
        ERRLOWOUTPUT
        ERRCAUTER
        INSUFFANESTH
        ANAPHYLAXIS
        TPR
        KINKEDTUBE
        FIO2
        PULMEMBOLUS
        PAP
        INTUBATION
        SHUNT
        DISCONNECT
        MINVOLSET
        VENTMACH
        VENTTUBE
        VENTLUNG
        VENTALV
        ARTCO2
        PVSAT
        SAO2
        CATECHOL
        HR
        CO
        BP
        HRSAT
        HREKG
        HRBP
        MINVOL
        EXPCO2
        PRESS)))
