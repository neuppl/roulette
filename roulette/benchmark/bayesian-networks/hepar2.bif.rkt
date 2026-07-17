#lang roulette/example/disrupt

(require "private/bayes.rkt")
(provide bn-hepar2)


(define (bn-hepar2)
      (main "bayesian-networks/hepar2.bif"
      'itching
      '(alcoholism
        vh_amn
        hepatotoxic
        RHepatitis
        THepatitis
        nausea
        anorexia
        hospital
        surgery
        gallstones
        amylase
        flatulence
        fat
        upper_pain
        choledocholithotomy
        transfusion
        injections
        ChHepatitis
        hbeag
        hcv_anti
        hbc_anti
        hbsag
        hbsag_anti
        fatigue
        fibrosis
        sex
        age
        Hyperbilirubinemia
        PBC
        joints
        pain
        le_cells
        ama
        pressure_ruq
        diabetes
        obesity
        Steatosis
        cholesterol
        ggtp
        ESR
        hepatomegaly
        hepatalgia
        pain_ruq
        triglycerides
        Cirrhosis
        carcinoma
        palms
        irregular_liver
        edge
        albumin
        spiders
        spleen
        ast
        alt
        encephalopathy
        consciousness
        density
        urea
        alcohol
        inr
        platelet
        bleeding
        edema
        proteins
        ascites
        phosphatase
        bilirubin
        jaundice
        skin
        itching)))
