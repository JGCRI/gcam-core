       PARAMETER(NMP = 23, NLDPMax = 19, NAGEP = 21)  !**** NLDPMax Must = param NLPMax in common.for
      Parameter( &
                nmERB = 9)     !  Number of periods in ERB

      CHARACTER*4 POPTYPE

      COMMON/DEMOG1/ &
        FCLM(nAGEp,NLDPMax,0:nMp),      &!  initial age-specific annual fertility rates   
        FCLZ(nAGEp,NLDPMax),            &!  terminal age-specific annual fertility rates 
        FXCLM(nAGEp,NLDPMax),           &!  weight for intermediate fclm values 
        GCLM(0:nMp,NLDPMax),            &!  male birth fractions  
        POPGCLM(2,nAGEp,NLDPMax,0:nMp), &!  base period population data 
        POPGCM(2,nAGEp,0:nMp),       &!  gender age period pop data 
        POPGLM(2,NLDPMax,0:nMp),        &!  gender region period pop data 
        POPGM(2,0:nMp),              &!  gender period pop data 
        POPLM(NLDPMax,0:nMp),           &!  region period pop data 
        POPM(0:nMp),                 &!  period pop data 
        BRTHGLM(2,NLDPMax,0:nMp),       &!  gender region period births  
        BRTHGM(2,0:nMp),             &!  gender period births 
        BRTHLM(NLDPMax,0:nMp),          &!  region period births  
        BRTHM(0:nMp),                &!  period births 
        DRL(2,nAgep,NLDPMax,0:nMp),     &!  5 year death rates 
        DTHGCLM(2,NAGEP,NLDPMax,0:nMp), &!  Gender age, region period deaths 
        DTHGCM(2,NAGEP,0:nMp),       &!  gender age period deaths 
        DTHGLM(2,NLDPMax,0:nMp),        &!  gender region period deaths 
        DTHGM(2,0:nMp),              &!  gender period deaths 
        DTHLM(NLDPMax,0:nMp),           &!  region period deaths 
        DTHM(0:nMp),                 &!  period deaths 
        MIGGCLM(2,NAGEP,NLDPMax,0:nMp), &!  Gender age, region period Migration 
        MIGGCM(2,NAGEP,0:nMp),       &!  gender age period Migration 
        MIGGLM(2,NLDPMax,0:nMp),        &!  gender region period Migration 
        MIGGM(2,0:nMp),              &!  gender period Migration 
        MIGLM(NLDPMax,0:nMp),           &!  region period Migration 
        MIGM(0:nMp),                 &!  period Migration 
        SVGCLM(2,nAGEp,NLDPMax,0:nMp),  &!  initial death rates 
        SVGCLZ(2,nAGEp,NLDPMax),        &!  terminal death rates 
        SVXGCLM(2,nAGEp,NLDPMax),       &!  weight for intermediate svglm values 
        TSVGCLZ(2,NLDPMax),             &!  time-to-terminal death rates  
        TFCLZ(NLDPMax),                 &!  time-to-terminal fertility rate 
        TXMIGZ(NLDPMax),                &!  time-to-terminal net migration rate 
        XMIG(2,nAGEp,NLDPMax,0:nMp),    &!  initial migration rates 
        YNG(2,NLDPMax,nMp),             &!  pop less than 15 
        OLD(2,NLDPMax,nMp),             &!  pop over 65 
        XMIGZ(2,nAGEp,NLDPMax),         &!  terminal migration rates 
        XXMIG(2,nAGEp,NLDPMax),         &!  weight for intermediate xmig values 
        MAXLIFE,                     &!  maximum age allowed in model 
        NAGE,                        &!  number of age groups. Set in popdata.for 
        nStep,                       &!  number of years in each time step 
        PopType 			!  population data type  only rate allowed.


       Real*4 &
            WAgeMC(2,NLDPMax,nmERB),      &!  working age population for MiniCAM. Passed from erb 
            DemoStatMC(10,NLDPMax,nmERB)  !  Demographic statistics for MiniCAM. Passed from erb
 
       Real*8 &
            Z(nmERB),            &!  total population in period M, ERB 
            ZLM(NLDPMax,0:nmERB)     !  regional population in ERB. Passed from erb
     
