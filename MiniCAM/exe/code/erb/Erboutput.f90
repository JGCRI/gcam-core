!***********************************************************************
!
!     New output subroutine to write out results in separate tables
!     shk 9/28/98
!
!***********************************************************************

      SUBROUTINE ERBOUTPUT
      
!***********************************************************************     


!     COMMON BLOCK
      USE COMMON
	  USE MAGCOM
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	  
!***********************************************************************
!
!     More detailed energy accounting 2-d flatfile output
!
	REAL*8 H2WORLD(NHP,NMP),ELECH2G(NMP)
	REAL*4 TOTAL,TEMPTOT,GTOTAL,TEMPGTOT,GR,CVRTO,CVRTG,CVRTC

!     Convert c. oil prices in $/GJ to $/bbl
      CVRTO = 1.055*5.8
!     Convert refined oil (gasoline) prices in $/GJ to $/gal
      CVRTAG = 1055*115400.0/1.0E9
!     Convert n. gas prices in $/GJ to $/mcf
      CVRTG = 1.055
!     Convert bituminous coal prices in $/GJ to $/tonne
      CVRTC = 29.1

!     Add one to last region for global total
	NLL=NL+1

      IIOIL=1
      IISOIL=2 !index for emissions from shale oil
      IICSYN=3 !index for emissions from coal synfuels
      IICOAL=4
      IIBSYN=5 !index for emissions credit from biomass synfuels
      IIGAS=6
	IICSCRUB=7
	IIBMASS=8
      IIFLR=9 !index for flared emissions
      IIGSYN=10 !synfuel liquid production from n. gas

      IF (GenRelease .ne. 1) OPEN (100,FILE='Erboutput.csv')
  854 FORMAT(A8,',',2(I8,','),F12.3,',',18(F12.3,','))
  855 FORMAT(A8,',',2(I8,','),15(F12.0,','))
  856 FORMAT(A8,',',2(I8,','),10(E16.4,','))
 1000 FORMAT(A80)
 1001 FORMAT(20(A10,','))
 1002 FORMAT(20(A,','))	
 1003 FORMAT(40(A,','))	! increased value to allow all OGCoefs to write out

      IF (GenRelease .ne. 1) WRITE(100,1000)CASENAME

      DO 850 L=1,NLL !REGION AND GLOBAL DO LOOP 


!     Calculate CO2 emissions by fuel, sector, region, and period.
!     fuel: oil, gas, coal
!     sector: res/com, ind, tran, elect, synfuel, H2 prod
!     SHK 6/16/99
      IF (L.NE.NLL) THEN
      DO M=2,NM
      CO2LMT(L,M)=0.0
	DO K=1,6
         CO2KLM(K,L,M)=0.0
         !Res/Com,Industry,Transportation 
         IF (K.LE.3) THEN
            DO J=1,INCOAL
               CO2JKLM(J,K,L,M)=FJKLM(J,K,L,M)*COI(J)*(1.0-SFEDIL(J,L))
               CO2KLM(K,L,M)=CO2KLM(K,L,M)+CO2JKLM(J,K,L,M)
            END DO
            !attribute shale oil production(J=4) emissions to industrial sector
            !attribute flared emissions(J=5) to industrial sector
            IF (K.EQ.2) THEN 
               CO2JKLM(4,K,L,M)=CO2ILM(IISOIL,L,M)	! as calculated in routine CO2. sjs - 04/02
               CO2JKLM(5,K,L,M)=CO2ILM(IIFLR,L,M)
		       CO2KLM(K,L,M)=CO2KLM(K,L,M)+CO2JKLM(4,K,L,M)+CO2JKLM(5,K,L,M)
            END IF
         !Electric Utility
         ELSEIF (K.EQ.4) THEN
            DO J=1,INCOAL
               CO2JKLM(J,K,L,M)=EDRIKLM(J,1,L,M)*COI(J)*(1.0-SFEDIL(J,L))
               CO2KLM(K,L,M)=CO2KLM(K,L,M)+CO2JKLM(J,K,L,M)
            END DO
            !subtract sequestered from utility sector
            CO2KLM(K,L,M)=CO2KLM(K,L,M)-CARBSEQ(2,L,M) 
         !Synfuel Production Sector w/ Biomass Credit 
         ELSEIF (K.EQ.5) THEN
	      CO2JKLM(INGAS,K,L,M)=CO2ILM(IIGSYN,L,M)
            CO2JKLM(INCOAL,K,L,M)=CO2ILM(IICSYN,L,M)
            CO2JKLM(INBMASS,K,L,M)=CO2ILM(IIBSYN,L,M)
            CO2KLM(K,L,M)=CO2JKLM(INGAS,K,L,M)+CO2JKLM(INCOAL,K,L,M)+CO2JKLM(INBMASS,K,L,M)
         !Hydrogen Production Sector (K=6) from fossil fuels
         ELSE
            DO J=1,INCOAL
	         CO2JKLM(J,K,L,M)=EDRIKLM(J,KH2,L,M)*COI(J)
	         CO2KLM(K,L,M)=CO2KLM(K,L,M)+CO2JKLM(J,K,L,M)
            END DO
            !Carbon sequestered during H2 production   
            CO2JKLM(4,K,L,M)=-CARBSEQ(3,L,M)
            CO2KLM(K,L,M)=CO2KLM(K,L,M)+CO2JKLM(4,K,L,M)
         END IF

         !Total Regional CO2 Emissions
         CO2LMT(L,M)=CO2LMT(L,M)+CO2KLM(K,L,M)
         !Total Global CO2 Emissions by Sector
         CO2KLM(K,NNLP,M)=CO2KLM(K,NNLP,M)+CO2KLM(K,L,M)	        
         !Total Global CO2 Emissions by Sector and fuel
         DO J=1,6
            CO2JKLM(J,K,NNLP,M)=CO2JKLM(J,K,NNLP,M)+CO2JKLM(J,K,L,M)	    
         END DO
      END DO !K LOOP
      !Total Global CO2 Emissions
      CO2LMT(NNLP,M)=CO2LMT(NNLP,M)+CO2LMT(L,M)
      END DO !M LOOP
      END IF


	IF (GenRelease .ne. 1) WRITE(100,*)'******* REGION',L,' ',REGNAMES(L),' *******'

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Gross Domestic Product (Billion 1990 US $)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,GDP(ppp),GDP(mer),GrowthRate,GDP(mer)/Cap($/per)'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         GR=(GNPPPP(L,M)/GNPPPP(L,M-1))**(1.0/15.0)-1.0
         WRITE(100,854)REGNAMES(L),L,IYR,GNPPPP(L,M),GNPMRKT(L,M),GR, &
         GNPMRKT(L,M)/ZLM(L,M)*1.0E6
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Working Age and Total Population (1000)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,MaleWork,FemWork,WorkTot,PopTot,GrowthRate'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            GR=(ZLM(L,M)/ZLM(L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,WAgeMC(1,L,M),WAgeMC(2,L,M), &
            (WAgeMC(1,L,M)+WAgeMC(2,L,M)),ZLM(L,M),GR
	   ELSE
            GR=(ZLM(L,M)/ZLM(L,M-1))**(1.0/15.0)-1.0
	      IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,WAgeMC(1,L,M),WAgeMC(2,L,M), &
            (WAgeMC(1,L,M)+WAgeMC(2,L,M)),ZLM(L,M)*1000.0,GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Carbon Emissions by Fuel (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,BiomCrdt,Flared,Total,GR,CO2/cap'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            GR=(CO2DLM(L,M)/CO2DLM(L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CARBFUEL(IN,L,M),IN=1,INBMASS),CO2ILM(IIFLR,L,M), &
             CO2DLM(L,M),GR,CO2DLM(L,M)/ZLM(L,M)*1000
	   ELSE
            GR=(CO2DM(M)/CO2DM(M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CARBFUEL(IN,L,M),IN=1,INBMASS),CO2IM(IIFLR,M), &
             CO2DM(M),GR,CO2DM(M)/ZLM(L,M)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Carbon Emissions by Sector (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Res/Com,Ind,Tran,Elect,Synfuel,H2Prod,Total,GR'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         GR=(CO2LMT(L,M)/CO2LMT(L,M-1))**(1.0/15.0)-1.0
         IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2KLM(K,L,M),K=1,6),CO2LMT(L,M),GR
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Res/Com Sector Carbon Emissions (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Total,GrowthRate'
      K=1
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         IF (L.NE.NLL) THEN
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,INCOAL), &
             CO2KLM(K,L,M),GR
         ELSE
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,INCOAL), &
             CO2KLM(K,L,M),GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Industrial Sector Carbon Emissions (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,ShaleOil,Flared,Total,GR'
      K=2
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         IF (L.NE.NLL) THEN
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,5), &
             CO2KLM(K,L,M),GR
         ELSE
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,5), &
             CO2KLM(K,L,M),GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector Carbon Emissions (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Total,GrowthRate'
      K=3
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         IF (L.NE.NLL) THEN
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,INCOAL), &
             CO2KLM(K,L,M),GR
         ELSE
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,INCOAL), &
             CO2KLM(K,L,M),GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Electricity Sector Carbon Emissions (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Total,GrowthRate'
      K=4
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         IF (L.NE.NLL) THEN
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,INCOAL), &
             CO2KLM(K,L,M),GR
         ELSE
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,INCOAL), &
             CO2KLM(K,L,M),GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Synfuel Production Precess Carbon Emissions (Million Metric Tonnes)' 
	IF (GenRelease .ne. 1) WRITE(100,*)'(Does not include emissions from synfuel combustion)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,N.Gas,Coal,BiomCrdt,Total,GrowthRate'
      K=5
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         IF (L.NE.NLL) THEN
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=2,4), &
             CO2KLM(K,L,M),GR
         ELSE
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=2,4), &
             CO2KLM(K,L,M),GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Carbon Emissions Hydrogen Production (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Sequest,Total,GrowthRate'
      K=6
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         IF (L.NE.NLL) THEN
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,4), &
             CO2KLM(K,L,M),GR
         ELSE
            GR=(CO2KLM(K,L,M)/CO2KLM(K,L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (CO2JKLM(J,K,L,M),J=1,4), &
             CO2KLM(K,L,M),GR
         END IF
      END DO


!     WRITE OUT GHG CONCENTRATIONS IF L=WORLD
      IF (L.EQ.NLL) THEN
!        CO2 CONCENTRATION
         IF (GenRelease .ne. 1) WRITE(100,*)
         IF (GenRelease .ne. 1) WRITE(100,*)'Atmospheric CO2 Concentration (PPMV)'
         IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,CO2Low,CO2Mid,CO2High,CO2User,GrowthRate'
         DO M=2,NM
  	      IYR = 1975 + (M-1)*NJUMP
            GR=(CO2ppmv(m)/CO2ppmv(m-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,CO2lppmv(m), &
               CO2mppmv(m),CO2hppmv(m),CO2ppmv(m),GR
         END DO

!        CH4 CONCENTRATION
         IF (GenRelease .ne. 1) WRITE(100,*)
         IF (GenRelease .ne. 1) WRITE(100,*)'Atmospheric CH4 Concentration (PPMV)'
         IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,CH4Low,CH4Mid,CH4High,CH4User,GrowthRate'
         DO M=2,NM
  	      IYR = 1975 + (M-1)*NJUMP
            GR=(CH4ppmv(m)/CH4ppmv(m-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,CH4Lppmv(m), &
              CH4mppmv(m),CH4Hppmv(m),CH4ppmv(m),GR
         END DO

!        N2O CONCENTRATION
         IF (GenRelease .ne. 1) WRITE(100,*)
         IF (GenRelease .ne. 1) WRITE(100,*)'Atmospheric N2O Concentration (PPMV)'
         IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,N2OUser,GrowthRate'
         DO M=2,NM
  	      IYR = 1975 + (M-1)*NJUMP
            GR=(RN2Oppmv(m)/RN2Oppmv(m-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,RN2Oppmv(m),GR
         END DO

!        Radiative Aerosol Forcing
         IF (GenRelease .ne. 1) WRITE(100,*)
         IF (GenRelease .ne. 1) WRITE(100,*)'Radiative Aerosol Forcing by GHGs (Watts/m2)'
         IF (GenRelease .ne. 1) WRITE(100,1002)'Region','Region#','Year','FcCO2','FcCH4', &
      'FcN2O','FcHALOS','FcTROPO3','FcSO4DIR','FcSO4IND','FcBIOAER', &
      'FcTOTAL','GrowthRate'
         DO M=2,NM
  	      IYR = 1975 + (M-1)*NJUMP
            GR=(FCTOT(M)/FCTOT(M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,FCCO2(M),FCCH4(M), &
            FCN2O(M),FCHALOS(M),FCTROPO3(M),FCSO4DIR(M),FCSO4IND(M), &
            FCBIO(M),FCTOT(M),GR
         END DO

!        GHG Emissions
         IF (GenRelease .ne. 1) WRITE(100,*)
         IF (GenRelease .ne. 1) WRITE(100,*)'GHG Emissions (GtC/yr)'
         IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,FossCO2,NETDEFOR,CH4,N2O'
         DO M=2,NM
  	      IYR = 1975 + (M-1)*NJUMP
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,FOSSCO2(M),RNETDEF(M), &
            CH4EM(M),RN2OEM(M)
         END DO

!        Sulfur Emissions
         IF (GenRelease .ne. 1) WRITE(100,*)
         IF (GenRelease .ne. 1) WRITE(100,*)'Sulfur Emissions (GtC/yr)'
         IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,SO2-REG1,SO2-REG2,SO2-REG3'
         DO M=2,NM
  	      IYR = 1975 + (M-1)*NJUMP
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,SO2REG1(M),SO2REG2(M), &
            SO2REG3(M)
         END DO

      END IF
!     END OF GLOBAL GHG CONCENTRATIONS


	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Carbon Taxes (1990 $/Tonne of Carbon)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,CarbTax'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
	       IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,TAXLM(L,M)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Carbon Sequestration (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,PlantCapt,Synfuel,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,-CARBSEQ(2,L,M), &
            -CARBSEQ(1,L,M),-CARBSEQ(3,L,M)
	   ELSE
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,-CARBSEQM(2,M), &
            -CARBSEQM(1,M),-CARBSEQM(3,M)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Carbon Backstop (Million Metric Tonnes)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,CarbBackstop'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,-(CO2DLMG(L,M)-CO2DLM(L,M))
	   ELSE
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,-(CO2MG(M)-CO2DM(M))
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Primary Energy Consumption (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,1001)'Region','Region#','Year','C.Oil', &
      'N.Gas','Coal','Biomass','Hydro','Solar', &
      'Nuclear','Fusion','Total','GrowthRate','E/Cap'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         IF (L.NE.NLL) THEN
            EDCOAL=ESILM(INCOAL,L,M)+EXILM(INCOAL,L,M)
            EDTOT=EDILM(INOIL,L,M)+EDILM(INGAS,L,M)+EDCOAL+ &
               EDILM(IBMASS,L,M)+ESILM(6,L,M)+ESILM(4,L,M)+ESILM(5,L,M)+ &
               ESILM(12,L,M)
            TEMPTOT=EDILM(INOIL,L,M-1)+EDILM(INGAS,L,M-1)+ &
               ESILM(INCOAL,L,M-1)+EXILM(INCOAL,L,M-1)+ &
               EDILM(IBMASS,L,M-1)+ESILM(6,L,M-1)+ESILM(4,L,M-1)+ &
               ESILM(5,L,M-1)+ESILM(12,L,M-1)
            GR=(EDTOT/TEMPTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,EDILM(INOIL,L,M), &
            EDILM(INGAS,L,M), &
            EDCOAL,EDILM(IBMASS,L,M),ESILM(6,L,M),ESILM(5,L,M), &
            ESILM(4,L,M),ESILM(12,L,M),EDTOT,GR,EDTOT/ZLM(L,M)*1.0E6
	   ELSE
            GR=(EDM(M)/EDM(M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(EDIM(I,M),I=1,NF), &
            EDIM(IBMASS,M),EDIM(6,M),EDIM(5,M),EDIM(4,M),EDIM(12,M), &
            EDM(M),GR,EDM(M)/ZLM(L,M)
         END IF	  
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Primary Energy Production (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass, SubTotal'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         TOTAL=0.0
         GTOTAL=0.0
	   DO  I = 1,NF
	       GTOTAL=GTOTAL+ESIM(I,M)
         END DO
	   GTOTAL=GTOTAL+ESIM(IBMASS,M)
         IF (L.NE.NLL) THEN
 	      DO I = 1,NF
 	         TOTAL=TOTAL+ESILM(I,L,M)
            END DO
	      TOTAL=TOTAL+ESILM(IBMASS,L,M)
	      IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(ESILM(I,L,M),I=1,NF), &
            ESILM(IBMASS,L,M),TOTAL
	   ELSE
	      IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(ESIM(I,M),I=1,NF), &
            ESIM(IBMASS,M),GTOTAL
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Fuel Import(+) and Export(-) (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass,Total'
      DO M=2,NM
  	   TOTAL=0.0
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
	      DO I = 1,NF
	        TOTAL=TOTAL+EXILM(I,L,M)
	      END DO
	      TOTAL=TOTAL+EXILM(IBMASS,L,M)
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(EXILM(I,L,M),I=1,NF), &
            EXILM(IBMASS,L,M),TOTAL
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Energy Resources Available (EJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,U.Oil,NucFuel,Total'
      DO M=2,NM
  	   TOTAL=0.0
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
	      DO I = 1,5
	        TOTAL=TOTAL+RESOURCE(I,L)-QISLM(I,L,M)
	      END DO
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(RESOURCE(I,L) &
            -QISLM(I,L,M),I=1,5),TOTAL
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Components of C. Oil Production  (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Conv.Oil,ShaleOil,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,ESIL1M(1,L,M), &
                          ESIL2M(1,L,M),ESILM(1,L,M)
	   ELSE
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,ESI1M(1,M),ESI2M(1,M), &
                          ESIM(1,M)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Global Prices for Primary Fuels  (1997 US $/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,1002)'Region','Region#','Year','C.Oil','N.Gas', &
      'Coal','Biomass','C.Oil(97$/bbl)','N.Gas(97$/mcf)', &
      'Coal(97$/tonne)'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ((P(I,L,M)*CVRT97),I=1,INBMASS), &
             P(INOIL,L,M)*CVRT97*CVRTO, &
             P(INGAS,L,M)*CVRT97*CVRTG, &
             P(INCOAL,L,M)*CVRT97*CVRTC
	   ELSE
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ((P(I,1,M)*CVRT97),I=1,INBMASS), &
             P(INOIL,1,M)*CVRT97*CVRTO, &
             P(INGAS,1,M)*CVRT97*CVRTG, &
             P(INCOAL,1,M)*CVRT97*CVRTC
	   END IF 
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Regional Prices for Primary Fuels: Plus Transportation & Tax  (1997 US $/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,1002)'Region','Region#','Year','C.Oil','N.Gas', &
      'Coal','Biomass', &
      'C.Oil(97$/bbl)','N.Gas(97$/mcf)','Coal(97$/tonne)'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ((PILM(I,L,M)*CVRT97),I=1,NF), &
            PILM(IBMASS,L,M)*CVRT97, &
            PILM(INOIL,L,M)*CVRT97*CVRTO, &
            PILM(INGAS,L,M)*CVRT97*CVRTG, &
            PILM(INCOAL,L,M)*CVRT97*CVRTC
	   END IF 
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Regional Prices for Refined/Delivered Fuels (1997 US $/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,1002)'Region','Region#','Year','C.Oil','N.Gas', &
      'Coal','Biomass','NucFuel','N.Gas-Liq','Coal-Liq','Coal-Gas', &
      'Bio-Liq','Bio-Gas','C.Oil(97$/bbl)','Gasoline(97$/gal)', &
      'N.Gas(97$/mcf)','Coal(97$/tonne)'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ((PJLM(J,L,M)*CVRT97),J=1,NF), &
            PJLM(JSBMASS,L,M)*CVRT97, &
            PILM(4,L,M)*CVRT97, &
            PCJILM(2,1,L,M)*CVRT97, &
           (PCJILM(3,I,L,M)*CVRT97,I=1,2), &
           (PCJILM(4,I,L,M)*CVRT97,I=1,2), &
            PJLM(INOIL,L,M)*CVRT97*CVRTO, &
            PJLM(INOIL,L,M)*CVRT97*CVRTAG, &
            PJLM(INGAS,L,M)*CVRT97*CVRTG, &
            PJLM(INCOAL,L,M)*CVRT97*CVRTC
	   END IF 
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Refinery Demand for Fuel by Fuel Type  (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            GR=(EDRLM(L,M)/EDRLM(L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(EDRILM(I,L,M),I=1,NF), &
            EDRILM(IBMASS,L,M),EDRLM(L,M)
	   ELSE
            GR=(EDRM(M)/EDRM(M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(EDRIM(I,M),I=1,NF), &
            EDRIM(IBMASS,M),EDRM(M)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Fuel Input for Synthetic Fuel Production  (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,N.Gas,Coal,Biomass,Total,GrowthRate'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         TOTAL=0.0
         GTOTAL=0.0
         TEMPTOT=0.0
         TEMPGTOT=0.0
	   IF (L.NE.NLL) THEN
	      TOTAL=TOTAL+SYNINPUT(INGAS,L,M)+SYNINPUT(INCOAL,L,M) &
                  +SYNINPUT(INBMASS,L,M)
	      TEMPTOT=TEMPTOT+SYNINPUT(INGAS,L,M-1)+SYNINPUT(INCOAL,L,M-1) &
                   +SYNINPUT(INBMASS,L,M-1)
            GR=(TOTAL/TEMPTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(SYNINPUT(J,L,M), &
            J=2,4),TOTAL,GR
	   ELSE
	      GTOTAL=GTOTAL+SYNINPUT(INGAS,L,M)+SYNINPUT(INCOAL,L,M) &
                  +SYNINPUT(INBMASS,L,M)
	      TEMPGTOT=TEMPGTOT+SYNINPUT(INGAS,L,M-1) &
                  +SYNINPUT(INCOAL,L,M-1)+SYNINPUT(INBMASS,L,M-1)
            GR=(GTOTAL/TEMPGTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(SYNINPUT(J,L,M), &
            J=2,4),GTOTAL,GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Synthetic Fuel Production  (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,N.Gas-Liq,Coal-Liq,Coal-Gas,Bio-Liq,Bio-Gas,Total,GR'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         TOTAL=0.0
         TEMPTOT=0.0
         GTOTAL=0.0
         TEMPGTOT=0.0
	   DO  I = 1,2
	       GTOTAL=GTOTAL+SYNIM(I,M)
             TEMPGTOT=TEMPGTOT+SYNIM(I,M-1)
         END DO

	   IF (L.NE.NLL) THEN
 	      DO I = 1,2
	        TOTAL=TOTAL+SYNILM(I,L,M)
	        TEMPTOT=TEMPTOT+SYNILM(I,L,M-1)
            END DO
            GR=(TOTAL/TEMPTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,SYNFUEL(2,1,L,M), &
            (SYNFUEL(3,I,L,M),I=1,2),(SYNFUEL(4,I,L,M),I=1,2),TOTAL,GR
	   ELSE
            GR=(GTOTAL/TEMPGTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,SYNJIM(2,1,M), &
            (SYNJIM(3,I,M),I=1,2),(SYNJIM(4,I,M),I=1,2),GTOTAL,GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Synthetic Fuel Production Efficiency (out/in)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,N.Gas-Liq,Coal-Liq,Coal-Gas,Bio-Liq,Bio-Gas'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,1/GCI(2,1), &
            (1/GCI(3,I),I=1,2),(1/GCI(4,I),I=1,2)
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Synthetic Fuel Prices (90$/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,N.Gas-Liq,Coal-Liq,Coal-Gas,Bio-Liq,Bio-Gas'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
           IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,PCJILM(2,1,L,M)*CVRT90, &
          (PCJILM(3,I,L,M)*CVRT90,I=1,2),(PCJILM(4,I,L,M)*CVRT90,I=1,2)
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Energy Demand by End-Use Sector  (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Buildings,Industrial,Transportation,Total,GrowthRate'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            GR=(EFLM(L,M)/EFLM(L,M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(EFKLM(K,L,M),K=2,4), &
            EFLM(L,M),GR
	   ELSE
            GR=(EFM(M)/EFM(M-1))**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(EFKM(K,M),K=2,4), &
            EFM(M),GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'End-Use Sector Energy Service (Service/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Buildings,Industrial,Transportation,Total,GrowthRate'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         GR=(ESERV(L,M)/ESERV(L,M-1))**(1.0/15.0)-1.0
         IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(ESERVK(K,L,M),K=1,3), &
            ESERV(L,M),GR
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Buildings Sector Energy Consumption (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total,GrowthRate'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         TOTAL=0.0
         TEMPTOT=0.0
         GTOTAL=0.0
         TEMPGTOT=0.0
	   IF (L.NE.NLL) THEN
	      DO  J = 1,NNJ
	          TOTAL=TOTAL+FJKLM(J,1,L,M)
	          TEMPTOT=TEMPTOT+FJKLM(J,1,L,M-1)
            END DO
            GR=(TOTAL/TEMPTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(FJKLM(J,1,L,M), J=1,NNJ), &
            TOTAL,GR
         ELSE
	      DO  J = 1,NNJ
	          GTOTAL=GTOTAL+FJKM(J,1,M)
	          TEMPGTOT=TEMPGTOT+FJKM(J,1,M-1)
            END DO
            GR=(GTOTAL/TEMPGTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(FJKM(J,1,M), J=1,NNJ), &
            GTOTAL,GR
	   END IF
	END DO
 
      IF (GenRelease .ne. 1) WRITE(100,*)
      IF (GenRelease .ne. 1) WRITE(100,*)'Industrial Sector Energy Consumption (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total,GrowthRate'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         TOTAL=0.0
         TEMPTOT=0.0
         GTOTAL=0.0
         TEMPGTOT=0.0
	   IF (L.NE.NLL) THEN
            DO  J = 1,NNJ
	          TOTAL=TOTAL+FJKLM(J,2,L,M)
	          TEMPTOT=TEMPTOT+FJKLM(J,2,L,M-1)
            END DO
            GR=(TOTAL/TEMPTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(FJKLM(J,2,L,M), J=1,NNJ), &
            TOTAL,GR
	   ELSE
            DO  J = 1,NNJ
	          GTOTAL=GTOTAL+FJKM(J,2,M)
	          TEMPGTOT=TEMPGTOT+FJKM(J,2,M-1)
            END DO
            GR=(GTOTAL/TEMPGTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(FJKM(J,2,M), J=1,NNJ), &
            GTOTAL,GR
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector Energy Consumption (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total,GrowthRate'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         TOTAL=0.0
         TEMPTOT=0.0
         GTOTAL=0.0
         TEMPGTOT=0.0
	   IF (L.NE.NLL) THEN
            DO  J = 1,NNJ
	          TOTAL=TOTAL+FJKLM(J,3,L,M)
	          TEMPTOT=TEMPTOT+FJKLM(J,3,L,M-1)
            END DO
            GR=(TOTAL/TEMPTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(FJKLM(J,3,L,M), J=1,NNJ), &
            TOTAL,GR
         ELSE
            DO  J = 1,NNJ
	          GTOTAL=GTOTAL+FJKM(J,3,M)
	          TEMPGTOT=TEMPGTOT+FJKM(J,3,M-1)
            END DO
            GR=(GTOTAL/TEMPGTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(FJKM(J,3,M), J=1,NNJ), &
            GTOTAL,GR
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Buildings Sector End-Use Energy Service Cost (90$/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(PJKLM(J,1,L,M)*CVRT90,  &
            J=1,NNJ)
	   END IF
	END DO
 
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Industrial Sector End-Use Energy Service Cost (90$/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity, Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(PJKLM(J,2,L,M)*CVRT90,  &
            J=1,NNJ)
	   END IF
	END DO
 
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector End-Use Energy Service Cost (90$/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity, Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(PJKLM(J,3,L,M)*CVRT90,  &
            J=1,NNJ)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Buildings Sector End-Use Energy Output/Input (%)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity, Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(1/GJKLM(J,1,L,M), J=1,NNJ)
	   END IF
	END DO
 
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Industrial Sector End-Use Energy Output/Input (%)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity, Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(1/GJKLM(J,2,L,M), J=1,NNJ)
	   END IF
	END DO
 
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector End-Use Energy Output/Input (%)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(1/GJKLM(J,3,L,M), J=1,NNJ)
	   END IF
	END DO
 
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Buildings Sector End-Use Non-Energy Cost (90$/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(HJKLM(J,1,L,M)*CVRT90,  &
            J=1,NNJ)
	   END IF
	END DO
 
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Industrial Sector End-Use Non-Energy Cost (90$/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(HJKLM(J,2,L,M)*CVRT90,  &
            J=1,NNJ)
	   END IF
	END DO
 
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector End-Use Non-Energy Cost (90$/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(HJKLM(J,3,L,M)*CVRT90,  &
            J=1,NNJ)
	   END IF
	END DO

        IF (NewTransp .eq. 1) THEN		! Only print this out if using new transport sector
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Total Fuel Consumption by Mode (Quads)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Auto,Truck,Bus,Rail,Air,Ship,Motorcycle,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFUEL(3,I,NFUEL+1,L,M),I=1,NTMODE+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Total Fuel Consumption by Fuel (Quads)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFUEL(3,NTMODE+1,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Consumption by Mode (Quads)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Auto,Truck,Bus,Rail,Air,Ship,Motorcycle,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFUEL(1,I,NFUEL+1,L,M),I=1,NTMODE+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Consumption by Fuel (Quads)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFUEL(1,NTMODE+1,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Fuel Consumption by Mode (Quads)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Auto,Truck,Bus,Rail,Air,Ship, Motorcycle,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFUEL(2,I,NFUEL+1,L,M),I=1,NTMODE+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Fuel Consumption by Fuel (Quads)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFUEL(2,NTMODE+1,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service (Million Pass-mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Auto,Truck,Bus,Rail,Air,Ship, Motorcycle,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,I,NFUEL+1,L,M),I=1,NTMODE+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service (Million Veh-mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Auto,Truck,Bus,Rail,Air,Ship,Motorcycle,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,I,NFUEL+1,L,M)/TLOADFAC(1,I,L,M),I=1,NTMODE)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service (Million Ton-mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Auto,Truck,Bus,Rail,Air,Ship,Motorcycle,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(2,I,NFUEL+1,L,M),I=1,NTMODE+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service (Million Veh-mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Auto,Truck,Bus,Rail,Air,Ship,Motorcycle,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(2,I,NFUEL+1,L,M)/TLOADFAC(2,I,L,M),I=1,NTMODE)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service by Fuel - Auto (Million Pass-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,1,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service by Fuel - Truck (Million Pass-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,2,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service by Fuel - Bus (Million Pass-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,3,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service by Fuel - Rail (Million Pass-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,4,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service by Fuel - Air (Million Pass-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,5,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service by Fuel - Ship (Million Pass-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,6,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service by Fuel - MotorCyc (Million Pass-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(1,7,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service by Fuel - Truck (Million Ton-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(2,2,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service by Fuel - Rail (Million Ton-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(2,4,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service by Fuel - Air (Million Ton-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(2,5,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service by Fuel - Ship (Million Ton-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,855)REGNAMES(L),L,IYR,  &
            (TRANSERV(2,6,J,L,M),J=1,NFUEL+1)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Cost - Auto (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(1,1,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Cost - Truck (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(1,2,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Cost - Bus (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(1,3,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Cost - Rail (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(1,4,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Cost - Air (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(1,5,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Cost - Ship (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(1,6,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Cost - MotorCycle (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(1,7,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Energy Intensity - Auto (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(1,1,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Energy Intensity - Truck (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(1,2,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Energy Intensity - Bus (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(1,3,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Energy Intensity - Rail (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(1,4,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Energy Intensity - Air (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(1,5,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Energy Intensity - Ship (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(1,6,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Energy Intensity - MtrCyc (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(1,7,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Cost - Auto (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(1,1,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Cost - Truck (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(1,2,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Cost - Bus (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(1,3,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Cost - Rail (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(1,4,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Cost - Air (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(1,5,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Cost - Ship (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(1,6,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service Fuel Cost - MotorCycle (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(1,7,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service  Non-Fuel Cost - Auto (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(1,1,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service  Non-Fuel Cost - Truck (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(1,2,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service  Non-Fuel Cost - Bus (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(1,3,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service  Non-Fuel Cost - Rail (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(1,4,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service  Non-Fuel Cost - Air (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(1,5,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service  Non-Fuel Cost - Ship (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(1,6,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Passenger Service  Non-Fuel Cost - MotorCycle (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(1,7,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Cost - Truck (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(2,2,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Cost - Rail (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(2,4,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Cost - Air (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(2,5,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Cost - Ship (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANCOST(2,6,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Energy Intensity - Truck (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(2,2,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Energy Intensity - Rail (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(2,4,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Energy Intensity - Air (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(2,5,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Energy Intensity - Ship (Btu/Veh-mi)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANEFF(2,6,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Fuel Cost - Truck (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(2,2,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Fuel Cost - Rail (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(2,4,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Fuel Cost - Air (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(2,5,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service Fuel Cost - Ship (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANFLCT(2,6,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service  Non-Fuel Cost - Truck (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(2,2,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service  Non-Fuel Cost - Rail (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(2,4,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service  Non-Fuel Cost - Air (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(2,5,J,L,M),J=1,NFUEL)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Transportation Sector: Freight Service  Non-Fuel Cost - Ship (90$/mile)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Oil,Gas,Coal,Electricity,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,  &
            (TRANNFCT(2,6,J,L,M),J=1,NFUEL)
	   END IF
	END DO

      END IF 	! New Transportation block
      
      
	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Biomass Production (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Modern,Old'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,ESIL1M(IBMASS,L,M), &
            ESIL2M(IBMASS,L,M)
	   ELSE
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,ESI1M(IBMASS,M), &
            ESI2M(IBMASS,M)
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Fuel Consumption for Hydrogen Production (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass,Electricity'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            DO I=1,NF
	         H2WORLD(I,M) = H2WORLD(I,M) + EDRIKLM(I,KH2,L,M)
	      END DO
	      H2WORLD(JHBMASS,M) = H2WORLD(JHBMASS,M)  &
                               + EDRIKLM(IBMASS,KH2,L,M)
	      H2WORLD(JHELCTRO,M) = H2WORLD(JHELCTRO,M) &
                             + ESHILM(JHELCTRO,L,M)*GHILM(JHELCTRO,M)
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(EDRIKLM(I,KH2,L,M),I=1,NF), &
                          EDRIKLM(IBMASS,KH2,L,M), &
                          ESHILM(JHELCTRO,L,M)*GHILM(JHELCTRO,M)
	   ELSE
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(H2WORLD(I,M),I=1,NF), &
            H2WORLD(JHBMASS,M),H2WORLD(JHELCTRO,M)
         END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Hydrogen Production by Fuel (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass,Electrolysis,Total'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         TOTAL=0.0
         GTOTAL=0.0
	   IF (L.NE.NLL) THEN
            TOTAL=TOTAL+ESHILM(INOIL,L,M)+ESHILM(INOIL+NH2,L,M) &
                 +ESHILM(INGAS,L,M)+ESHILM(INGAS+NH2,L,M) &
                 +ESHILM(INCOAL,L,M)+ESHILM(INCOAL+NH2,L,M) &
                 +ESHILM(4,L,M) &
                 +ESHILM(5,L,M)

            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ESHILM(INOIL,L,M)+ESHILM(INOIL+NH2,L,M), &
            ESHILM(INGAS,L,M)+ESHILM(INGAS+NH2,L,M), &
            ESHILM(INCOAL,L,M)+ESHILM(INCOAL+NH2,L,M), &
            (ESHILM(I,L,M),I=4,5),TOTAL
	   ELSE
            GTOTAL=GTOTAL+ESHIM(INOIL,M)+ESHIM(INOIL+NH2,M) &
                  +ESHIM(INGAS,M)+ESHIM(INGAS+NH2,M) &
                  +ESHIM(INCOAL,M)+ESHIM(INCOAL+NH2,M) &
                  +ESHIM(4,M) &
                  +ESHIM(5,M)

            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ESHIM(INOIL,M)+ESHIM(INOIL+NH2,M), &
            ESHIM(INGAS,M)+ESHIM(INGAS+NH2,M), &
            ESHIM(INCOAL,M)+ESHIM(INCOAL+NH2,M), &
            (ESHIM(I,M),I=4,5),GTOTAL
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Hydrogen Prices by Fuel Source  (1990 US $/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass,Electrolysis,Ave.Prc'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
           IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
           ((PHILM(J,L,M)*CVRT90),J=1,5),PJLM(JSH2,L,M)*CVRT90
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Hydrogen Production Efficiency by Source'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass,Electrolysis'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(1/GHILM(I,M),I=1,5)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Hydrogen Production Non-Energy Cost (1990 US $/GJ)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass,Electrolysis'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,((HHILM(I,M)*CVRT90),I=1,5)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'High GWP Equivalent Gas by Sector ( )'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Industrial,Transportation,Buildings'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,HGWPHFC(L,M),HGWPPFC(L,M), &
                          HGWPSF6(L,M)
	   ELSE
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,SUM(HGWPHFC(1:NL,M)), &
            SUM(HGWPPFC(1:NL,M)),SUM(HGWPSF6(1:NL,M))
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Electricity Transmission and Distribution Costs (1990 US Cents/KWh)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Res/Com,Industrial,Transport'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ((PUTDKM(K,M)*CVRT90*0.36),K=1,3)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Electricity Prices by Sector (1990 US Cents/KWh)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Res/Com,Industrial,Transport'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ((PUKLM(K,L,M)*CVRT90*0.36),K=1,3)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'End-Use Electricity Service Costs by Sector (1990 US Cents/KWh)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Res/Com,Industrial,Transport'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            ((PJKLM(4,K,L,M)*CVRT90*0.36),K=1,3)
	   END IF
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Fuel Consumption for Electricity Generation  (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,C.Oil,N.Gas,Coal,Biomass,Hydrogen'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
	      ELECH2G(M) = ELECH2G(M)+ESUILM(JUH2GEN,L,M)* &
                         GUILM(JUH2GEN,L,M)
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (EDRIKLM(I,1,L,M), I=1,INCOAL),EDRIKLM(IBMASS,1,L,M), &
             ESUILM(JUH2GEN,L,M)*GUILM(JUH2GEN,L,M)
	   ELSE
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (EDRIKM(I,M), I=1,INCOAL),EDRIKM(IBMASS,M),ELECH2G(M)
	   END IF	  
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Fuel Consumption for Electric Central Power Carbon Capture (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Coal,C.Oil,N.Gas'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
	   GTOTAL=0.0
	   IF (L.NE.NLL) THEN
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (ESUILM(I,L,M)*GUILM(I,L,M), I=NNU-3,NNU-1)
	   ELSE
	      !GLOBAL BASE ON US EFFICIENCY-CHANGE LATER
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (ESUIM(I,M)*GUILM(I,1,M), I=NNU-3,NNU-1)
	   END IF	  
      END DO


	IF (GenRelease .ne. 1) WRITE(100,*)
	IF (GenRelease .ne. 1) WRITE(100,*)'Electricity Generation by Fuel  (EJ/yr)'
      IF (GenRelease .ne. 1) WRITE(100,1001)'Region','Region#','Year','C.Oil','N.Gas','Coal', &
       'Nuclear','SolarPV','Hydro','Biomass','Coal_CO2Cap','Oil_CO2Cap', &
       'Gas_CO2Cap','H2Fcell','Fusion','Wind','Total', &
       'GrowthRate','Elec/cap'
      DO M=2,NM
  	   IYR = 1975 + (M-1)*NJUMP
         TOTAL=0.0
         GTOTAL=0.0
         TEMPTOT=0.0
         TEMPGTOT=0.0
	   DO  I = 1,NNU 
	       GTOTAL=GTOTAL+ESUIM(I,M)
	       TEMPGTOT=TEMPGTOT+ESUIM(I,M-1)
         END DO
	   IF (L.NE.NLL) THEN
	      DO I = 1,NNU
	         TOTAL=TOTAL+ESUILM(I,L,M)
	         TEMPTOT=TEMPTOT+ESUILM(I,L,M-1)
            END DO
            GR=(TOTAL/TEMPTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(ESUILM(I,L,M),I=1,NNU), &
            TOTAL,GR,TOTAL/ZLM(L,M)*1000
	   ELSE
            GR=(GTOTAL/TEMPGTOT)**(1.0/15.0)-1.0
            IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR,(ESUIM(I,M),I=1,NNU), &
            GTOTAL,GR,GTOTAL/ZLM(L,M)
         END IF 
      END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
      IF (GenRelease .ne. 1) WRITE(100,*)'Electricity Generation Prices (1990 Cents/KWh)'
	IF (GenRelease .ne. 1) WRITE(100,1001)'Region','Region#','Year','C.Oil','N.Gas','Coal', &
                     'Nuclear','SolarPV','Hydro','Biomass','Coal_Cap', &
                     'Oil_Cap','Gas_Cap','H2Fcell','Fusion','Wind', &
                     'Ave. Prc'
      FAC=CVRT90*0.36  !Conversion to Cents/KWh
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
	      IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            PSSUM(INOIL,1,L,M)*FAC, &
            PSSUM(INGAS,1,L,M)*FAC, &
            PSSUM(INCOAL,1,L,M)*FAC, &
            PUILM(4,L,M)*FAC,  &
            PUILM(5,L,M)*FAC,  &
            PUILM(6,L,M)*FAC,  &
            PSSUM(INCOAL,3,L,M)*FAC, &
            PSSUM(INCOAL,2,L,M)*FAC, &
            PSSUM(INOIL,2,L,M)*FAC, &
            PSSUM(INGAS,2,L,M)*FAC, &
            PUILM(7,L,M)*FAC, &
            PUILM(8,L,M)*FAC, &
            PUILM(9,L,M)*FAC, &
            PJLM(4,L,M)*FAC 
	   END IF   !no global prices
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
      IF (GenRelease .ne. 1) WRITE(100,*)'Electricity Generation Efficiency (Gen/Fuel %)'
	IF (GenRelease .ne. 1) WRITE(100,1001)'Region','Region#','Year','C.Oil','N.Gas','Coal', &
                     'Nuclear','Solar','Hydro','Biomass','Coal_Cap', &
                     'Oil_Cap','Gas_Cap','H2Fcell','Fusion','Wind'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
	      IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (1/GUILM(I,L,M),I=1,NNU)
	   END IF   !no global efficiency
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
      IF (GenRelease .ne. 1) WRITE(100,*)'Fuel Costs for Electricity Generation (1990 Cents/KWh)'
	IF (GenRelease .ne. 1) WRITE(100,1001)'Region','Region#','Year','C.Oil','N.Gas','Coal', &
                     'Nuclear','SolarPV','Hydro','Biomass','Coal_Cap', &
                     'Oil_Cap','Gas_Cap','H2Fcell','Fusion','Wind'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
	      IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (PSSUM(INOIL,1,L,M)-HUILM(INOIL,L,M))*FAC, &
            (PSSUM(INGAS,1,L,M)-HUILM(INGAS,L,M))*FAC, &
            (PSSUM(INCOAL,1,L,M)-HUILM(INCOAL,L,M))*FAC, &
            (PUILM(4,L,M)-HUILM(4,L,M))*FAC,  &
            (PUILM(5,L,M)-HUILM(5,L,M))*FAC,  &
            (PUILM(6,L,M)-HUILM(6,L,M))*FAC,  &
            (PSSUM(INCOAL,3,L,M)-HUILM(JUBMASS,L,M))*FAC, &
            (PSSUM(INCOAL,2,L,M)-HUILM(JUCSCRUB,L,M))*FAC, &
            (PSSUM(INOIL,2,L,M)-HUILM(JUOSCRUB,L,M))*FAC, &
            (PSSUM(INGAS,2,L,M)-HUILM(JUGSCRUB,L,M))*FAC, &
            (PUILM(7,L,M)-HUILM(JUH2GEN,L,M))*FAC, &
            (PUILM(8,L,M)-HUILM(JUFUSION,L,M))*FAC,  &
            (PUILM(9,L,M)-HUILM(JUWIND,L,M))*FAC
	   END IF   !no global fuel costs
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*)
      IF (GenRelease .ne. 1) WRITE(100,*)'Non-Fuel Costs for Electricity Generation (1990 Cents/KWh)'
	IF (GenRelease .ne. 1) WRITE(100,1001)'Region','Region#','Year','C.Oil','N.Gas','Coal', &
                     'Nuclear','Solar','Hydro','Biomass','Coal_Cap', &
                     'Oil_Cap','Gas_Cap','H2Fcell','Fusion','Wind'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.NE.NLL) THEN
	      IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(L),L,IYR, &
            (HUILM(I,L,M)*FAC,I=1,NNU)
	   END IF   !no global non-fuel costs
	END DO

!     ******** AGRICUTURAL OUTPUTS FOR OLD AGLU MODEL*********

IF (AGMODEL.EQ.0) THEN	! Only do these outputs for old AgLU model

	IF (GenRelease .ne. 1) WRITE(100,*) 
	IF (GenRelease .ne. 1) WRITE(100,*) 'Agricultural and Forest Production'
	IF (GenRelease .ne. 1) WRITE(100,2000) 'Region,Region#,Year,Crop(1000ton),Livestock(1000ton),Biomass(EJ),Forest(1000CM)'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.LE.NLL) THEN
         IF (GenRelease .ne. 1) WRITE(100,854) REGNAMES(L),L,IYR,(QSJLM(J,L,M),J=1,NJ)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*) 
	IF (GenRelease .ne. 1) WRITE(100,*) 'Agricultural and Forest Demand'
 2000 FORMAT(A) 
	IF (GenRelease .ne. 1) WRITE(100,2000) 'Region,Region#,Year,FinalCrop(1000ton),FeedCrop(1000ton),Livestock(1000ton),', &
      'Forest(1000CM),FuelWood(1000CM)'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.LE.NLL) THEN
         IF (GenRelease .ne. 1) WRITE(100,854) REGNAMES(L),L,IYR, &
         (QDJLM(JCROP,L,M)-QDCB(L,M)), &
         QDCB(L,M),QDJLM(JLSTOCK,L,M),QDJLM(JFOREST,L,M), &
         QDJLM(JBMASS,L,M)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*) 
	IF (GenRelease .ne. 1) WRITE(100,*) 'Prices Paid'
	IF (GenRelease .ne. 1) WRITE(100,2000) 'Region,Region#,Year,Crop($/1000ton),Livestock($/1000ton),',&
      'Biomass($/1000CM),Forest($/1000CM)'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.LT.NLL) THEN
         IF (GenRelease .ne. 1) WRITE(100,854) REGNAMES(L),L,IYR, &
         (PPJLM(J,L,M),J=1,NJ)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*) 
	IF (GenRelease .ne. 1) WRITE(100,*) 'Land Use Change (1000 Ha)'
	IF (GenRelease .ne. 1) WRITE(100,*) 'Region,Region#,Year,Crop,Pasture,Biomass,NewForest,OldForest,Unmanaged'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.LE.NLL) THEN
         IF (GenRelease .ne. 1) WRITE(100,854) REGNAMES(L),L,IYR, &
         (QLJLM(J,L,M),J=1,NJ), &
         QLFOREST(L,M),QLUNMAN(L,M)
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*) 
	IF (GenRelease .ne. 1) WRITE(100,*) 'Per Capita Information (per cap)'
	IF (GenRelease .ne. 1) WRITE(100,*) 'Region,Region#,Year,CropProd,LivestockPr,CropCons'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.LE.NLL) THEN
         IF (GenRelease .ne. 1) WRITE(100,854) REGNAMES(L),L,IYR, &
         (QSJLM(JCROP,L,M)/ZLM(L,M)),(QSJLM(JLSTOCK,L,M)/ZLM(L,M)), &
         ((QDJLM(JCROP,L,M)-QDCB(L,M))/ZLM(L,M))
	   END IF
	END DO

	IF (GenRelease .ne. 1) WRITE(100,*) 
	IF (GenRelease .ne. 1) WRITE(100,*) 'Terrestrial Carbon Emission (TgC)'
	IF (GenRelease .ne. 1) WRITE(100,*) 'Region,Region#,Year,Carbon Emission'
      DO M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   IF (L.LE.NLL) THEN
         IF (GenRelease .ne. 1) WRITE(100,854) REGNAMES(L),L,IYR,CARBLAND(L,M)
	   END IF
	END DO


END IF	! End Ag sector outputs

!     For calibration purpose only.  IF (GenRelease .ne. 1) WRITE out base year aggregate
!     energy price by end-use sector.
	IF (L.EQ.NLL) THEN
        IF (GenRelease .ne. 1) WRITE(100,*)
        IF (GenRelease .ne. 1) WRITE(100,*)'****  FOR CALIBRATION ONLY  ****'
        IF (GenRelease .ne. 1) WRITE(100,*)'Base Year Aggregate Energy Price by Sector (1975 US $/GJ)'
        IF (GenRelease .ne. 1) WRITE(100,*)'Region,Region#,Year,Res/Com,Industrial,Transport'
        IYR = 1975
	  DO LL=1,NLL
	    IF (LL.NE.NLL) THEN
             IF (GenRelease .ne. 1) WRITE(100,854)REGNAMES(LL),LL,IYR, &
             (BPKL(K,LL),K=1,3)
	    END IF
        END DO
      END IF


	IF (GenRelease .ne. 1) WRITE(100,*)

  850 CONTINUE !END REGION DO LOOP


!     WRITE LIST OF INPUT FILES
	IF (GenRelease .ne. 1) WRITE(100,*)
      IF (GenRelease .ne. 1) WRITE(100,1000)CASENAME
	IF (GenRelease .ne. 1) WRITE(100,1000)CASENOTE
      IF (GenRelease .ne. 1) WRITE(100,1000)AGNAME
	IF (GenRelease .ne. 1) WRITE(100,1000)AGNOTE

	DO IFILE=1,INFILES
         IF (GenRelease .ne. 1) WRITE(100,1000)FILES(IFILE)
	END DO
     
	CLOSE(100)

!     WRITE REGIONAL CARBON TAX FOR RUN CONTROL MACRO
      OPEN (200,FILE='CTAX.CSV')
  852 FORMAT ('Region,','Region#,','Year,','CarbTax,')
      IF (GenRelease .ne. 1) WRITE(200,852)
      DO L=1,NL
         DO M=2,NM
            IYR = 1975 + (M-1)*NJUMP
            IF (GenRelease .ne. 1) WRITE(200,854)REGNAMES(L),L,IYR,TAXLM(L,M)
         END DO
	END DO
      CLOSE(200)


!     WRITE OG COEFFS FOR CALIBRATION/INSPECTION
      OPEN (200,FILE='OG_COEFFS.CSV')

	  DO igas = 1, NOGMax
	  	IF (GenRelease .ne. 1) WRITE(200,*) 
		IF (GenRelease .ne. 1) WRITE(200,*) OGLabel(igas)
	  	IF (GenRelease .ne. 1) WRITE(200,1003) ' ',(OGSrcLabel(igas,isrc), isrc = 1, NOGSrcMax)

		DO L=1,NL
			IF (GenRelease .ne. 1) WRITE(200,1056) L, (OGCOEF(igas,isrc,L), isrc = 1, NOGSrcMax)
		END DO
	  END DO

      IF (GenRelease .ne. 1) CLOSE(200)
 1056 FORMAT(I2,',',30(F12.8,','))

      RETURN
      END
