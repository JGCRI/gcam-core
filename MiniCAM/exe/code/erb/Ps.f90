!**********************************************************************
!
      SUBROUTINE   PSPS
!**********************************************************************
!
!        -- THE SECONDARY ENERGY PRICE PREPROCESSOR --
!
! THIS SUBROUTINE TAKES THE PRICES FOR PRIMARY ENERGY BY REGION AND
! FUEL TYPE AND CONVERTS THEM INTO THE PRICES OF SECONDARY FUELS,
! ELECTRICITY, AND ENERGY SERVICES.  IT COMPUTES BASE GNP FROM DEMO-
! GRAPHIC AND LABOR PRODUCTIVITY DATA AND THEN ADJUSTS FINAL GNP
! FOR ENERGY PRICE EFFECTS.
!
! MODEL VERSION:  A.31.07.84
!
! INTEGER INPUTS: NF,NI,NJ,NKL,NKMAX,NM,NU
! REAL INPUTS:    BSUILM,GIJ,GJ,GJK,GNP,GUILM,HIJ,HJ,NK,HUILM,HYDRO,
!                 PILM,RUI,RY,SJKLP,TXJKLM
!
! REAL OUTPUTS:   PJKLM,PJLM,PKLM,PUILM,SUIL,YLM
!
! LOCAL VARIABLES:PS,T,SUM
! LOCAL INTEGERS: NK,NNF
!
! SUBROUTINES CALLED:  NONE
!
! WRITTEN BY:
!    JAE EDMONDS                      LATEST REVISION:
!    31 JULY 1984                      1 SEPT 88 TO INCLUDE COMMON
!                                      14 AUGUST 89 TO CORRECT PS/BPSL     
!                                      21 AUGUST 89 TO CHANGE TXJKL TO 
!                                         TXJKLM
!                                       7 DECEMBER 89 TO INCORPORATE 
!                                       UTILITY CONSUMPTION TAX TXUILM
!                                       14 DECEMBER 89 TO CHANGE TXJKLM 
!                                       TO AN ADDITIVE TAX VICE A RATE
!
!     2/2/98  Added pricing of hydrogen production  maw
!***********************************************************************
!
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      REAL*8, SAVE:: Elec_Price(NLPMax)	! Electric price before elec capacity limitation. Used for H2 generation
      REAL*8 SUIL_Temp(NUP)
!

! -- BEGIN REGIONAL DO LOOP
      h2frac(l)=0.0  !initialize at zero
!     Test to see if carbon scrubbing policy is on
      IF(ICARBOUT(L,1).NE.0 .AND. M.GT.ICARBOUT(L,1)) THEN
	   ISCRUB=1
!        Grandfather in existing plants, so a fraction remains until
!        lifetime has been reached since policy start date
         XMAXL=3  !For now, hardwire lifetime at 3 periods
	   POLYRSON = M-ICARBOUT(L,1)
         GFATHR = MAX(0.d0,(1.0-POLYRSON/XMAXL))
!        see if conversion to h2 is on
         IF(ICARBOUT(L,2).NE.0 .AND. M.GT.ICARBOUT(L,2)) THEN
	      IGOH2=1
	      POLYRSON = M-ICARBOUT(L,2)
            H2FRAC(L) = MIN(1.d0,POLYRSON/XMAXL)
	   ELSE
	      IGOH2=0
	   END IF
	ELSE
	   ISCRUB=0
	END IF

      MM1=M-1 
      MM2=M-2
      NK=NKL(L) !number of final demand sectors

!     Calculate refined prices for fossil fuels
      DO 50 J=1, NF
         I=J
         PJLM(J,L,M)=PILM(I,L,M)*GIJ(I) + HIJ(I)
   50 CONTINUE

!     Calculate refined price for biomass

      J=JSBMASS
	I=IBMASS
      PJLM(J,L,M)=PILM(I,L,M)*GIJ(J)+HIJ(J)


!---- COMPUTE HYDROGEN PRODUCTION FUEL SHARES AND PRICE  ------

!     For hydrogen from fossil fuels, add a carbon sequestered subsector
!  
! H2 Feedstock prices changed to regional primary prices, not refined fuel prices. sjs -- 09/01

      DO J=INOIL,INCOAL !Loop over fossil fuels 	   
        PSSH(J,1) = (PILM(J,L,M)+TXUILM(J,L,M)) &
        *GHILM(J,M)+HHILM(J,M)

!       CO2 scrubber (after bio and electrolytic in input table)
        JH = J + NH2
	  CDISPCST = CARBDISP(L,M)*TXUILM0(J,L,M)/100.0
        PSSH(J,2) = (PILM(J,L,M)+TXUILM(J,L,M)*(1.0-REMFRAC(2,L))  &
        +CDISPCST)*GHILM(JH,M) + HHILM(JH,M) 

!     Compute share and avg. price using logit formula

	  SUMSS=0.0
        DO ISS=1,2
           SSHIL(J,ISS,L) = BSSHILM(J,ISS,L,M)*PSSH(J,ISS)**RUISS(J,ISS)
	     SUMSS = SUMSS + SSHIL(J,ISS,L)
	  END DO

	  IF(TXUILM(J,L,M).EQ.0.0 .AND. ISCRUB.EQ.0 &
           .AND. BSSHILM(J,1,L,M) .NE. 0.0) THEN
	    ISS=2  !No share if tax is zero
	    SUMSS = SUMSS - SSHIL(J,ISS,L)
          SSHIL(J,ISS,L) = 0.0
	  ELSE IF(ISCRUB.EQ.1) THEN
!	    ISS=1
!         No Unscrubbed once scrub policy begins at all
!          SUMSS = SUMSS - SSHIL(J,ISS,L)
!	    SSHIL(J,ISS,L) = 0.0
        END IF
               
	  PHILM(J,L,M) = 0.0
	  DO ISS=1,2
	     SSHIL(J,ISS,L) = SSHIL(J,ISS,L)/SUMSS
	     PHILM(J,L,M) = PHILM(J,L,M) + SSHIL(J,ISS,L)*PSSH(J,ISS)
	  END DO   
	END DO  !Loop over fossil fuels


!     Hydrogen from Biomass
	IN=INBMASS 
	JH=JHBMASS
	JS=JSBMASS
	PHILM(JH,L,M)=PILM(IBMASS,L,M)*GHILM(JH,M)+HHILM(JH,M)

!     Electrolytic production of H2 (Is a simultaneity problem. Demand part Solved in Dd.for.)
!     Problem with electricity price here -- is not defined yet
!        In practice, this is price from last iteration, so ok

      IF (Elec_Price(L) .eq. 0) Elec_Price(L) = 5.0 ! Aribtrary minimum price to start iteration
      
      JH=JHELCTRO

!     The electricity price is considered as an opportunity cost
      PHILM(JH,L,M) = Elec_Price(L)*GHILM(JH,M) + HHILM(JH,M)

! Add "New" H2 Technologies. sjs -- 08/02
! These are added after the scrubbing sectors

      DO I = NH2 + (NNH2-NH2)+1,NH2 + (NNH2-NH2) + NH2_New
         PHILM(I,L,M) = HHILM(I,M)
         RHI(I) = RHI(JHELCTRO)
      END DO

!     Compute hydrogen shares, NH2=5 number of hydrogen production sector
      SUMH2=0.D0
      DO I=1,NH2
         SHIL(I,L)=BSHILM(I,M)*PHILM(I,L,M)**RHI(I)
         SUMH2 = SUMH2 + SHIL(I,L)
      END DO
      
! Calc shares for "new technologies". sjs -- 08/02
! No B coefficient for these technologies, can use price to adjust
      DO I = NH2 + (NNH2-NH2)+1,NH2 + (NNH2-NH2) +NH2_New
		SHIL(I,L)= PHILM(I,L,M)**RHI(NH2)
		SUMH2 = SUMH2 + SHIL(I,L)
      END DO

!     Compute hydrogen price
      PJLM(JSH2,L,M)=0.0
      DO I=1,NH2
         SHIL(I,L)=SHIL(I,L)/SUMH2
         PJLM(JSH2,L,M)=PJLM(JSH2,L,M)+SHIL(I,L)*PHILM(I,L,M)
      END DO

! Add prices for "new technologies". sjs -- 08/02
      DO I = NH2 + (NNH2-NH2)+1,NH2 + (NNH2-NH2) +NH2_New
         SHIL(I,L)=SHIL(I,L)/SUMH2
         PJLM(JSH2,L,M)=PJLM(JSH2,L,M)+SHIL(I,L)*PHILM(I,L,M)
      END DO
      PJLM(JSH2,L,M)=PJLM(JSH2,L,M)

      IF (M .eq. 22) THEN
         write(*,*) L,"PHILM: ",P_Elec,PJLM(NJ,L,M),GHILM(JH,M), HHILM(JH,M)
         write(*,*) "      e- P's: ", &
            PUILM(1,L,M),PUILM(2,L,M),PUILM(3,L,M),PUILM(JUWIND+1,L,M)
          write(*,*)  "    Ag H Price: ", PJLM(JSH2,L,M)
          write(*,*)  "      H Prices: ",(PHILM(I,L,M),I=1,NH2)
          write(*,*)  "      H Shares: ",(SHIL(I,L),I=1,NH2)
      END IF

!     Compute shares among fossil scrubbed vs unscrubbed subsectors

	DO J=INOIL,INCOAL
         SHARETOT=SHIL(J,L)
	   SHIL(J,L) = SHARETOT*SSHIL(J,1,L)
         SHIL(J+NH2,L) = SHARETOT*SSHIL(J,2,L)
	END DO

!----- END HYDROGEN PRICE AND SHARES  ----------




! COMPUTE THE PRICE OF ELECTRICITY
!
!  -- COMPUTE ELECTRICITY GENERATION PRICES FOR ALL COMPETING FUELS
!
! Electricity fuel prices changed to regional primary prices, not refined fuel prices. sjs -- 09/01

!CCC  Oil **************************************************
!     Share out oil and gas between scrubbed and non-scrubbed

      J=INOIL
      PSSU(J,1) = (PILM(J,L,M)*PAUIL(J,L)+TXUILM(J,L,M)) &
        *GUILM(J,L,M)+HUILM(J,L,M)
!     Store electricity price by fuel, technology, region, and period      
	PSSUM(J,1,L,M)=PSSU(J,1)

!     CO2 scrubber
      JU=JUOSCRUB
	cdispcst = CARBDISP(L,M)*TXUILM0(J,L,M)/100.0
      PSSU(J,2) = (PILM(J,L,M)*PAUIL(J,L)+TXUILM(J,L,M)*  &
        (1.0-REMFRAC(2,L))+cdispcst)*GUILM(JU,L,M) + HUILM(JU,L,M)
!     Store electricity price by fuel, technology, region, and period      
	PSSUM(J,2,L,M)=PSSU(J,2)
!     Compute share among oil and avg. price using logit formula

	SUMSS=0.0
      DO ISS=1,2
         SSUIL(J,ISS,L) = BSSUILM(J,ISS,L,M)*PSSU(J,ISS)**RUISS(J,ISS)
	   SUMSS = SUMSS + SSUIL(J,ISS,L)
	END DO

	ISS=2  !No share if tax is zero
	IF(TXUILM(J,L,M).EQ.0.0 .AND. ISCRUB.EQ.0) THEN
	   SUMSS = SUMSS - SSUIL(J,ISS,L)
         SSUIL(J,ISS,L) = 0.0
	ELSE IF(ISCRUB.EQ.1)  THEN !Fixed share of unscrubbed if policy on
	   ISS=1
!        Fix supply, and estimate share for price (correct share in DD)
         SUMSS = SUMSS - SSUIL(J,ISS,L)
	   ESUILM(J,L,M) = ESUILM(J,L,ICARBOUT(L,1))*GFATHR    
	   SSUIL(J,ISS,L) =SSUIL(J,ISS,L)*GFATHR
         SUMSS = SUMSS + SSUIL(J,ISS,L)
      END IF
               
	PUILM(J,L,M) = 0.0
	DO ISS=1,2
	   SSUIL(J,ISS,L) = SSUIL(J,ISS,L)/SUMSS
	   PUILM(J,L,M) = PUILM(J,L,M) + SSUIL(J,ISS,L)*PSSU(J,ISS)
	END DO   

!CCC  Gas CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      J=INGAS
      PSSU(J,1) = (PILM(J,L,M)*PAUIL(J,L)+TXUILM(J,L,M)) &
        *GUILM(J,L,M)+HUILM(J,L,M)
!     Store electricity price by fuel, technology, region, and period      
	PSSUM(J,1,L,M)=PSSU(J,1)

!     CO2 scrubber
      JU=JUGSCRUB
	cdispcst = CARBDISP(L,M)*TXUILM0(J,L,M)/100.0
      PSSU(J,2) = (PILM(J,L,M)*PAUIL(J,L)+TXUILM(J,L,M)*  &
        (1.0-REMFRAC(2,L))+cdispcst)*GUILM(JU,L,M) + HUILM(JU,L,M)
!     Store electricity price by fuel, technology, region, and period      
	PSSUM(J,2,L,M)=PSSU(J,2)

!     Compute share among gas and avg. price using logit formula

	SUMSS=0.0
      DO ISS=1,2
         SSUIL(J,ISS,L) = BSSUILM(J,ISS,L,M)*PSSU(J,ISS)**RUISS(J,ISS)
	   SUMSS = SUMSS + SSUIL(J,ISS,L)
	END DO

	ISS=2  !No share if tax is zero
	IF(TXUILM(J,L,M).EQ.0.0 .AND. ISCRUB.EQ.0) THEN
	   SUMSS = SUMSS - SSUIL(J,ISS,L)
         SSUIL(J,ISS,L) = 0.0
	ELSE IF(ISCRUB.EQ.1) THEN !Fixed share of unscrubbed if policy on
	   ISS=1
!        Fix supply, and estimate share for price (correct share in DD)
         SUMSS = SUMSS - SSUIL(J,ISS,L)
	   ESUILM(J,L,M) = ESUILM(J,L,ICARBOUT(L,1))*GFATHR    
	   SSUIL(J,ISS,L) =SSUIL(J,ISS,L)*GFATHR
         SUMSS = SUMSS + SSUIL(J,ISS,L)          
      END IF
               
	PUILM(J,L,M) = 0.0
	DO ISS=1,2
	   SSUIL(J,ISS,L) = SSUIL(J,ISS,L)/SUMSS
	   PUILM(J,L,M) = PUILM(J,L,M) + SSUIL(J,ISS,L)*PSSU(J,ISS)
	END DO   

!CCC  Solids **************************************************

!     For solids, share out between coal and biomass
!     There is only one electric price for solid fuel that includes 
!     coal and biomass.
	J=INCOAL
      PSSU(J,1) = (PILM(J,L,M)*PAUIL(J,L)+TXUILM(J,L,M)) &
        *GUILM(J,L,M)+HUILM(J,L,M)
!     Store electricity price by fuel, technology, region, and period      
	PSSUM(J,1,L,M)=PSSU(J,1)

!     CO2 scrubber
      JU=JUCSCRUB
	cdispcst = CARBDISP(L,M)*TXUILM0(J,L,M)/100.0
      PSSU(J,2) = (PILM(J,L,M)*PAUIL(J,L)+TXUILM(J,L,M)*  &
        (1.0-REMFRAC(2,L))+cdispcst)*GUILM(JU,L,M) + HUILM(JU,L,M)
!     Store electricity price by fuel, technology, region, and period      
	PSSUM(J,2,L,M)=PSSU(J,2)

	JU=JUBMASS
	IN=INBMASS 
	JS=JSBMASS
	PSSU(J,3) = (PILM(IBMASS,L,M)+TXUILM(JU,L,M))*PAUIL(IN,L) &
        *GUILM(JU,L,M)+HUILM(JU,L,M)
!     Store electricity price by fuel, technology, region, and period      
	PSSUM(J,3,L,M)=PSSU(J,3)

!     Compute share among solids and avg. price using logit formula

	SUMSS=0.0
      DO ISS=1,3
         SSUIL(J,ISS,L) = BSSUILM(J,ISS,L,M)*PSSU(J,ISS)**RUISS(J,ISS)
	   SUMSS = SUMSS + SSUIL(J,ISS,L)
	END DO

	ISS=2  !No share if tax is zero
	IF(TXUILM(J,L,M).EQ.0.0 .AND. ISCRUB.EQ.0) THEN
	   SUMSS = SUMSS - SSUIL(J,ISS,L)
         SSUIL(J,ISS,L) = 0.0
	ELSE IF(ISCRUB.EQ.1) THEN !Fixed share of unscrubbed if policy on
	   ISS=1
!        Fix supply, and estimate share for price (correct share in DD)
         SUMSS = SUMSS - SSUIL(J,ISS,L)
	   ESUILM(J,L,M) = ESUILM(J,L,ICARBOUT(L,1))*GFATHR    
	   SSUIL(J,ISS,L) =SSUIL(J,ISS,L)*GFATHR
         SUMSS = SUMSS + SSUIL(J,ISS,L)          
      END IF

	PUILM(J,L,M) = 0.0
	DO ISS=1,3
         SSUIL(J,ISS,L) = SSUIL(J,ISS,L)/SUMSS
	   PUILM(J,L,M) = PUILM(J,L,M) + SSUIL(J,ISS,L)*PSSU(J,ISS)
	END DO   
! *** End solids ***********************************

! *** Nuclear Electricity Cost *********************
      J=JUNUC
      PUILM(J,L,M)=(PILM(J,L,M)+TXUILM(J,L,M))*GUILM(J,L,M) &
                  +HUILM(J,L,M)

! *** Solar PV Electricity Cost *********************
      I=5
      JU=JUSOLPV
!     Solar Photovoltaic Cost; pjlm = 0.0 (no input fuel)
      PUILM(I,L,M) = (0.0+TXUILM(I,L,M)) &
        *GUILM(JU,L,M)+HUILM(JU,L,M)

! *** Wind Electricity Cost *********************
!     Wind Cost; utility index is 9 but use same tax as solar for now
      I=5
      JU=JUWIND
      PUILM(9,L,M) = (0.0+TXUILM(I,L,M)) &
        *GUILM(JU,L,M)+HUILM(JU,L,M)

! *** Hydro Electricity Cost *********************
!     No longer using HYDRO4L, but HUILM for hydro cost
!     PILM(NI,L,M)=0.0 see Anteper.for
!     SHK  5/1/00
      I=JUHYDRO
      PUILM(I,L,M)=(PILM(I,L,M)+TXUILM(I,L,M))*GUILM(I,L,M) &
                  +HUILM(I,L,M)

! *** Hydrogen Fuel Cell Electricity Cost *********************
!     Electricity cost from hydrogen fuel cells using hydrogen fuel price
!     calculated from above.  There are no taxes on electricity generated
!     from fuel cells.

      I=7
	JU=JUH2GEN 
	JS=JSH2
	PUILM(I,L,M) = PJLM(JS,L,M)*GUILM(JU,L,M)+HUILM(JU,L,M)

       IF (M .eq. 22) then
         write(*,*) "PUILM(JUH2GEN): ", &
            PUILM(I,L,M),PJLM(JS,L,M),GUILM(JU,L,M),HUILM(JU,L,M)
       end if
! *** Fusion Power Electricity Cost *********************
!     Electricity cost from fusion power plants.  Fusion is treated as a
!     renewable technology and therefore, fuel price is null and efficiency 
!     is 100 percent.  Consider non-energy costs only.  shk 9/17/98

      I=8
      JU=JUFUSION
	PILM(I,L,M) = 0.0  !set here for now but set as input later
	TXUILM(I,L,M) = 0.0  !set here for now but set as input later
      PUILM(I,L,M)=(PILM(I,L,M)+TXUILM(I,L,M))*GUILM(JU,L,M) &
                   +HUILM(JU,L,M)

! *** Electricity From Solar/Wind Storage Cost ********************* !SWStor. sjs 01/01
! Storage is automatically done using H2 production, but can add an additional exogenous storage capacity here

! Do the usual logistic split here
      S_Wind =  PUILM(9,L,M)**(-3)/(PUILM(9,L,M)**(-3) + PUILM(5,L,M)**(-3))
      S_Solar =  PUILM(5,L,M)**(-3)/(PUILM(9,L,M)**(-3) + PUILM(5,L,M)**(-3))
      PUILM(NU+1,L,M) =  PUILM(9,L,M)*S_Wind + PUILM(5,L,M)*S_Solar	! Weighted price of Solar & wind
      
      PUILM(NU+1,L,M) = PUILM(NU+1,L,M) + HUILM(JUWIND+1,L,M) 	! Add non-fuel cost of storage to the cost of solar & wind power

! Price for next option
      PUILM(NU+2,L,M) = HUILM(JUWIND+2,L,M) 

      
! PUILM Indexed as:
! 1 - 3: oil, gas, coal 
! 4 Nuc
! 5 Solar
! 6 Hydro
! 7 H2
! 8 Fusion
! 9 Wind

!
!  -- COMPUTE THE ELECTRICITY FUEL SHARES AND PRICE
!     Skip hydro since fixed production
!     Loop through competing utility types
!     Determine subsector shares below
      SUMElec=0.D0
      DO I=1,NU
         IF (I.NE.NI) THEN
           SUIL(I,L)=BSUILM(I,L,M)*PUILM(I,L,M)**RUI(I)
           SUMElec=SUMElec + SUIL(I,L)
         END IF
      END DO
         
! If present, then do share for sol/wind storage. Use solar parameters.
      IF (NStype .ge. 1) THEN
        SUIL(NU+1,L)=BSUILM(5,L,M)*PUILM(NU+1,L,M)**RUI(5)
        SUMElec=SUMElec + SUIL(NU+1,L)
      END IF
    
      IF (NStype .ge. 2) THEN	! next option. Probably Sat Solar.
        SUIL(NU+2,L)=BSUILM(5,L,M)*PUILM(NU+2,L,M)**RUI(5)
        SUMElec=SUMElec + SUIL(NU+2,L)
      END IF

! -- SUIL here dimentioned same as PUILM, but is re-ordered below
     
!     NJ=4 Number of secondary energy types, use as electricity price
!     NI=6 Represents hydro, whose production is fixed
!     NU=9, is the number of competing utility type
!     Calculate electricity price starting with hydro and readjust
!     generation shares because of fixed production from hydro.
!     NJ=4 Used as index for electricity
!     Add share of hydroelectricity price to the initial value of
!     the average electricity price.

!      original hydro share
!      SUIL(NI,L)=HYDRO(5,L)
      I=JUHYDRO
      IF (BSUILM(I,L,M) .gt. 1) BSUILM(I,L,M) = 0.9 	! Check for bad input
      SUIL(I,L)=BSUILM(I,L,M)			! Input base share, not actual share, used to set price
      PJLM(NJ,L,M)=SUIL(I,L)*PUILM(I,L,M)	! Hydro's contribution to total e- price

      DO 90 I=1,NU+NStype 	!Include all tech's. sjs 01/01
         IF (I.NE.NI) THEN
            SUIL(I,L)=(1.D0-SUIL(JUHYDRO,L))*SUIL(I,L)/SUMElec
            PJLM(NJ,L,M)=PJLM(NJ,L,M)+SUIL(I,L)*PUILM(I,L,M)
         END IF
  90  CONTINUE


!     Change index for fuel cell,fusion, wind, etc.
!     Must do this in reverse order, or else will overwrite something

 	IF (NStype .ge. 2) SUIL(JUWIND+2,L)=SUIL(11,L)	!Sat Solar. sjs 02/01
	IF (NStype .ge. 1) SUIL(JUWIND+1,L)=SUIL(10,L)	!SWStor. sjs 01/01
	SUIL(JUWIND,L)=SUIL(9,L)
	SUIL(JUFUSION,L)=SUIL(8,L)
        SUIL(JUH2GEN,L)=SUIL(7,L)
	
	SUIL(7,L) = 0.0		! Zero out biomass (old H2 gen SUIL) and scrubbed sectors
	SUIL(JUOSCRUB,L) = 0	! so as not to mess up capacity check which is expecting sum shares = 1
	SUIL(JUGSCRUB,L) = 0	! 
	SUIL(JUCSCRUB,L) = 0	! These are set below after capacity check (and corresponding base share reduced, so still will sum to 1)
	
	SUIL_Temp(:) = SUIL(:,L)	! Save capacity value with no limitations
	
! First iteration (IDUM = 1) gives price with no capacity limitations. sjs--02/01
! Second iteration, after CapCheck is called, gives the end-user price including capacity limitations
       DO IDUM = 1,2  
         PJLM(NJ,L,M)=0
         DO I=1,6	! loop through hydro
            PJLM(NJ,L,M)=PJLM(NJ,L,M)+SUIL(I,L)*PUILM(I,L,M)
         END DO

! Need to do these by hand because of change in share indexing
         PJLM(NJ,L,M)= PJLM(NJ,L,M) + SUIL(JUH2GEN,L)*PUILM(7,L,M)
         PJLM(NJ,L,M)= PJLM(NJ,L,M) + SUIL(JUFUSION,L)*PUILM(8,L,M)
         PJLM(NJ,L,M)= PJLM(NJ,L,M) + SUIL(JUWIND,L)*PUILM(9,L,M)
         IF (NStype .ge. 1) PJLM(NJ,L,M)= PJLM(NJ,L,M) + SUIL(JUWIND+1,L)*PUILM(10,L,M)
         IF (NStype .ge. 2) PJLM(NJ,L,M)= PJLM(NJ,L,M) + SUIL(JUWIND+2,L)*PUILM(11,L,M)
 
 
! Loop that allows electric price for H2 generation to be set as price before capacity limitation
         IF (IDUM .eq. 1) THEN
!          This price means are using any elec source to generate H2, which might be reasonable in some scenarios
           Elec_Price(L) = PJLM(NJ,L,M)	! Save the electric price without capacity limitations.
           				! This is the price appropriate for H2 generation (which can be done anytime and is not subject to capacity limitations)
           				! This will, therefore, also supply automatic electricity storage
					! This cooresponds to NH2electPtype = 1 (and is also the default method if not set)
					
           IF (NH2electPtype .eq. 2) THEN
! The (NH2electPtype=2) option assumes that H2 is produced only from solar and wind
! This is more reasonable if there is a strict capacity limit on solar and wind and these sources are cheep
!    so H2 is generated preferentially from these. This does not work out rigoursly (some elec prob also comes from other sources)
!
! Also, while the method above and the orignal method seemed to work fine (both used price lagged by one iteration) 
!    this might also speed convergence somewhat since the price below is static
 
             S_Wind =  PUILM(9,L,M)**(-3.)/(PUILM(9,L,M)**(-3.) + PUILM(5,L,M)**(-3.))	! Logistic split
             S_Solar =  PUILM(5,L,M)**(-3.)/(PUILM(9,L,M)**(-3.) + PUILM(5,L,M)**(-3.))
             Elec_Price(L) = PUILM(9,L,M)*S_Wind + PUILM(5,L,M)*S_Solar	! Weighted price of Solar & wind
           END IF
            			
           Call CapCheck(0.0)		! Now adjust capacities if any have reached maximums
        END IF                    	! NOTE: Need to call this after indexing change               
      END DO	!IDUM Loop

      SUIL(:,L) = SUIL_Temp(:)	! Restore capacity value to that with no limitations 
      				! (will be limited in dd.for after electrolytic H2 contribution is calculated)


!     Compute shares among oil and gas subsectors
      J=INOIL
      SHARETOT=SUIL(J,L)
	SUIL(J,L) = SHARETOT*SSUIL(J,1,L)
      SUIL(JUOSCRUB,L) = SHARETOT*SSUIL(J,2,L)

      J=INGAS
      SHARETOT=SUIL(J,L)
	SUIL(J,L) = SHARETOT*SSUIL(J,1,L)
      SUIL(JUGSCRUB,L) = SHARETOT*SSUIL(J,2,L)

!     Compute shares of solid sector subsectors

      J=INCOAL
      SHARESOL=SUIL(J,L)
      SUIL(J,L) = SHARESOL*SSUIL(J,1,L)
      SUIL(JUBMASS,L) = SHARESOL*SSUIL(J,3,L)
      SUIL(JUCSCRUB,L) = SHARESOL*SSUIL(J,2,L)      


!CCCC END ELECTRICITY PRICE  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


!
!
! COMPUTE THE ENERGY SERVICE PRICES
!     Loop over final demand sectors.
      PS=0.D0
      DO 110 K=1,NK
         PKLM(K,L,M)=0.D0
         RPKL(K,L)=0.D0
!     Note:  Consider updating sjkklp each period  maw 2/2/98

         DO 100 J=1,NJ

!        For forced H2 policy only, assume all fossil fuel is forced to
!        be converted to H2 (separate from the explicit H2 in model)
!        Add conversion losses and costs to hydrogen
! 		 Note: IGOH2 = 1 (set above) toggles forced H2 policy

            IF(J.LE.NF)THEN   !Fossil only	   
	         JH=J+NH2
               GTOH2=1+IGOH2*H2FRAC(L)*(GHILM(J+NH2,M)-1.0)
!              Add carbon disposal costs
         	     CDISPCST=IGOH2*H2FRAC(L)*CARBDISP(L,M)* &
        		 TXUILM0(J,L,M)/100.0

               PP=PJLM(J,L,M)*GTOH2 + HHILM(JH,M)*H2FRAC(L) + cdispcst

	         IF (J .le. 2) THEN	! Share out price according to synfuel share
	           PP = (PJLM(J,L,M)+ TXJKLM(J,K,L,M))*SynShareJILM(J,J,L,M) + &		! Fossil Oil/Gas
	                PCJILM(3,J,L,M)*SynShareJILM(3,J,L,M) + &	! Syn from Coal
	                PCJILM(4,J,L,M)*SynShareJILM(4,J,L,M)		! Syn from Biomass
	           IF (J .eq. 1) PP = PP + PCJILM(2,J,L,M)*SynShareJILM(2,J,L,M) ! Syn liquids from gas

	         ELSE
	           PP = (PJLM(J,L,M)+ TXJKLM(J,K,L,M))
	         END IF 

	      ELSE
	         PP=PJLM(J,L,M)
	      END IF

!           Add transmissions and distribution cost to electricity price            
            IF (J.EQ.NJ) THEN 
		     PP = PP + PUTDKM(K,M)
               PUKLM(K,L,M) = PP
            END IF

! Carbon tax moved above, so that synfuels could be accounted for. sjs - 08/01

! Now take into account service efficiency and add non-fuel costs to get service price
            PP=(PP)*GJKLM(J,K,L,M) + HJKLM(J,K,L,M)
            PJKLM(J,K,L,M)=PP
            PKLM(K,L,M)=PKLM(K,L,M)+SJKLP(J,K,L)*PJKLM(J,K,L,M)	! Aggregate service cost
   
            PP=PP-HJKLM(J,K,L,M)
            RPKL(K,L)=RPKL(K,L)+PP*SJKLP(J,K,L)	! Price elasticity denominator
  100    CONTINUE	! Fuel loop (to electricity)

!     Compute biomass and hydrogen end-use price
!     Move inside large loop so that can add to energy service price & RPKLM calcuations -- sjs 10/00

       DO J=JSBMASS, JSH2
 	       PJKLM(J,K,L,M) = PJLM(J,L,M)*GJKLM(J,K,L,M) + HJKLM(J,K,L,M)
 ! Add H2 distribution cost  -- sjs 10/01	   
	       IF (J.eq.JSH2) &	
			  PJKLM(J,K,L,M) = (PJLM(J,L,M)+PH2TDKM(K,M))*GJKLM(J,K,L,M) + HJKLM(J,K,L,M)

		   PKLM(K,L,M)=PKLM(K,L,M)+SJKLP(J,K,L)*PJKLM(J,K,L,M)
           PP=PJKLM(J,K,L,M)-HJKLM(J,K,L,M)
           RPKL(K,L)=RPKL(K,L)+PP*SJKLP(J,K,L) 	   
       END DO

! Price for GDP feedback
         PS=PS+BSKL(K,L)*PKLM(K,L,M)	! This also should be updated, although only affects GDP feedback -- sjs

! Calculate the energy service elasticity
         RPKL(K,L)=RPKK(K)*PKLM(K,L,M)/RPKL(K,L)
         RPKLM(K,L,M)=RPKL(K,L)

! Code above recalculates RPKL each time period
! This was meant to be used for initialization only
! However, sometimes there are bizzare values in base year in developing regions.
! So compromize and freeze these values after 2020 to remove any spurious dynamics
! after this point.  sjs -- 11/01

! Also check for such spurious values and reduce them.

       IF (M .gt. 4) THEN
          RPKLM(K,L,M) = RPKLM(K,L,4)
          RPKL(K,L) =  RPKLM(K,L,M)
       ELSE
          IF (L .gt. 5) THEN
    !          SaveR = RPKL(K,L)
              AveRPKL = SUM(RPKL(K,1:5))/5.0
              Diff = (abs(RPKL(K,L)-AveRPKL))/abs(AveRPKL)
              IF (Diff .gt. 0.25)  RPKL(K,L) = AveRPKL
              RPKLM(K,L,M)=RPKL(K,L)
 !             IF (L .eq. 7 .and. M .eq. 2) Write(*,*) Diff, SaveR, AveRPKL, RPKL(K,L)
          END IF
       END IF
  110 CONTINUE
       
       
      IF (M .EQ. 1) THEN
        BPSLM(L,M)=PS
        PS=1.D0
      ELSE
         BPSLM(L,M)=PS
         PS=PS/BPSLM(L,M-1)
	END IF
!
      DO 115 K=1,NK
         IF (M .EQ. 1)   BPKL(K,L)=PKLM(K,L,M)
         PKLM(K,L,M)=PKLM(K,L,M)/BPKL(K,L)		! PKLM is scaled to base-year value
  115 CONTINUE
!
!             ----------------------------------------
!             --  COMPUTE BASE GNP FOR ALL REGIONS  --
!             ----------------------------------------
        IType = 0	! Normally use GNP Feedback
        YLM(L,M) = GDP_Fn(L,M,IType)
        
  130 CONTINUE

      RETURN
      END



Real*8 FUNCTION GDP_Fn(L1,M1,IType)
      
!***********************************************************************
!
!   Function which returns GDP in YLM form, which is scaled to the 1975 value
!   IType = 0 includes energy price feedback	(default)
!             Note that mode 0 cannot be done looking forward
!   IType = 1 does not include energy price feedback
!             But this only works if YLM is populated self-consistantly with 
!             that do not include the energy feeddback.
!             Therefore, in general, this works only before the model has run
! 
!
!   Must use this function for setting GDP so that ag module has access
!   to forward-looking GDP
!
!***********************************************************************

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	  REAL(8) PSratio	! Ratio of energy prices between current period and previous period
	  INTEGER IType

!              ------------------------------------
!              --      FOR NON-BASE YEARS        --
!              --  1) COMPUTE ACTUAL LABOR PRO-  --
!              --       DUCTIVITY FOR PREVIOUS   --
!              --       PERIOD                   --
!              --  2) COMPUTE BASE LABOR PRO-    --
!              --       DUCTIVITY FOR PREVIOUS   --
!              --       PERIOD                   --
!              --  3) COMPUTE BASE GNP FOR CUR-  --
!              --       ENT PERIOD               --
!              ------------------------------------
 
!  Branch 1 uses working age population changes to scale GNP
!  Branch 2 is the old method.

!              ------------------------------------
!              --  INITIALIZE BASE GNP TO 1 FOR  --
!              --        THE BASE PERIOD         --
!              ------------------------------------

      if (M1 .eq. 1) then
        YLM(L1,M1) = 1.d0
	    GNP_tmp = 1.d0
      else

        IF (ifpoprate) then     
          if (M1 .eq. 2) then
            GNP_tmp = YLM(L1,M1-1) * (1.D0 + PROLM(L1,M1)) ** NJUMP &
                       * (ZLM(L1,1)/ZLM(L1,0))
	      else
	        GNP_tmp = YLM(L1,M1-1) * (1.D0 + PROLM(L1,M1)) ** NJUMP &
                     * (RLFP(L1,M1)*(WAgeMC(1,L1,M1) + WAgeMC(2,L1,M1))) &
                     / (RLFP(L1,M1-1)*(WAgeMC(1,L1,M1-1) + WAgeMC(2,L1,M1-1)))
	      end if
	    ELSE
          IF (LFPercMethod .eq. 0) then	! Old method
              GNP_tmp = YLM(L1,M1-1) * (1.D0 + PROLM(L1,M1)) ** NJUMP &
                       * (ZLM(L1,M1-1)/ZLM(L1,M1-2))
          ELSE		! New method using directly the working percentage PLFPerc
              GNP_tmp = YLM(L1,M1-1) * (1.D0 + PROLM(L1,M1)) ** NJUMP &
                       * ZLM(L1,M1)*PLFPerc(L1,M1)/(ZLM(L1,M1-1)*PLFPerc(L1,M1-1))  
          END IF	! LFPercMethod branch
	    END IF
	  End if

! Use M (in common block), and back out the price adjustment to GDP from 1- current period to eliminate feedbacks
! This code is needed to construct a version that provides GDP without energy feedback
! while the model is running.
! At present, don't need this version, so don't use.
! 
!      IF (IType .EQ. 1) Then
!        Adj = 1
!        DO I = 2,M	
!          PSratio = BPSLM(L1,I)/BPSLM(L1,I-1)
!          Adj = Adj/PSratio**RYL(L1)
!        END DO
         
!        GDP_Fn = GNP_tmp/Adj
!        Return
!      END if

	  GNP(L1,M1) = GNP_tmp		! GNP variable does not include energy price feedback for this period

	  IF (IType .EQ. 1) GDP_Fn = GNP(L1,M1)	! Return unadjusted GDP

      IF (IType .EQ. 0) THEN	!  ADJUST GNP FOR ENERGY PRICE. Moved to avoid zero errors. sjs -- 01/02


        IF (M1 .ne. 1) PSratio = BPSLM(L1,M1)/BPSLM(L1,M1-1)
        IF (M1 .eq. 1) PSratio = 1
        GNP_tmp = GNP_tmp*PSratio**RYL(L1)


        IF (PSratio .eq. 0) then
        	write(100,*) "Error: PSratio, YLM(L1,M1): ",PSratio, YLM(L1,M1)
        	YLM(L1,M1) = 1/PSratio
        endif ! SJS -- test for 1/0 error
		
		
		GDP_Fn = GNP_tmp	! Return adjusted GDP
	  END IF ! Itype = 0
 
      RETURN
      END



Real*8 FUNCTION Mkt_GDP(L1,M1)	
	  ! Function returns base market GDP in trillions of $1990 dollars
      ! Without any price feedbacks       
	   USE COMMON
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)

       Mkt_GDP = GDP_Fn(L1,M1,1)*GNPBL(L1)/(1.0D6)

      RETURN
      END


Real*8 FUNCTION Mkt_GDP_Calb(L1,M1)	
	  ! Function returns base market GDP in trillions of $1990 dollars
      ! Without any price feedbacks       
      
      ! If calibration is in effect, then uses input calibration GNP
      ! otherwise, uses model parameters
      ! Note that M = 2 val with calibration will include price feedback
      
      ! This is so that future GNP values are available to Ag model before the model is run
      
	   USE COMMON
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)

       IF (gnpcalib(L1,M1) .ne. 0) THEN
         IF (gnpcalib(L1,M1) .gt. 10000) gnpcalib(L1,M1) = gnpcalib(L1,M1)/GNPBL(L1)	! Translate to units relative to 1975 GNP (which is in $1990)
         GNP_Scaled = gnpcalib(L1,M1)      
       ELSE
         GNP_Scaled = GDP_Fn(L1,M1,1)
       END IF
       
       Mkt_GDP_Calb = GNP_Scaled * GNPBL(L1)/(1.0D6)
       
      RETURN
      END
