!***********************************************************************
!
      SUBROUTINE   SSSS
!
!***********************************************************************
!
!                     -- THE SUPPLY MODULE --
!
! THIS MODULE GENERATES SUPPLIES FOR ALL REGIONS AND FOSSIL FUELS
! AS WELL AS A SUPPLY PRICE FOR NON-FOSSIL BACKSTOP TECHNOLOGIES.
!
!
! VERSION:  A.31.07.84
!
! INTEGER INPUTS: M, NF, NSYN, NIG, NIS, NL, NU
!
! REAL INPUTS:    BESIL, BSCIL, CIGIS, CIL, CILT, ESFILM, FLRL,
!                 GCI, HCILT, HYDRO, PILM, RCI, RIL, RIGISL, RSYIL,
!                 RYSHT, STISM, VISLM
!
! REAL OUTPUTS:   BESILM, ESIL, ESIL1M, ESIL2M, ESRL1M, ESRL2M,
!                 ESRILM, SCIL, SFLRL,EBRSLM
!
! SUBROUTINES CALLED:  NONE
!
! CODED BY:
!     JAE EDMONDS                      LATEST REVISION:
!     1 JANUARY 1982                    9 AUGUST, 1982
!                                        (NEW BASE SUPPLY ALGORITHM AND
!                                        BIOMASS COMPUTATIONS ADDED)
!                                       
!
!     3/97, move synfuels production to own subroutine, separate biomass
!     from coal   maw 

!***********************************************************************
!
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! LOCAL VARIABLES
!    
      REAL*8   B,C,E,E1,EHOLD,FACT,SHARE,SLOPE, &
        TT,T1,R,V,TECH, PPt, P1, PPvar
!
      INTEGER  IIP,ICOAL,IGAS,IOIL,IUOIL,MM1,MAXR
!
      IOIL = 1
      IGAS = 2
      ICOAL= 3
      IUOIL= 4
      T    = (M-1)*NJUMP
      TT   = NJUMP
      T1   = T-TT
!      MM1=M-1
!-----------------------------------------------------------------------
!                  LONG-RUN SUPPLY SCHEDULE FOR
!                     IS=1= CONVENTIONAL OIL
!                        2= CONVENTIONAL GAS
!                        3= COAL
!                        4= UNCONVENTIONAL OIL
!-----------------------------------------------------------------------

!               +------------------------------------------------+
!               |     COMPUTE ESTIMATED CUMULATIVE PRODUCTION    |
!               +------------------------------------------------+


!                      *******************************
!                      *** ZERO TOTAL FUEL COUNTER ***

      DO 10 I=1,NF
         ESIL(I,L)=0.D0
   10 CONTINUE

!               +------------------------------------------------+
!               |     ESTIMATE CUMULATIVE PRODUCTION TO DATE     :|
!               +------------------------------------------------+

      DO IS=1,NIS
!             ***********************************
!             ***   SET PRIMARY FUELS INDEX   ***
!             ***********************************
          I=IS
          IF(IS.GT.NF) I=IS-NF 


          IF(M.LE.1) THEN
!         ***    START BY ZEROING QISLM FOR (M.EQ.1) AND  ***
!         ***      SETTING OUTPUT TO ESFIL FOR 1975       ***
         	 QISLM(IS,L,M)=0.D0
         	 E=ESFIL(IS,L)		! ESFIL is the energy production in 1990. Is read in.
          ELSE
! Use resource amounts & short-term capacity limits to set production rate
     P1=P(I,L,M)
	        E = FossProd(P1,IS,1)
     
! Branch to test new way of doing production
! TURNED OFF
IF (L .eq. 111 .and. IS .eq. 2 .and. MODL .le. 3 .and. M .eq. 3) Then
   PPvar = PPt*0.25 
   DO II = 1,33
	  PPvar = PPvar*1.1
	  PTemp = PPVar
	  Write(*,'(I2," IS: ",I2,"  ",2(a,f8.3))') II,IS,"Price: ",PTemp," Prod: ",FossProd(PPvar,IS,0)
	  PPVar = PTemp 
   END DO
  END IF 
         END IF !  (M > 1 branch)


!                +-------------------------------------+
!                |  ASSIGN OUTPUT TO SUPPLY VARIABLES  |
!                +-------------------------------------+
          
     IF (IS.LE.NF)   ESIL1M(I,L,M)=E
     IF (IS.GT.NF)   ESIL2M(I,L,M)=E
     ESIL(I,L) = ESIL(I,L) + E
     E=0.0 !reset temp annual production to 0
   END DO ! End Primary fuels loop
   
   
      ESIL2M(IGAS,L,M)=0.D0

!   +---------------------------------------------------+
!   |   COMPUTE BIOMASS SUPPLY PORTION OF SOLID FUELS   |
!   +---------------------------------------------------+

      ESIL1M(ICOAL,L,M)=ESIL(ICOAL,L)

!               --  ZERO THE TOTAL BIOMASS COUNTER  --
        
      I=IBMASS
      ESIL2M(I,L,M)=0.D0
      EBRSLM(L,M)=0.D0

!  --  SET THE BASE YEAR BIOMASS OUTPUT TO ZERO

      IF (M .EQ. 1) THEN       
          EBRSLM(L,M)=BIOLM(L,1)
          GO TO 680
      END IF

!                     --  ITERATE OVER MODES  --
!
!      Now only one mode computed here, the biomass from waste, etc.

      IM=1
      DO 640 IP=1,NBIP

!  --  CHECK EACH CRITICAL POINT TO FIND THE PRICE
!        PAIR BETWEEN WHICH THE MARKET PRICE LIES  --
         IF(P(INBMASS,L,M)-BIOPSM(IP,1,IM)) &
        620,600,600

!  --  IF THE PRICE LIES ABOVE THE SATURATION PRICE,
!        THEN SET THE SHARE TO THE SATURATION LEVEL  --

  600    CONTINUE
         IF (IP .LT. NBIP)  GO TO 640
         SHARE=BIOPSM(NBIP,2,IM)
         GO TO 630

!  --  AFTER CRITICAL POINTS ARE LOCATED BEGIN
!        COMPUTING BIOMASS PENETRATION SHARE  --

  620    CONTINUE
         IF (IP .EQ. 1)   SHARE=0.D0
         IF (IP .EQ. 1)   GO TO 630
         IF (IP .GT. 1)   IIP=IP-1
         SLOPE=(BIOPSM(IP,2,IM)-BIOPSM(IIP,2,IM)) &
         /(BIOPSM(IP,1,IM)-BIOPSM(IIP,1,IM))
         SHARE=BIOPSM(IIP,2,IM)+SLOPE &
         *(P(INBMASS,L,M)-BIOPSM(IIP,1,IM))
  630    CONTINUE
!               --  COMPUTE MODAL BIOMASS CONTRIBUTION  --

         EHOLD=BIOLM(L,IM)*SHARE
         IF (IM.EQ.1)   EHOLD=EHOLD*YLM(L,M)**RYSHT
         IF (IM.EQ.1) EBRSLM(L,M)=BIOLM(L,IM)*(1.D0-SHARE)* &
      YLM(L,M)**RYSHT

         GO TO 650
! EBRSLM is supposed to be landfills (I believe) and was used to drive CH4 emissions.
! this needs to be updated
! EHOLD is what gets used as bio supply figure.

  640 CONTINUE
  650 CONTINUE
      ESIL2M(IBMASS,L,M)=ESIL2M(IBMASS,L,M)+EHOLD
  680 CONTINUE

!               +--------------------+
!               !     ADJUSTMENTS    !
!               +--------------------+

!     -- ADJUST FOR FLARING AND NON-COMMERCIAL GAS USE

      SFLRL(L)=XNTERP(FLRL(1,L),FLRL(2,L),FLRL(3,L),T)
      ESIL1M(IGAS,L,M)=ESIL1M(IGAS,L,M)*(1.D0-SFLRL(L))
      ESIL(IGAS,L)=ESIL1M(IGAS,L,M)


!        +---------------------------------------+
!        !   COMPUTE HYDRO ELECTRIC PRODUCTION   !
!        +---------------------------------------+

      FACT=DEXP(HYDRO(1,L)+HYDRO(2,L)*T)
      ESIL(NI,L)=HYDRO(3,L)*FACT/(1.D0+FACT)
      TESIL(L)=ESIL(NI,L)



      RETURN
      END

!**********************************************************************
!
! This function provides production of fuel IS at price P based.
!
! For high prices is same as original MiniCAM production function
!
! Contains option to asymtoically goes to zero at lower prices
! But this is turned off at present
!
!**********************************************************************
      FUNCTION FossProd(P1,IS,ITag)
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	      E = FossProd_Base(P1,IS,IG,ITag)
	      IGrade = IG
	      EOld = E

IF (1 .eq. 2) Then !turn this part on/off	      
	      Ppt = FosPricePoint(IS)
	      PriceBreakPt = 2*PPt
          IF (P1 < PriceBreakPt) Then
       EScale = FossProd_Base(PriceBreakPt,IS,IG,0)
       E = EScale * (P1/PriceBreakPt)
          END IF
          
END IF      
          FossProd = E

      RETURN
      END



!**********************************************************************
!
! This function provides production of fuel IS at price P1 based on cumulative
! reserves, limited by short-term production capacity constraints. 
! 
! Moved here so that can be accessed separately
! 
! Flag ITag should be set to 1 for this routine to set QISLM var,
! which needs to be done for the "real" call
!
! This is the original function from MiniCAM
! Produciton here does not smoothly go to zero, so this allows the option of changing that
!
!**********************************************************************
      FUNCTION FossProd_Base(P1,IS,IG,ITag)
     
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! LOCAL VARIABLES
!    
      REAL*8   B,C,E,E1,EHOLD,FACT,SHARE,SLOPE, &
        TT,T1,R,V,TECH,&
        QISLM_Save(NMP)
!
      INTEGER  IIP,ICOAL,IGAS,IOIL,IUOIL,MM1,MAXR
!
      IOIL = 1
      IGAS = 2
      ICOAL= 3
      IUOIL= 4
      T    = (M-1)*NJUMP
      TT   = NJUMP
      T1   = T-TT
      
      QISLM_Save(:) = QISLM(IS,L,:)
      PPSave = P1
      
          MAXR=0

!             ***********************************
!             ***   SET PRIMARY FUELS INDEX   ***
!             ***********************************
          I=IS
          IF(IS.GT.NF) I=IS-NF 


!                 +------------------------------------+
!                 | COMPUTE RATE OF TECHNOLOGICAL      |
!                 | CHANGE AND ENVIRONMENTAL COST      |
!                 | STISM=READ IN TECHNICAL CHANGE RATE|
!                 +------------------------------------+
	      TECH=1.0
     DO MTEMP=2,M	         
	         TECH=TECH*(1.D0+STISM(IS,MTEMP))**(NJUMP)
	      END DO
	      

!                 +---------------------------------+
!                 |   LOCATE THE MOST PROFITABLE    |
!                 |         RESOURCE GRADE          |
!                 | RIGISL=READ IN RESOURCE         |
!                 | CIGIS=READ IN EXTRACTION COST   |
!                 | TXISLM=CARBON SEVERANCE TAX     |
!                 | VISLM=READ IN ENVIRONMENTAL COST|
!                 +---------------------------------+

     V=VISLM(IS,L,M) 
     NNIG=NIG+1
     R=0.D0
     DO 30 IG=1,NNIG
!              move up in grade until extraction cost > price
!              R is total resouce available
        IF(IG.LE.NIG) R=R+RIGISL(IG,IS,L) 
        C=CIGIS(IG,IS)						! C is cost of next grade, including tech change & env costs (which go down with tech change)
        C=(C+V)/TECH+TXISLM(IS,L,M)			! This is only place Environmental cost enters 
                                            ! This is also where the carbon tax, TXISLM, enters if applied to production

! Unconventional Oil. Tax variable is not defined here, so put in by ratio with conv fossil oil
        IF (IS .eq. 4) THEN		
            C = C + TXISLM(1,L,M)*(COI(6)/COI(1))
        END IF
        
       IF(C.GT.P1) GO TO 40 
   30       CONTINUE
     MAXR=1 
!                 +-------------------------------+
!                 | COMPUTE CUMULATIVE PRODUCTION |
!                 +-------------------------------+
   40       CONTINUE
!           MAXR IS SET TO 0 INITIALLY
!                 +-------------------------------+
!                 |   IF PRICES ARE TOO LOW ZERO  |
!                 | PRODUCTION.  IF RESOURCE IS   |
!                 | EXHAUSTED SET QISLM TO MAX.   |
!                 +-------------------------------+
     IF(IG.EQ.1) THEN
        E=0.D0
        QISLM(IS,L,M)=QISLM(IS,L,M-1)
        GO TO 60
     END IF
     
!           maxr=1 when no grade left at current price
!           Resource is total resource; sum of all grades
     IF(MAXR.EQ.1) QISLM(IS,L,M)=R
     IF(MAXR.EQ.1) GO TO 50
!                 +-------------------------------+
!                 | IF RESOURCE IS ACTIVE COMPUTE |
!                 |     CUMULATIVE PRODUCTION     |
!                 +-------------------------------+
!           Note grade in production
!           IG is now final grade or grade at which ex. cost>price
!           Determine resource where extraction cost=price

! This code determines cumulative resource used to date (== R) by using price
! Resource used is RIGISL(IG-2,IS,L) + FRACT * RISISL(IG-1,IS,L)
! Where this fraction is determined by price FRACT = (C-P1)/((CIGIS(IG,IS)-CIGIS(IG1,IS))/TECH
! 
! This code is finding the total resource that can be extracted at price P
! If this is greater than the resource extrated to date it tries to extract all of that 
!		-- subject to short-term capacity limits

! Note that C contains non-energy costs so that these costs subtract to zero in "C-P1"

     igused(is,l,m) = ig-1
     IG1=IG-1
     IG2=IG-2
     SLOPE=RIGISL(IG1,IS,L)/((CIGIS(IG,IS)-CIGIS(IG1,IS))/TECH)
     IF(IG.LE.NIG) R=R-RIGISL(IG,IS,L)
     QISLM(IS,L,M)=R-SLOPE*(C-P1)
   50       CONTINUE
   
! Minimum Production
!                 +-------------------------------+
!                 |   ESTIMATE ANNUAL PRODUCTION  |
!                 |RATE FROM CUMULATIVE PRODUCTION|
!                 +-------------------------------+
     IF(IS.LE.NF) E1=ESIL1M(I,L,M-1)
     IF(IS.EQ.IUOIL) E1=ESIL2M(I,L,M-1)
!            IF(IS.EQ.IUOIL) E1=ESIL1M(I,L,M-1)
     E=2.D0*(QISLM(IS,L,M)-QISLM(IS,L,M-1))/TT-E1
     
! This equation is an inversion of the equation that would be used to calculate cumulative 
! production (i.e., QISLM) from annual production (i.e., E & E1). 

!                 +-------------------------------+
!                 | MAKE SURE THAT OUTPUT IS POS- |
!                 |  ITIVE BUT WITHIN SHORT-TERM  |
!                 |       CAPACITY LIMITS         |
!                 +-------------------------------+

! Code below intended to recover from drops, let E1 go back two periods. Not tested.
!  IF (M .gt. 3) Then
!      E2 = ESIL1M(I,L,M-2) 
!      IF(IS.EQ.IUOIL) E2=ESIL2M(I,L,M-2)
!      E1 = max(E1,E2*0.5)
!   END IF

!**************************************************************
! Changes to short-term supply limits. sjs -- 09/01
! This is one of the key parts of fossil supply.
!
! Problem with org. version is that prices MUST increase in order for supply
! to increase significantly. 
! Now apply tech change to capacity limits so that 
! higher tech change allows capacity to grow faster than GDP.
!
! Also, make elasticity relative to extraction cost only, not including environmental
! costs. If environmental costs are included in this calcuation then any fuel with increasing
! environmental costs must escalate price by a large amount to increase production. 
! Just want environmental costs to add to price directly, not to also have this secondary
! effect -- which can be much larger than the environmental cost itself.
!
! Also, if an economy contracts, capacity limit remains at old level.
! 
!**************************************************************

     IF(E.LE.0.D0) THEN
        QISLM(IS,L,M)=QISLM(IS,L,M-1)
        E=0.D0
     ENDIF
     B=BESIL(IS,L)	 ! Set max production to BESIL, minimum capacity change
 
 ! Adjust base capacity limit by amount of tech change
     Techlast = (1.D0+STISM(IS,M))**(NJUMP)   

     IF (IS .le. NF) ExpR = RFosExpan(IS,L,M)	! sjs -- 6/02
     IF (IS .eq. 4)  ExpR = RFosExpan(1,L,M)	! RFosExpan was defined only for three fuels. Expand to include unconventional oil.
     
     EHOLD = E1 *((YLM(L,M)/YLM(L,M-1))**ExpR)* Techlast ! E1 equals production in previous period
     IF(B.LT.EHOLD) THEN   ! Adjust to new capacity limit, if larger than BESIL
        B=EHOLD
     ENDIF

! New variable for GDP exponent added (RFosExpan - above). sjs -- 03/02
! Gives better control over how fast production of fossil fuels can expand.
! If price elasticity is not small, then can expand significantly by increasing prices.


     P_now  = P(I,L,M) - V/TECH	!	This is extraction cost only, subtract environmental costs
     P_last = P(I,L,M-1) - VISLM(IS,L,M-1)*Techlast/TECH
     P1=P_now/P_last	! Adjust either BESIL, or EHOLD, by price ratio

     B=B*P1**RIL(I,L)	! Adjust supply for price effects 


! If production is too high, then restrict to max production rate calculated above.
     IF(M.GT.2) THEN  !only restrict after 1990 maw 11/97
        IF(B.LT.E) THEN
           QISLM(IS,L,M)=QISLM(IS,L,M-1)+TT*(B+E1)/2.D0
           E=B
        ENDIF
	      ENDIF
	      
! This restricts production in new period to increase by increase in GDP * increase in price
! i.e., <= E1*[Y/Y(M-1)]*[P/P(M-1)], with a minimum value of BESIL*[P/P(M-1)].
! Minimum is now adjusted by tech change

60 CONTINUE

      IF (ITAG .NE. 1) QISLM(IS,L,:) = QISLM_Save(:)
      P1 = PPSave
      FossProd_Base = E
END




      FUNCTION   FosPricePoint(IS)
!***********************************************************************
!
!  Function returns the price at which fossil production would be zero
!
!***********************************************************************

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! LOCAL VARIABLES
!    
      REAL*8   B,C,E,E1,EHOLD,FACT,SHARE,SLOPE, &
        TT,T1,R,V,TECH
!
      INTEGER  IIP,ICOAL,IGAS,IOIL,IUOIL,MM1,MAXR
!
      IOIL = 1
      IGAS = 2
      ICOAL= 3
      IUOIL= 4
      T    = (M-1)*NJUMP
      TT   = NJUMP
      T1   = T-TT
!      MM1=M-1
!-----------------------------------------------------------------------
!                  LONG-RUN SUPPLY SCHEDULE FOR
!                     IS=1= CONVENTIONAL OIL
!                        2= CONVENTIONAL GAS
!                        3= COAL
!                        4= UNCONVENTIONAL OIL
!-----------------------------------------------------------------------


!             ***********************************
!             ***   SET PRIMARY FUELS INDEX   ***
!             ***********************************
          I=IS
          IF(IS.GT.NF) I=IS-NF 

          IF(M.LE.1) THEN
      Price_Point = 0
          ELSE

!                 +------------------------------------+
!                 | COMPUTE RATE OF TECHNOLOGICAL      |
!                 | CHANGE AND ENVIRONMENTAL COST      |
!                 | STISM=READ IN TECHNICAL CHANGE RATE|
!                 +------------------------------------+

	      TECH=1.0
     DO MTEMP=2,M	         
	         TECH=TECH*(1.D0+STISM(IS,MTEMP))**(NJUMP)
	      END DO
	      

     V=VISLM(IS,L,M) 
     NNIG=NIG+1
     R=0.D0
     R0 = 0
     DO 30 IG=1,NNIG
!              move up in grade until total resource > Cumulative extracted to date
!              R is total resouce available
!              R0 is total resouce up to previous grade
        R0 = R
        IF(IG.LE.NIG) R=R+RIGISL(IG,IS,L) 

        IF(R.GT.QISLM(IS,L,M-1)) GO TO 40 	! Stop when resource use is greater than total used to date
   30       CONTINUE
   40       CONTINUE

     P2 = (CIGIS(IG,IS)+V)/TECH+TXISLM(IS,L,M)
     IF (IG .eq. 1) Then
        P1 = 0
     ELSE
        P1 = (CIGIS(IG-1,IS)+V)/TECH+TXISLM(IS,L,M)
     END IF
! How far into this resource are we?
     Q_Diff = QISLM(IS,L,M-1)-R0
     SLOPE = Q_Diff/RIGISL(IG,IS,L)
     
     Price_Point = (P2-P1)*SLOPE + P1

          END IF ! M>1 branch
          
     FosPricePoint = Price_Point
     
     END
