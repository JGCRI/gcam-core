SUBROUTINE AG2LINK(AGMODE)
            
! This subroutine is called by the MiniCAM to run the AgLU module
! It can run under 4 modes:
! 1 = Read in AgLU data and calibrate
! 2 = Run the model
! 3 = Calculate greenhouse gas emissions
! 4 = Output

! written 7/01 ktg

USE COMMON

IMPLICIT NONE

REAL(8),PARAMETER :: GJperGCAL = 4.1868d0

! local arrays: prices to be read into AgLU module (10 is max number of market
! indicies in AgLU model), global supply, global demand,calibration prices,
! MiniCAM GDP function which returns YLM (needed for GDP), Land use change emissions
REAL(8) agprice(10,NLP),Sup(8,NLP,NMP),Dem(8,NLP,NMP),MC_CALP(NLP,10),GDP_Fn, &
		LUCEmiss(NLP,NMP), &
		Ag_BiomCEM(NLP)		! Land-use emissions associated with biomass land expansion

INTEGER AGMODE,i,IN,BREAK,T

! AgLU market good indicies
INTEGER JWood,JFWood,JBeef,JFoodGr,JCoarseGr,JOilCrops,JMisccrops,JBio,JPast

SELECT CASE(AGMODE)

  CASE(1)

	! Initialize AgLU market indicies (MiniCAM indices initialized in Nset subroutine)
	JWood = 1
	JFWood = 2
	JBeef = 3
	JFoodGr = 4
	JCoarseGr = 5
	JOilCrops = 6
	JMiscCrops = 7
	JBio = 8
	JPast = 10	! only prices are set to JPast - pasture sup & dem are INPAST=3

    M = 1 ! Initialize to 1 so that GDP fn won't look for energy prices 
	
	! Initialize YLM for Mkt_GDP function within Ag2init
	DO i=1, NLP
	  DO T=1, NMP
		YLM(i,T) = GDP_Fn(i,T,1)
     END DO	
	  ! Initialize the AgLU price of biomass and convert to 1990$/Gcal
	  MC_CALP(i,JBio) = P(INBMASS,i,1) * CVRT90 * GJperGCAL
	END DO

	! Initialize the AgLU model, passing in population and passing out initial prices
	! Demog module doesn't currently produce forward looking pop estimates
	! which AgLU needs, so the Demog cannot be used with AgLU
	! Also pass the size of the population array
	CALL Ag2init(ZLM,MC_CALP,NNLPMax)

	! Initialize MiniCAM Ag sector calibration prices
	DO i=1, NL
	  P(INFOREST,i,1) = MC_CALP(i,JWood)
	  P(INFFOREST,i,1) = MC_CALP(i,JFWood)
	  P(INPAST,i,1) = MC_CALP(i,JPast)
	  P(INFOODGR,i,1) = MC_CALP(i,JFoodGr)
	  P(INCOARSEGR,i,1) = MC_CALP(i,JCoarseGr)
	  P(INOILCROPS,i,1) = MC_CALP(i,JOilCrops)
	  P(INMISCCROPS,i,1) = MC_CALP(i,JMiscCrops)
	  
	END DO
	
	BREAK = 1.0d0


  CASE(2)

	! Don't run if period 1 (1975).  Return zeros.
	IF (M.EQ.1) THEN 
      DO i=1,NL
	    DO IN=1,NINP
 	      AGDEM(IN,i) = 0.0d0
	      AGSUP(IN,i) = 0.0d0
	    END DO
	  END DO
	 
	ELSE         
	  
	  agprice = 0.0d0

	  ! Assign prices from MiniCAM solution to ag model arrays      
	  ! Convert price of biomass first to 1990$/GJ and then to 1990$/Gcal
	  ! The region (L) and time period (M) are known from common block
      agprice(JWood,L) = P(INFOREST,L,M)
	  agprice(JFWood,L) = P(INFFOREST,L,M)
	  agprice(JPast,L) = P(INPAST,L,M)
	  agprice(JFoodGr,L) = P(INFOODGR,L,M)
	  agprice(JCoarseGr,L) = P(INCOARSEGR,L,M)
	  agprice(JOilCrops,L) = P(INOILCROPS,L,M)
	  agprice(JMiscCrops,L) = P(INMISCCROPS,L,M)
	  agprice(JBio,L) = P(INBMASS,L,M) * CVRT90 * GJperGCAL

! Now adjust biomass price for carbon price. 
! The market "price" for market INLUCEm is the emissions per unit biomass production in Tg/EJ
! So conversion is the same as for TXUILM0 (except LU model takes prices in $1990)

      agprice(JBio,L) = agprice(JBio,L)
      IF (agprice(JBio,L) .lt. 0) agprice(JBio,L) = 0.0
      
	  ! Call the AgLU model, passing in prices, passing out supply and demand
	  CALL Ag2model(L,M,agprice,Dem,Sup,MODL)

	  ! Return supply back to the MiniCAM
	  AGSUP(INFOREST,L) = Sup(JWood,L,M)
	  AGSUP(INFFOREST,L) = Sup(JFWood,L,M)
	  AGSUP(INPAST,L) = Sup(JBeef,L,M)
	  AGSUP(INFOODGR,L) = Sup(JFoodGr,L,M)
	  AGSUP(INCOARSEGR,L) = Sup(JCoarseGr,L,M)
	  AGSUP(INOILCROPS,L) = Sup(JOilCrops,L,M)
	  AGSUP(INMISCCROPS,L) = Sup(JMiscCrops,L,M)
	  AGSUP(INBMASS,L) = Sup(JBio,L,M)*GJperGCAL/(100000d0) ! Convert from 10^13cals to EJ

	  ! Return demand back to the MiniCAM (not for biomass)
	  AGDEM(INFOREST,L) = Dem(JWood,L,M)
	  AGDEM(INFFOREST,L) = Dem(JFWood,L,M)
	  AGDEM(INPAST,L) = Dem(JBeef,L,M)
	  AGDEM(INFOODGR,L) = Dem(JFoodGr,L,M)
	  AGDEM(INCOARSEGR,L) = Dem(JCoarseGr,L,M)
	  AGDEM(INOILCROPS,L) = Dem(JOilCrops,L,M)
	  AGDEM(INMISCCROPS,L) = Dem(JMiscCrops,L,M)

!	  IF (M .GT. 2) &
!	    WRITE(97,'(I3,"; ",I5,"|  fforest> ",f7.2,": ",2(f11.0,", "),"  past> ",f7.2,": ",2(f9.0,", "))') L,MODL,&
!	     agprice(Jfoodgr,L),AGSUP(INfoodgr,L),AGDEM(INfoodgr,L), &
!		 agprice(Jfwood,L),AGSUP(INfforest,L),AGDEM(INfforest,L), &
!	     agprice(jpast,L),AGSUP(INPAST,L),AGDEM(INPAST,L)
!		 agprice(JBio,L),AGSUP(INBMASS,L),ESIL(IBMASS,L),SSJKL(2)


	END IF


  CASE(3)
	
	! Retrieve CO2 emissions and other gas activities at end of each time period
	CALL Ag2CH4N2O(M,AGCH4ACT(1:3,1:NL),AGN2OACT(1:3,1:NL))

	IF (M .GT. 1) CALL Ag2Emiss(M,LUCEmiss)

	CARBLAND(:,1) = 0.0d0	! Initialize 1975 land use change emissions to zero
	CARBLAND(1:NLP,M) = LUCEmiss(1:NLP,M)	! Fill MiniCAM array with LUC emissions

	BREAK = 1.0d0

  CASE(4)

	! Print output to csv file
	CALL Ag2Output
	! Dbase output subroutine is called in mcmain so RunID can be used

  CASE DEFAULT

END SELECT

BREAK = 1.0d0

END SUBROUTINE Ag2Link
