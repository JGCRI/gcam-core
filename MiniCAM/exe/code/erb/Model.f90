!     Main guts of model within time loop that was plucked from MAIN.FOR
!     to make it easier to call repeatedly from a routine solving for
!     carbon taxes to meet a carbon emissions target.  MAW 1/5/95 



      SUBROUTINE MODEL

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	  Real(8) CTargTotal, EmissionsDiff(16)
	  Real(8), Save:: OldEmissions(16)
	  
      MODL=MODL+1


!           +---------------------------------+
!           |   CALL THE WORLD ENERGY MODEL   |
!           +---------------------------------+
!        Call only over regions in market being evaluated

      NR=MRKDEF(MRK,3)
      CTargTotal = 0
      
	   DO LL=1,NR
            L=MRKDEF(MRK,3+LL)
            CALL CARBTAX
            CALL SSSS

!     CALL THE AG MODEL HERE
			IF (AGMODEL.EQ.0) THEN		! Old AgLU model. THIS WON'T WORK without mods in this new order. sjs
			  L=MRKDEF(MRK,3+LL)
              CALL AGLINK
			ELSE						! New AgLU model
			  MODEAG = 2
			  CALL AG2LINK(MODEAG)
			END IF

            Call SYNFUELS	! Calculate synfuel shares BEFORE calling PPPP

            CALL PPPP
            CALL PSPS
            CALL DDDD

!     Call synfuel production module here and reset region
            L=MRKDEF(MRK,3+LL)
            CALL SynAdjust	! Set syninputs, outputs, & adjust supply & demand. 08/01-sjs. 

! This code is a bad trick to check if there is a carbon target in the regions being evaluated            
            IF (CEMTARGS(L,M) .gt. 0) CTargTotal = CEMTARGS(L,M) 	! If any target is >0, zero any previous negative sums
            IF (CEMTARGS(L,M) .lt. 0 .and. CTargTotal .le. 0) CTargTotal = CTargTotal + CEMTARGS(L,M)   ! Only add if total is not postive         
	   END DO

 	   CALL POSTPER	! Moved here because SO2 (and in future others) need some of these quantities

      CALL CO2  !get CO2 emissions from energy
	
      ! Get ag emissions here. Needed in case we need to get LandUse CO2. sjs - 11/01
	  IF (AGMODEL.EQ.1) THEN  ! Get emissions from new AgLU model
	    MODEAG = 3
		CALL AG2LINK(MODEAG)
	  END IF

!  Compute total emissions to be controlled in the model:  (mrj 1/14/02)
!  EMISSTCE(L,M) holds this information in units of Tg CE by region and period

	EMISSTCE(:,M) = CO2DLM(:,M)    !initialize with CO2 emissions from energy

	!IF constraint is to include land use emissions then add them in now:
	IF (CarbConstraintType .eq. 1) &
	EMISSTCE(:,M) = EMISSTCE(:,M) + CARBLAND(1:NLPMax,M)
	   
    ! if we want to include other greenhouse gases in the model, run the appropriate
    ! subroutines and add the result in to EMISSTCE(L,M):         (mrj)
    ! Also need to call every time if are going to use MAGICC.     
	IF (IOTHGASTOG.EQ.1 .or. IOTHERGHG_Const() .eq. 1) THEN

	  CALL ALLOTHERGASES  !everything but sulfur at the moment
      IF (M .gt. 2) CALL SULFUREM	! Move S Em call here so that can have for every period

      ! following line adds in all other gases in carbon equiv units (including ag sources)
	  ! note that all source units must be in tg gas -- b/c gets multiplied by gwp directly here
	  
	  DO LL = 1, NL
	    EMISSTCE(LL,M) = EMISSTCE(LL,M) + SUM(SUM(OGEMISS(:,:,LL,M),DIM=2) * OGGWP(:,M) * CO2toC)
	  END DO
           
	END IF
    
!  End of EMISSTCE(L,M) computation, the value is passed directly to the solution mechanism


! Code to call MAGICC each time there is an appropriate target
    IF (M .gt. 2 .and. CarbConstraintType .gt. 1 .and. CarbConstraintType .lt. 5 .and. CTargTotal.GT.0.0) THEN
       		CALL MAGICCLINK(M)				! transfers emissions for MAGICC 
       		
       		IF (P(INCARB,1,M) .gt. 0) &
 		    CALL CLIMAT(0,MAGICCCResults,MagEM)	! Run MAIGICC without writting out values
 		    ! Write out something to show its still working
  			IxTag = 0
  			IF (MODL .lt. 5 .or. (mod(MODL,100) .eq. 0)) IxTag = 1
    		IF (IxTag .eq. 1) THEN
 	          write(*,'("M: ",I2,a,I2," ",4f7.1,I6)') M," Constr,CO2 EM,Target,Conc,CTax,&MODL", &
			       CarbConstraintType,MagEM(M,2),CEMTARGS(1,M),MAGICCCResults(2,M-1), P(INCARB,1,M),MODL
 		    END IF
	END IF
		


! some extra reporting for debugging ----------------------------------------
IF (M .gt. 33) THEN
Write (*,'("M:",I2," MODL:",I3," CTax:",f4.0,a,3(f5.1,","),a,2(f6.1,","))') &
		M, MODL,TAXRLM(1,M), &
		" Fos, LUC & Constr: ",SUM(EMISSTCE(1:14,M) )/1000,SUM(CARBLAND(1:14,M))/1000, &
		SUM(CEMTARGS(1:14,M))/1000, &
		" Bio PE: ",SUM(AGSUP(INBMASS,:))
Write (*,'("  Prices:",12(f5.1,","))') P(1:12,1,M)
!Write (*,'(55(" "),"Ag Supplies (Wood,FoodGr,Beef) : ",10(f6.0,","))') &
!		SUM(AGSUP(INFOREST,1:14))/1000,SUM(AGSUP(INFOODGR,1:14))/1000,SUM(AGSUP(INPAST,1:14))/1000
END IF


!     Translate all prices and demands and supplies to the markets used in SOLUTN

      CALL SOLLINK
!

      RETURN
      END
      
