!***********************************************************************
!
      SUBROUTINE   CALIB90
!***********************************************************************
!
!                  -- 1990 Calibration routine --
!
! THIS SUBROUTINE CALIBRATES MODEL TO REPRODUCE EXACTLY 1990 ENERGY USE BY FUEL & SECTOR
!
! Routine calculates: 1990 end-use and electric fuel shares, ProLM, and TKLM.
!                     Also: Base-biomass "waste" values.
!
! Routine needs to be run after period M=2 runs once. These preliminary prices are used to update
! variables. Loops several times until calibrated.
!
! Steve Smith - 12/00
!
! New input variables (erbinput): fdemand {EJ}, gnpcalib {$90}, egeneff90 {1/eff}
! 					(note that variable fdemand here is fuel demands -- not energy service demand as in excel calib code)
!
!***********************************************************************
!

! COMMON BLOCKS
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!
!  LOCAL VARIABLES
!
      INTEGER IDum
      CAL_Accuracy = 0.0001
!
      IDum = 0
      oscillate = 0	! Toggle for oscillatory problems
      aMaxDiff = 0 ! Toggle, printout result
      call CalCheck(aMaxDiff)
      aMaxDiff_prev = aMaxDiff
      aImprov_prev = 0
      ocount = 0
      Write (*,*) "Calibrating Period: ",M
      
	  DO While ((IDum .le. 40) .and. (aMaxDiff .gt. CAL_Accuracy) )	! LOOP UNTIL Calibrated
       IDum = IDum + 1
       
       DO L=1,NL		! LOOP through each region
  
!          IF (IDum .lt. 4) CALL Calib_BioSuppl	! Don't need to do once GNP settles down
! Don't do this anymore since biomass market is global.
! Need to fix so that 1990 trad biomass use is correct

          CALL Calib_GNP
          CALL Calib_ElecShares(IDum)
          CALL Calib_EndUShares(IDum) 
          CALL Calib_SectEUse
	   END DO ! L

       CALL UPDATE_SJKLP	! Update to accurate SJKLP's. sjs -- 11/01
       CALL Run_Model	!  Solve for this period again 	   

       aMaxDiff = 0	! Turn off/on progress printout 
       call CalCheck(aMaxDiff)
       
       aImprov = aMaxDiff_prev-aMaxDiff        
       aMaxDiff_prev = aMaxDiff
       aImprov_diff = aImprov
	  END DO ! IDum: Main calib loop
      
       aMaxDiff = 1	! Toggle, printout result
       call CalCheck(aMaxDiff)
!       Call PrintComparison	! Print out all final energy values & compare
      
      Call CalibCO2Em
	  call Run_Model
      
      J = incoal
      isec = 1
      DO L = 1,NL
         P_coal =(PJLM(J,L,M) + TXJKLM(J,isec,L,M))*GJKLM(J,isec,L,M) + HJKLM(J,isec,L,M)	! Coal end-use price. Since this val has been replaced by biomass price in model
         P_Biom = pjklm(incoal+2,isec,L,M)
         IF (P_Biom/P_coal .gt. 10) THEN
           MsgStr = "Biomass price probably too large to calibrate. P_Biom/P_Coal = "
           Call MCLog(2,MsgStr,L,M,J,P_Biom/P_coal)
         END IF
       END DO

      RETURN
      END
      
      SUBROUTINE   CALIB_Other
!***********************************************************************
!    -- Routine to calibrate GNP & Tot PE or FE for any year (generally post 1990)--
!  
!    Note: if calibrating to PE (not FE)
!    		routine internally calibrates to Refined Energy, with adjustments to account for synfuel losses
!          (PE includes losses due to synfuel exports, which are not changed by
!          (changing TKLM, so routine could fail if calibrate to this)
!        
!			The above is what should be, but is not PE is not = RefE + SynLosses exactly so must adjust
!        
!***********************************************************************

      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!  LOCAL VARIABLES
!
      INTEGER IDum
      Real*8  SavePE(NLPMax), RegSynLosses(NLPMax), RegPE(NLPMax)
      CAL_Accuracy = 0.004
!
      IDum = 0
      oscillate = 0	! Toggle for oscillatory problems
      aMaxDiff = 0 ! Toggle, printout result
      call CalCheckPE(aMaxDiff)
      aMaxDiff_prev = aMaxDiff
      aImprov_prev = 0
      ocount = 0

! Save cal PE values 
       SavePE(1:NL) = PEcalib(1:NL,M)
      
	  DO While ((IDum .le. 40) .and. (aMaxDiff .gt. CAL_Accuracy) )	! LOOP UNTIL Calibrated 
        IDum = IDum + 1
       
        IF (FECalib .ne. 1) THEN
! If calibrating for Primary Energy, then Adjust for Difference between PE and RefE

! Adjust for Synfuel losses (This should be the difference, but it isn't)
        SynLossTot = SUM(SYNLoss(:,:,:,M))	! Total losses from synfuel production
        RegSynLosses(1:NL) = SUM(SYNLoss(:,1,1:NL,M),DIM=1)+SUM(SYNLoss(:,2,1:NL,M),DIM=1)

!        Ratio = 1-SynLossTot/TotRefECons
!        PEcalib(1:NL,M) =  Ratio* SavePE(1:NL)	! Subtract Synfuel losses -- these are added back in region by region
												! This should work, but it doesn't since PE-RefE are not equal to synfuel losses (although it should be)
												
! Subtract from total the overall difference between PE and RefE
       TotRefECons = sum(EDRLM(:,M))
       TotPECons = Sum(EDILM(INOIL:INCOAL,1:NL,M))+SUM(EXILM(INCOAL,1:NL,M))+ &
                   SUM(EDILM(IBMASS,1:NL,M))+SUM(ESILM(4:6,1:NL,M))+ &
                   SUM(ESILM(JUFUSION:JUWIND+1,1:NL,M))
       Ratio = 1-(TotPECons-TotRefECons)/TotPECons      
       PEcalib(1:NL,M) =  Ratio* SavePE(1:NL)	! Subtract the global difference between RefE & PE
												! These are distributed evenly, proportional to total PE use
												! This guarantees global calibration, 
												! but regional calibration will be off
												! But results will be "reasonable" 

! Calibrate to new value -- are calibrating to RefE in each region
       END IF !End adjustments if calib to PE
       
       DO L=1,NL		! LOOP through each region
          CALL Calib_GNP
          CALL Calib_TotEUse(IDum)

! Make hydro coefficient (used in ps.for) exactly equal to hydro share
          IF (IDum .eq. 2) BSUILM(JUHYDRO,L,M) =ESIL(NI,L)/FJL(NJ,L)
	   END DO ! L

       CALL Run_Model	!  Solve for this period again 	   

       aMaxDiff = 0	! Turn off/on progress printout 
       call CalCheckPE(aMaxDiff)
       
       aImprov = aMaxDiff_prev-aMaxDiff        
       aMaxDiff_prev = aMaxDiff
       aImprov_diff = aImprov
	  END DO ! IDum: Main calib loop

      
       aMaxDiff = 1	! Toggle, printout result
       call CalCheckPE(aMaxDiff)

       PEcalib(1:NL,M) =   SavePE(1:NL)	! Restore to org value. For good housekeeping.
       
      RETURN
      END
      
!******************************************************************************************
	Subroutine Run_Model	! Solve Main model

       CALL ANTEPER
       CALL SOLUTN
!	   CALL POSTPER	Now called from model
	
	RETURN
	END
	

	Subroutine Run_Model_Once(LL)	! Make one model call for region LL. Not used.
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	      L = LL
            CALL CARBTAX
            CALL PPPP
            CALL PSPS
            CALL SSSS
            CALL DDDD
		  IF (AGMODEL.EQ.0) THEN	! Old AgLU model	
			L = LL
            CALL AGLINK
		  ELSE						! New AgLU model
		    MODEAG = 2
			CALL AG2LINK(MODEAG)
		  END IF
	      L = LL
            CALL SYNFUELS
	      L = LL
                        
	RETURN
	END

!******************************************************************************************
	Subroutine CalibCO2Em	! Calibrate Base-year CO2 Emissions
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/CO2Cal/CO2Calb(3)	! Used only here and erbinput.com
      
      DO IFuel = 1,3
        CEmiss = Sum(CARBFUEL(IFuel,1:NL,2))
        COI(1) = COI(1) * CO2Calb(IFuel)/CEmiss
      END DO
      
      MsgStr = "New Oil CO2 Coefficient: "
      Call MCLog(4,MsgStr,0,0,1,COI(1))
      MsgStr = "New Gas CO2 Coefficient: "
      Call MCLog(4,MsgStr,0,0,2,COI(2))
      MsgStr = "New Coal CO2 Coefficient: "
      Call MCLog(4,MsgStr,0,0,3,COI(3))

      CALL CO2	! now recalc Emissions
      
      RETURN
      END

!******************************************************************************************
! Routine modified 8/01
! Now calibrates for periods M > 2 to GDP without energy feedback
! This is so that Ag model can be given an accurate future GDP value when calibration is turned on
! 
! Since M =2 should always be the same, calibrate actuall GDP for that year
!   (This is also so that base-year values are exactly as input)
!
	Subroutine Calib_GNP	! Set ProLM to achieve proper GNP/GDP level
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Real*8 Model_GNP
      
      IF (gnpcalib(L,M) .gt. 10000) gnpcalib(L,M) = gnpcalib(L,M)/GNPBL(L)	! Translate to units relative to 1975 GNP

      Model_GNP = GDP_Fn(L,M,1)	! GNP relative to 1975 without energy feedback
      IF (M .eq. 2) Model_GNP = YLM(L,M)
      
      IF (gnpcalib(L,M) .ne. 0) THEN
        diff = abs(gnpcalib(L,M)-Model_GNP)/gnpcalib(L,M)
        IF (diff .gt. 0.001) THEN
          ProLMOld = PROLM(L,M)
      
          R = gnpcalib(L,M)/Model_GNP*(1.D0 + PROLM(L,M))** NJUMP
          PROLM(L,M) = R**(1.d0/NJUMP) - 1

          Ratio = 1
          If (M .gt. 2) Ratio = GDP_Fn(L,M,0)/GDP_Fn(L,M,1)
          YLM(L,M) = gnpcalib(L,M)*Ratio	! Set GNP to proper value just so model continues with that.
        END IF	! IF diff > 0.1%
      END IF	! IF gnpcalib <> 0
      
      RETURN
      END

!******************************************************************************************
	Subroutine Calib_SectEUse	! Use mainly to calibrate 1990 energy-use by sector
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       
       DO isec = 1, 3
        TKLM_old = TKLM(isec,L,2)
        SecTot = FJKL(1,isec,L)+FJKL(2,isec,L)+FJKL(3,isec,L)+FJKL(4,isec,L)+FJKL(5,isec,L)! total fuel demand as calcuated by model
        SecTotIn = fdemand(L,isec,1)+fdemand(L,isec,2)+fdemand(L,isec,3)+fdemand(L,isec,4)+fdemand(L,isec,5)! total fuel demand

        R = SecTotIn/SecTot/(1.D0+TKLM(isec,L,2))**NJUMP     
        TKLM(isec,L,2) = (1.d0/R)**(1.0/NJUMP) - 1
      END DO ! sec

      RETURN
      END

!******************************************************************************************
! Calibrate model output to match specified energy-use path (relative to 1990) for each region
! 
! Calibrates to refined energy, even though the input is PE
! Difference is subtracted from calibration value before getting to this routine
!******************************************************************************************
	Subroutine Calib_TotEUse(IDum)  ! Use mainly to calibrate post 1990 total energy-use
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

       Real*8, Save:: SaveVals(NLPMax,4),TKSave(3,NLPMax), Bisected(NLPMax),ILastInc(NLPMax)
       ITSwitch = 10 	! how many iterations until switch method
       					! The bisection method can get in trouble if PE changes due to price changes
       					! So need to let settle down first
       IF (IDum .eq. 1) SaveVals(L,4) = 0
       
      IF (PEcalib(L,M) .ne. 0) THEN
        IF (FECalib .ne. 1) THEN 
        	Call PECalCheck(EnergyDiff,ERatio)
         Else
        	Call FECalCheck(EnergyDiff,ERatio)
        END IF

       IF (EnergyDiff .gt. 0.001) then    
         IF (ILastInc(L) .ne. IDUm-1) SaveVals(L,4) = 0	! reset counter if skipped an increment
         SaveVals(L,4) = SaveVals(L,4) + 1
         IncL = SaveVals(L,4)	! Number of times incremented for this L
         
         IncL = IDum	! Turn off for now until make sure working ok
         
! Bisection routine. Once have tried brute force a few times, bisect to hone in on solution
! Bisection variable is Diff, where Dratio = 1-Diff
        
        ResultRatio = 1.0
        IF (IncL .ge. ITSwitch) THEN	! Use brute force for a few times, if still off then bisect
             DiffOld = SaveVals(L,1)
             ResultRatio = (1-ERatio)/(1-SaveVals(L,2))
             IF (ResultRatio .lt. 0) THEN
                Diff = -DiffOld/2.0 ! Flipped signs, so try again at half increment 
!                IF (ResultRatio .lt. 0) Diff = -Diff
                Bisected(L) = 1 
             ELSE 
               IF (ResultRatio .gt. 1) THEN
                 Diff = -DiffOld	 ! Getting worse, so change direction
               ELSE
                 Diff = DiffOld	 ! Getting better, so keep on going
               END IF
             END IF 
             DRatio = SaveVals(L,3)-Diff
             IF (DRatio .lt. 0) THEN
                DRatio = ERatio	! New values in case started off wrong
                Diff = 1-ERatio
             END IF
        ELSE
             DRatio = ERatio	! Brute force iteration
             Bisected(L) = 0
             Diff = 1-DRatio	! Set diff so that have it once go to bisection
        END IF
        
        DO isec = 1, 3
          R = DRatio/(1.D0+TKSave(isec,L))**NJUMP
          TKLM(isec,L,M) = (1.d0/R)**(1.0/NJUMP) - 1
        END DO ! sec

        IF (IncL .lt. ITSwitch) TKSave(1:3,L) = TKLM(1:3,L,M)	! once bisecting, fix base TKLM and only change diff
        
        SaveVals(L,1) = Diff	! Increment
        SaveVals(L,2) = ERatio	
        SaveVals(L,3) = DRatio	! Last Base Val
        ILastInc(L) = IDum
      END IF	! EnergyDiff > .1%

      END IF ! PEcalib <> 0
            
      RETURN
      END


!******************************************************************************************
! Calc difference between calibrated and modeled primary energy use
 	Subroutine PECalCheck(EnergyDiff,ERatio)	
     USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

        TotCons1 = EDRLM(L,2)	! Use this instead -- only small difference for first period
        CalPE = PEcalib(L,M) * TotCons1

!        TotCons = Sum(EDILM(INOIL:INCOAL,L,M))+EXILM(INCOAL,L,M)+EDILM(IBMASS,L,M)
!        TotCons = TotCons + SUM(ESILM(4:6,L,M))+SUM(ESILM(JUFUSION:JUWIND+1,L,M)) 
        TotConsRefE = EDRLM(L,M)	! Total refined energy demand

!       PECons = Sum(EDILM(INOIL:INCOAL,L,M))+ EXILM(INCOAL,L,M) + &
!                   EDILM(IBMASS,L,M) + sum(ESILM(4:6,L,M)) + &
!                   SUM(ESILM(JUFUSION:JUWIND+1,L,M))
  
       EnergyDiff = abs(CalPE-TotConsRefE)/CalPE
       ERatio = CalPE/TotConsRefE	! Old version, using PE, is DRatio = CalPE/TotCons

      RETURN
      END

!******************************************************************************************
! Calc difference between calibrated and modeled final energy use
	Subroutine FECalCheck(EnergyDiff,ERatio)
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

        TotCons1 = EFLM(L,2)	! Use this instead -- only small difference for first period
        CalFE = PEcalib(L,M) * TotCons1

        TotConsFE = EFLM(L,M)	! Total refined energy demand
  
       EnergyDiff = abs(CalFE-TotConsFE)/CalFE
       ERatio = CalFE/TotConsFE

      RETURN
      END


!******************************************************************************************
	Subroutine Calib_BioSuppl	! Make sure enough bio supply to meet demand
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
! Change bio supply parameters if not enough
! This is important. If is not enough supply, then bio price gets extremely high 
! and ratio of prices to the elasticity can easilly go beyond double precision and calibration will fail

      IM = 1
        BioDemand = fdemand(L,1,5)+fdemand(L,2,5)+fdemand(L,3,5)+ fdemand(L, 4, 7)	! Total biomass demand        
        BioSuppl = BIOLM(L,IM) * BIOPSM(3,2,1)	! Supply near middle of curve
        BioSuppl = BioSuppl*YLM(L,M)**RYSHT

        IF (BioSuppl .lt. BioDemand) BIOLM(L,IM) = BioDemand*2.0 ! Make supply is at least twice demand so that price is reasonable
      
      RETURN
      END


!******************************************************************************************
	Subroutine Calib_ElecShares(IDum)	! Set ProLM to achieve proper 1990 GNP/GDP level
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
 
      REAL*8  ElecSum(NLPMax), sumfuel(NLPMax), guilmavg(NLPMax), elecgen(NLPMax,7)
      REAL*8  sumhyd(NLPMax), loc_suil(NLPMax,7)

      ElecSum(L) = 0
      sumfuel(L) = 0

      !fossil fuel generation
      DO iftype = 1, 3
        guilm(iftype,L,M) = egeneff90(L, iftype)
        elecgen(L, iftype) = fdemand(L, 4, iftype) / egeneff90(L, iftype)
        ElecSum(L) = ElecSum(L) + elecgen(L, iftype)
        sumfuel(L) = sumfuel(L) + fdemand(L, 4, iftype)
      END DO
      
      !average fossil fuel generation efficiency
      guilmavg(L) = sumfuel(L) / ElecSum(L)
      
      !renewable generation
      DO iftype = 3 + 1, 6
        guilm(iftype,L,M) = egeneff90(L, iftype)	! these are all = 1, but do anyway
        elecgen(L, iftype) = fdemand(L, 4, iftype) / guilmavg(L)
        ElecSum(L) = ElecSum(L) + elecgen(L, iftype)
      END DO
      
      !biomass generation
      iftype = 7	!Biomass
      guilm(iftype,L,M) = egeneff90(L, iftype)
      elecgen(L, iftype) = fdemand(L, 4, iftype) / egeneff90(L, iftype)
      ElecSum(L) = ElecSum(L) + elecgen(L, iftype)
      
      loc_suil(L,6) = elecgen(L, 6) / ElecSum(L)	!Hydro share
      sumhyd(L) = ElecSum(L) - elecgen(L, 6) ! subtract hydro generation
      
      !share of total by fuel and logit coefficient for electricity
      DO iftype = 1, 7
        If (iftype .ne. 6) loc_suil(L, iftype) = (1 - loc_suil(L, 6)) * elecgen(L, iftype) / sumhyd(L)
      END DO

! Check consistancy on first run
      IF (IDUM .eq. 1) THEN
        TotDem = 0
        DO K = 1,3
          TotDem = TotDem + fdemand(L, K, 4) 
        END DO
        Diff = abs(TotDem-ElecSum(L))/TotDem
        IF (Diff .gt. 0.01) write(*,'(a,I2,a,f5.1,"%")')  &
           "***WARNING: ",L," Elec Generation Inputs inconsistant by: ",Diff*100.   
      END IF
 
 ! now do coal/biomass split
       B_Coal = bssuilm(incoal, 1, L, M)
       B_Biom = bssuilm(incoal, 3, L, M)
       
       bssuilm(incoal, 1, L, M) = elecgen(L, incoal) / PSSUM(INcoal,1,L,M) ** Ruiss(incoal, 1)

       bssuilm(incoal, 3, L, M) = elecgen(L, 7) / PSSUM(INcoal,3,L,M) ** Ruiss(incoal, 3)

       subsum = bssuilm(incoal, 1, L, M) + bssuilm(incoal, 3, L, M)
       bssuilm(incoal, 1, L, M) = bssuilm(incoal, 1, L, M) / subsum
       bssuilm(incoal, 3, L, M) = bssuilm(incoal, 3, L, M) / subsum
      
       incoalShare = elecgen(L, incoal) / (elecgen(L, incoal) + elecgen(L, 7))
       BioShare = elecgen(L, 7) / (elecgen(L, incoal) + elecgen(L, 7))
       
      !Note: puilm(incoal,L,M) is the new combined solid's price calculated by ps.for
      
!   now logit coefficient's for rest of rest of electric generation based on that for oil
 
      DO iftype = 2, 5 ! Only calcuate up to solar
         GenShare = loc_suil(L, iftype)
         IF (iftype .eq. incoal) GenShare = GenShare + loc_suil(L, 7)
         bsuilm(iftype,L,M) = bsuilm(1,L,M) * (GenShare / loc_suil(L, 1))  &
                                * (puilm(1,L,M) / puilm(iftype,L,M)) ** rui(iftype)
      END DO
      
!  Re-scale coefficients so that they all sum to unity
       tempsum = sum(bsuilm(1:5,L, M))
       bsuilm(1:5,L, M) = bsuilm(1:5,L, M) / tempsum
       
      ! Hydro share set directly
          bsuilm(6,L,M) = loc_suil(L, 6)
                        
      RETURN
      END

!******************************************************************************************
	Subroutine Calib_EndUShares(IDum)
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	  INTEGER IDum
  
      REAL*8  ESdemand(NLPMax,4,7), sumjklm(NLPMax,4,9), ssjklm(NLPMax,4,9) 
      REAL*8  B_Coal,B_Biom,P_coal,P_Biom
      
! Correct for different end-use energy from transportation module, if present
      IF (NewTransp .eq. 1 .and. L .eq. 1) THEN
        DO iftype = 1, 5
           TranspFuel = TRANFUEL(3,NTMODE+1,iftype,L,M)*1.055/1.05
           CalFuelTrans = fdemand(L, 3, iftype)
           Diff = CalFuelTrans-TranspFuel
           fdemand(L, 3, iftype) = TranspFuel
           fdemand(L, 2, iftype) = fdemand(L, 2, iftype) + Diff	! Make-up difference in industry sector
        END DO
      END IF
     
      DO isec = 1,3		! Calc end-use energy service demands
       sumjklm(L, isec, M) = 0
       DO iftype = 1, 5
         ESdemand(L, isec, iftype) = fdemand(L, isec, iftype) / gjk(iftype, isec)
         sumjklm(L, isec, M) = sumjklm(L, isec, M) + ESdemand(L, isec, iftype)
       END DO
      END DO
      
      !determine energy service consumption shares by fuel by sector
      DO isec = 1, 3
       DO iftype = 1, 5
         ssjklm(L, isec, iftype) = ESdemand(L, isec, iftype) / sumjklm(L, isec, M)
       END DO
      END DO
      
      X = YLM(L,M)/(ZLM(L,M)/ZLM(L,1))
      
      ! Now new fuel shares
      DO isec = 1, 3
       IF (bsjklm( 1, isec,L, M) .eq. 0) bsjklm( 1, isec,L, M) = 1.0	! Make sure is not zero
       DO iftype = 2, 4
       IF (iftype .eq. 3) BOld = bsjklm(iftype,isec,L, M)
        Share = ssjklm(L, isec, iftype)
        IF (iftype .eq. 3) Share = Share + ssjklm(L, isec, incoal + 2) ! Since 3 is coal + biomass in model
        bsjklm(iftype,isec,L, M) =   &
            bsjklm(1,isec,L, M) * (Share/ ssjklm(L, isec, 1))  &
            * (pjklm(1, isec, L, M) / pjklm(iftype,isec,L, M)) ** rpjk(iftype,isec)  &
            * X ** (ryjklm(1, isec, L, M) - ryjklm(iftype,isec,L, M))

       END DO !  ftype

!  Re-scale coefficients so that they all sum to unity
       tempsum = bsjklm(1,isec,L,M)+bsjklm(2,isec,L,M)+bsjklm(3,isec,L,M)+bsjklm(4,isec,L,M)
       bsjklm(:,isec,L, M) = bsjklm(:,isec,L, M) / tempsum

      END DO !  sec
     
!   Calculate bssjklm to give proper coal/biomass end-use split
      DO isec = 1, 3
        BSum = bssjklm(isec, 1, L, M) + bssjklm(isec, 2, L, M)
        IF (BSum .eq. 0) THEN
          IF (bssjklm(isec, 1, L, M) .eq. 0) bssjklm(isec, 1, L, M) = 1.0
          BSum = 1.
        END IF
      
        bssjklm(isec, 1, L, M) = bssjklm(isec, 1, L, M) / BSum
        bssjklm(isec, 2, L, M) = bssjklm(isec, 2, L, M) / BSum

          J = incoal
          P_coal =(PJLM(J,L,M) + TXJKLM(J,isec,L,M))*GJKLM(J,isec,L,M) + HJKLM(J,isec,L,M)	! Coal end-use price. Since this val has been replaced by biomass price in model
    
          B_Coal = ssjklm(L, isec, incoal) / P_coal ** Rsspjk(isec, 1)
          B_Biom = ssjklm(L, isec, incoal + 2) / (pjklm(incoal+2,isec,L,M)) ** Rsspjk(isec, 2)
          tempsum = B_Coal + B_Biom
          If (tempsum .eq. 0) Then
              tempsum = 1
              B_Coal = 1
          End If
          B_Coal= B_Coal / tempsum
          B_Biom = B_Biom / tempsum

          bssjklm(isec, 1, L, M) = B_Coal
          bssjklm(isec, 2, L, M) = B_Biom

           P_Biom = pjklm(incoal+2,isec,L,M)
                  IF ( L .eq. 19) &
          write(*,'(a,I2,", ",f6.2,": ",14(f6.2,", "))')  &
            "Biom Price & Components:  ",isec,P_Biom,PJLM(incoal+2,L,M) &
             ,TXJKLM(incoal+2,isec,L,M),GJKLM(incoal+2,isec,L,M),HJKLM(incoal+2,isec,L,M)
          
 
      END DO !  sec

       BioD = FJKL(5,1,L)+FJKL(5,2,L)+FJKL(5,3,L)+fdemand(L,4,7)
    
      RETURN
      END
      
       
!******************************************************************************************
	Subroutine CalCheck(aMaxSecDiff)
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
 
      iPrintOut = aMaxSecDiff
      aMaxDiff = 0
      aMaxSecDiff = 0
      AbsMaxSecDiff = 0
      
      DO L = 1,NL
      DO isec = 1, 3
       IF (isec.EQ.3 .AND. L.EQ.1 .and. NewTransp .eq. 1) THEN
		! Do nothing, since can't calibrate the new transport code
	   ELSE
        SecTot = FJKL(1,isec,L)+FJKL(2,isec,L)+FJKL(3,isec,L)+FJKL(4,isec,L)+FJKL(5,isec,L)! total fuel demand as calcuated by model
        SecTotIn = fdemand(L,isec,1)+fdemand(L,isec,2)+fdemand(L,isec,3)+fdemand(L,isec,4)+fdemand(L,isec,5)! total fuel demand
        SecDiff = abs(SecTot - SecTotIn)/SecTotIn
        AbsSecDiff = abs(SecTot - SecTotIn)
        IF (AbsSecDiff .gt. 0.01) aMaxSecDiff = MAX(SecDiff,aMaxSecDiff)  !Don't count mall differences due to input trunctions and such.
        AbsMaxSecDiff = MAX(AbsMaxSecDiff,AbsSecDiff) 

        bMaxDiff = 0	! Now do for electric generation
        DO iftype = 1, 5
          IF (fdemand(L, isec, iftype) .ne. 0) Then
             AbsDiff = abs(FJKL(iftype,isec,L) - fdemand(L, isec, iftype))
             IF (AbsDiff .gt. 0.01)  &
                  bMaxDiff = MAX(AbsDiff/fdemand(L, isec, iftype),bMaxDiff)      
           END IF
        END DO ! iftype         
        aMaxDiff = MAX(bMaxDiff,aMaxDiff)

        tempMax = aMaxDiff ! Max difference not counting electricity
        
        SecDiff = bMaxDiff	! Also check for elec
        AbsSecDiff = AbsDiff
        IF (AbsSecDiff .gt. 0.01) aMaxSecDiff = MAX(SecDiff,aMaxSecDiff)  !Don't count mall differences due to input trunctions and such.
        AbsMaxSecDiff = MAX(AbsMaxSecDiff,AbsSecDiff) 

       IF (1 .eq. 2) then	! For debugging
        write (*,'(I3,",",I3,a,2(f6.3,", "),(f5.1,"% diff"))') L,isec,"- Model & Calib fuel totals: ", SecTot, SecTotIn, SecDiff*100
         write(*,'(a,5(f6.3,", ")," EJ")') "   Model:",FJKL(1,isec,L),FJKL(2,isec,L),FJKL(3,isec,L),FJKL(4,isec,L),FJKL(5,isec,L)
         write(*,'(a,5(f6.3,", ")," EJ")') "   Input:",fdemand(L,isec,1),fdemand(L,isec,2),fdemand(L,isec,3),fdemand(L,isec,4),fdemand(L,isec,5)
!         write(*,'(a,2(f5.1,"%, "))')  "       Max SecDiff & MaxDiff(each fuel): ", aMaxSecDiff*100 ,bMaxDiff*100
       END IF
      END IF	! New Transport
      
      END DO ! sec
      END DO ! L

      IF (iPrintOut .eq. 1) THEN
          write (*,'(a,(f8.2,"% ("),f4.1," EJ diff) , Max Fuel Diff: ",(f7.2,"%, "))')  &
                "Calibrated to (max sect tot): ", &
                tempMax*100,AbsMaxSecDiff, aMaxDiff*100
        MsgStr = "Detailed sector/energy calibrated to (%): "
        Call MCLog(4,MsgStr,M,0,0,AbsMaxSecDiff*100)
      END IF
      
!      IF (aMaxDiff .gt. 0.5 .and. aMaxSecDiff .lt. 0.1) aMaxSecDiff = max(aMaxSecDiff,aMaxDiff)
      RETURN
      END

!******************************************************************************************
	Subroutine CalCheckPE(aMaxDiff)	! Check only for total Refined Energy
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      iPrintOut = int(aMaxDiff)	! Toggle for printing out result
      aMaxDiff = 0
      
      DO L = 1,NL
       IF (PEcalib(L,M) .ne. 0) THEN
        IF (FECalib .ne. 1) THEN
          TotCons1 = EDRLM(L,2)
          CalPE = PEcalib(L,M) * TotCons1
          TotConsRefE = EDRLM(L,M)	! Total refined energy demand

          PECons = Sum(EDILM(INOIL:INCOAL,L,M))+ EXILM(INCOAL,L,M) + &
                   EDILM(IBMASS,L,M) + sum(ESILM(4:6,L,M)) + &
                   SUM(ESILM(JUFUSION:JUWIND+1,L,M))

          TotCons = Sum(EDILM(INOIL:INCOAL,L,M))+EXILM(INCOAL,L,M)+EDILM(IBMASS,L,M)
          TotCons = TotCons + SUM(ESILM(4:6,L,M))+SUM(ESILM(JUFUSION:JUWIND+1,L,M))
        
          Diff = abs(TotConsRefE - CalPE)/CalPE
          aMaxDiff = max(aMaxDiff,Diff)
        ELSE	! If calibrating to FE
        
          TotCons1 = EFLM(L,2)	! Use this instead -- only small difference for first period
          CalFE = PEcalib(L,M) * TotCons1
		  TotConsFE = EFLM(L,M)
		  
          Diff = abs(TotConsFE - CalFE)/CalFE
          aMaxDiff = max(aMaxDiff,Diff)
        END IF
       END IF	! If doing calib
      END DO ! L

      IF ((iPrintOut .eq. 1) .and. (aMaxDiff .ne. 0)) THEN
         write (*,'("M = ",I2,a,f8.2,"% ")')  &
                M," Calibrated to: ",aMaxDiff*100
        MsgStr = "Refined energy calibrated to within (%): "
        IF (FECalib .eq. 1) &
      		  MsgStr = "Final energy calibrated to within (%): "
        Call MCLog(4,MsgStr,M,0,0,aMaxDiff*100)
      END IF
       
!       IF (aMaxDiff .gt. 0.01 .and. iPrintOut .eq. 1) STOP		! For Debugging. Stop if didn't really calibrate
      RETURN
      END


!******************************************************************************************
	Subroutine PrintComparison	! For debugging purposes
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
 
      aMaxDiff = 0
      aMaxSecDiff = 0
      AbsMaxSecDiff = 0
      
       L = 7
       
        Write(*,*) "BSJKLM: L,K,BSJKLM(J,K,L,M)'s"
        DO K = 1,3
         Write (*,'(2I2,8(f6.3,", "))') L,K,BSJKLM(1,K,L,M),BSJKLM(2,K,L,M),BSJKLM(3,K,L,M),BSJKLM(4,K,L,M)
        END DO
        Write(*,*)
        
!       Write(*,*) 'BSJKLM Parts: L,BSJKLM(J,K,L,M),PJKLM(J,K,L,M),RPJK(J,K),X,RYJKLM(J,K,L,M)'
!       DO I = 1,4
!        Write (*,'(2I2,8(f6.3,", "))') L,I,BSJKLM(J,K,L,M),PJKLM(J,K,L,M),RPJK(J,K),X,RYJKLM(J,K,L,M)
!       END DO

       Write(*,*) 'EDRIKL Parts: L,EDRIKL(I,1,L),EFJKL(I,1,L),GIJ(I),SUIL(I,L),GUILM(I,L,M)'
       DO I = 1,3
        Write (*,'(2I2,8(f6.3,", "))') L,I,EDRIKL(I,1,L),EFJKL(I,1,L),GIJ(I),SUIL(I,L),GUILM(I,L,M)
       END DO
 
      WRITE(*,*) 'Elec Gen parts: bsuilm(if),bsuilm(1),suil(if),suil(1),puilm(1),puilm(if),rui(if)'
      DO iftype = 2, 4 ! Up to Nuc
       write (*,'(2I2,a,8(f6.3,", "))') L,Iftype,"- Components: ",bsuilm(iftype,L,M),bsuilm(1,L,M),  &
                    suil(iftype,L), suil(1,L),puilm(1,L,M) , puilm(iftype,L,M), rui(iftype)
      END DO
      
       I = 3
       Write(*,*) 'Elec Coal Parts: L,I,BSUILM(I,L,M),PUILM(I,L,M),RUI(I),SSUIL(INcoal,1,L),SSUIL(INcoal,3,L)'
       Write (*,'(2I2,8(f6.3,", "))') L,I,BSUILM(I,L,M),PUILM(I,L,M),RUI(I),SSUIL(INcoal,1,L),SSUIL(INcoal,3,L)
       Write(*,*) 'Elec Coal Parts2: L,I,BSSUILM(INcoal,1,L,M),PSSU(INcoal,1),BSSUILM(INcoal,3,L,M),PSSU(INcoal,3)'
       Write (*,'(2I2,8(f6.3,", "))') L,I,BSSUILM(INcoal,1,L,M),PSSU(INcoal,1),BSSUILM(INcoal,3,L,M),PSSU(INcoal,3)
 
      
      Write(*,*)
      
      Write(*,*) "Sec 1 Biomass Price "
      J = incoal
      isec = 1
      DO L = 1,NL
          BioD = FJKL(5,1,L)+FJKL(5,2,L)+FJKL(5,3,L)+fdemand(L,4,7)
          BioS = ESIL1M(IBMASS,L,M) + ESIL2M(IBMASS,L,M)

         P_coal =(PJLM(J,L,M) + TXJKLM(J,isec,L,M))*GJKLM(J,isec,L,M) + HJKLM(J,isec,L,M)	! Coal end-use price. Since this val has been replaced by biomass price in model
         P_Biom = pjklm(incoal+2,isec,L,M)
         write(*,'(a,I2,4f6.1,f8.1)') "L:  Biom/Coal Price, D, S, Base: ", &
                                  L,P_Biom/P_coal, BioD, BioS,BIOLM(L,1)     
       END DO
       Write(*,*)
      
      DO L = 7,7
!      DO L = 1,NL
      DO isec = 1, 3
        SecTot = FJKL(1,isec,L)+FJKL(2,isec,L)+FJKL(3,isec,L)+FJKL(4,isec,L)+FJKL(5,isec,L)! total fuel demand as calcuated by model
        SecTotIn = fdemand(L,isec,1)+fdemand(L,isec,2)+fdemand(L,isec,3)+fdemand(L,isec,4)+fdemand(L,isec,5)! total fuel demand
        SecDiff = abs(SecTot - SecTotIn)/SecTotIn
        aMaxSecDiff = MAX(SecDiff,aMaxSecDiff)

        AbsSecDiff = abs(SecTot - SecTotIn)
        AbsMaxSecDiff = MAX(AbsMaxSecDiff,AbsSecDiff) 

        bMaxDiff = 0
        DO iftype = 1, 5
          IF (fdemand(L, isec, iftype) .ne. 0)  &
             bMaxDiff = MAX(abs(FJKL(iftype,isec,L) - fdemand(L, isec, iftype))/fdemand(L, isec, iftype),bMaxDiff)      
        END DO ! iftype         
        aMaxDiff = MAX(bMaxDiff,aMaxDiff)
 
         write (*,'(I2,",",I2,a,2(f6.3,", "),(f5.1,"% diff"))') L,isec,"- Model & Calib fuel totals: ", SecTot, SecTotIn, SecDiff*100
         write(*,'(a,5(f5.2,", ")," EJ")') "   Model:",FJKLM(1,isec,L,M),FJKLM(2,isec,L,M),FJKLM(3,isec,L,M), &
         FJKLM(4,isec,L,M),FJKLM(5,isec,L,M)
         write(*,'(a,5(f5.2,", ")," EJ : ",f5.2,"%")') "   Input:",fdemand(L,isec,1),fdemand(L,isec,2), &
         fdemand(L,isec,3),fdemand(L,isec,4),fdemand(L,isec,5),bMaxDiff*100
!         write(*,'(a,2(f5.1,"%, "))')  "       Max SecDiff & MaxDiff(each fuel): ", aMaxSecDiff*100 ,bMaxDiff*100
        
      END DO ! sec

! Print out elec comparison too

        SecTot = EDRIKLM(1,1,L,M)+EDRIKLM(2,1,L,M)+EDRIKLM(3,1,L,M)+EDRIKLM(IBMASS,1,L,M)! total fuel demand as calcuated by model
        SecTotIn = fdemand(L,4,1)+fdemand(L,4,2)+fdemand(L,4,3)+fdemand(L,4,7)! total fuel demand
        SecDiff = abs(SecTot - SecTotIn)/SecTotIn
        aMaxSecDiff = MAX(SecDiff,aMaxSecDiff)

        bMaxDiff = 0
        DO iftype = 1, 7
          IF (fdemand(L, 4, iftype) .ne. 0)  &
             bMaxDiff = MAX(abs(EDRIKLM(iftype,1,L,M) - fdemand(L, 4, iftype))/fdemand(L, 4, iftype),bMaxDiff)      
        END DO ! iftype         
        aMaxDiff = MAX(bMaxDiff,aMaxDiff)

         write (*,'(I3,",",I3,a,2(f6.3,", "),(f5.1,"% diff"))') L,4,"- Model & Calib fuel totals: ", SecTot, SecTotIn, SecDiff*100
         write(*,'(a,6(f5.2,", ")," EJ")') "   Model:",EDRIKLM(1,1,L,M),EDRIKLM(2,1,L,M),EDRIKLM(3,1,L,M),	&!Fossil Fuels 
                                                EDRIKLM(4,1,L,M), &! Nuclear 
      											EDRIKLM(6,1,L,M), &! Hydro 
      											EDRIKLM(7,1,L,M) !Biomass
         write(*,'(a,6(f5.2,", ")," EJ : ",f5.2,"%")') "   Input:",fdemand(L,4,1),fdemand(L,4,2),fdemand(L,4,3), &
      										fdemand(L,4,4),fdemand(L,4,6),fdemand(L,4,7), bMaxDiff*100.
!         write(*,'(a,2(f5.1,"%, "))')  "       Max SecDiff & MaxDiff(each fuel): ", aMaxSecDiff*100 ,bMaxDiff*100
      
      END DO ! L
      
      write (*,'(a,(f9.3,"% ("),f5.2,") , ",(f9.3,"%, "))') "Cal check, max by: sector & detail: ", &
                aMaxSecDiff*100,AbsMaxSecDiff, aMaxDiff*100

      RETURN
      END

!******************************************************************************************
	Subroutine WriteCalFile
!  
! Write out file in input format so that can use that instead of running calibration
! Also so can see new 1990 parameters so that can set future parameters to be compatable
!
      USE COMMON

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      OPEN (98,FILE='Calib.csv')	! SJS - Print out new calibrated parameters
      Write (98,*) 'Parameters for 1990 Calibration'
      Write (98,*) 'Can use this file instead of calibrating:'
      Write (98,*) 'Calibrated for inputs:'
      DO INF=1,INFILES
        Write (98,*) FILES(INF)
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*)
      
      Write (98,*) "Labor productivity growth rate"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 8
      Write (98,*) ",,PROLM,"
      Write (98,*) "Region,1975,1990,2005,2020,2035,2050,2065,2080,2095,"
      DO L = 1,NL
       Write(98,'(I2,",",9(f11.8,","),a)') L,(PROLM(L,MM),MM=1,9) &
               ,REGNAMES(L)
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "1990 Normalized to unity"
      Write (98,*) "BSJKLM Logit function scale parameters for enduse sectors"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I3)') 240
      Write (98,*) ",,BSJKLM,"
      Write (98,*) "Fuel,1975,1990,2005,2020,2035,2050,2065,2080,2095,Sector,Region"
! Normalize to unity      
      DO L = 1,NL
       DO K = 1,3
          Total = SUM(BSJKLM(:,K,L,2))
          BSJKLM(:,K,L,2) = BSJKLM(:,K,L,2)/Total
       END DO
      END DO

      DO J = 1,4
       DO L = 1,NL
        DO K = 1,3
         Write(98,'(I2,",",9(f11.8,","),2(I2,","),a)') J,(BSJKLM(J,K,L,MM),MM=1,9),K,L &
               ,REGNAMES(L)
        END DO
       END DO
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "COAL/BIOMASS SUBSECTOR END USE SHARE COEFFICIENTS"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 82
      Write (98,*) ",,BSSJKLM,"
      Write (98,*) "Sub sec,1975,1990,2005,2020,2035,2050,2065,2080,2095,Sector,Region"
      DO ISS = 1,2
       DO L = 1,NL
        DO K = 1,3
         Write(98,'(I2,",",9(f11.8,","),2(I2,","),a)') ISS,(BSSJKLM(K,ISS,L,MM),MM=1,9) &
               ,K,L,REGNAMES(L)
        END DO
       END DO
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "1990 Normalized to unity"
      Write (98,*) "BSUILM Logit function scale parameters for electricity"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 15
      Write (98,*) ",,BSUILM,"
      Write (98,*) "Fuel,1975,1990,2005.0,2020,2035,2050,2065,2080,2095,Region"
! Normalize to unity      
      DO L = 1,NL
          Total = SUM(bsuilm(:,L,2))
          bsuilm(:,L,2) = bsuilm(:,L,2)/Total
      END DO

      DO J = 1,NU
       DO L = 1,NL
         Write(98,'(I2,",",9(f11.8,","),1(I2,","),a)') J,(bsuilm(J,L,MM),MM=1,9),L &
               ,REGNAMES(L)
       END DO
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "BSSUILM:  ELECTRIC SUBSECTOR SHARE WEIGHTS for Biomass"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 88
      Write (98,*) ",,BSSUILM,"
      Write (98,*) "sub sector,1975,1990,2005.0,2020,2035,2050,2065,2080,2095,Region"
      DO L = 1,NL
         Write(98,'(I2,",",9(f11.8,","),1(I2,","),a)') 3,(bssuilm(3,3,L,MM),MM=1,9),L &
               ,REGNAMES(L)
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "GUILM-GENERATION EFFICIENCY -- THE RATIO OF JOULES OF ENERGY IN ", &
                   "TO JOULES OF ELECTRICITY OUT."
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 70
      Write (98,*) ',,"GUILM(I,L,M)",'
      Write (98,*) "Fuel,1975,1990,2005.0,2020,2035,2050,2065,2080,2095,Region"
      DO J = 1,NNU
       DO L = 1,NL
         Write(98,'(I2,",",9(f11.8,","),1(I2,","),a)') J,(GUILM(J,L,MM),MM=1,9),L &
               ,REGNAMES(L)
       END DO
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "TKLM rate of end-use energy efficiency improvement"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I3)') 220
      Write (98,*) ",,TKLM,"
      Write (98,*) "Sector,1975,1990,2005.0,2020,2035,2050,2065,2080,2095,Region"
      DO K = 1,3
       DO L = 1,NL
         Write(98,'(I2,",",9(f11.8,","),1(I2,","),a)') K,(TKLM(K,L,MM),MM=1,9),L &
               ,REGNAMES(L)
       END DO
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "WASTE ENERGY Capacity"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 49
      Write (98,*) ',,"BIOLM(L,IM)",'
      Write (98,*) "Value (EJ),Region"
      DO L = 1,NL
         Write(98,'((f9.2,","),1(I2,","),a)') (BIOLM(L,1)),L &
               ,REGNAMES(L)
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "Hydro parameters"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 25
      Write (98,*) ',,"HYDRO(I,L)",'
      Write (98,*) "REGION,HYDRO1L,HYDRO2L,HYDRO3L,HYDRO4L,HYDRO5L,"
       DO L = 1,NL
         Write(98,'(I2,",",5(f11.8,","),a)') L,(HYDRO(II,L),II=1,5) &
               ,REGNAMES(L)
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "FEEDSTOCK USES OF FOSSIL FUELS (SFEDIL) -- SHARE OF EACH FOSSIL FUEL USED AS A FEEDSTOCK."
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 47
      Write (98,*) ',,"SFEDIL(I,L)",'
      Write (98,*) "OIL,GAS,COAL,REGION"
       DO L = 1,NL
         Write(98,'(3(f11.8,","),I2,",",a)') (SFEDIL(II,L),II=1,3),L,REGNAMES(L)
       END DO
  

      Write (98,*)
      Write (98,*)
      Write (98,*) "COEFFICIENTS FOR THE SUPPLY FUNCTION FOR BIOMASS"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 48
      Write (98,*) ',,"BIOPSM",'
      Write (98,*) "PRICE,SHARE"
       DO IP = 1,NBIP
         Write(98,'(3(f6.3,","),I2)') (BIOPSM(IP,1,IM),BIOPSM(IP,2,IM), &
                                IM=1,NIM)
       END DO
  

      Write (98,*)
      Write (98,*)
      Write (98,*) "INCOME ELASTICITY OF BIOMASS SUPPLY"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 50
      Write (98,*) ',"RYSHT",'
      Write (98,*) "RYSHT"
         Write(98,'(3(f6.3,","),I2)') RYSHT
  

      Write (98,*)
      Write (98,*)
      Write (98,*) "COAL/BIOMASS SUBSECTOR LEVEL PRICE ELASTICITY"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 81
      Write (98,*) ',,"RSSPJK(J = 3)",'
      Write (98,*) "COAL,BIOMASS,SECTOR"
       DO K = 1,3
         Write(98,'(2(f5.1,","),I2)') (RSSPJK(K,ISS), ISS=1,2),K
       END DO
  
      Write (98,*)
      Write (98,*)
      Write (98,*) "1975 Base GDP/GNP"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I1)') 7
      Write (98,*) ',,"BIOLM(L,IM)",'
      Write (98,*) "Region, Value (Millions $1990)"
      DO L = 1,NL
         Write(98,'(I2,",",f10.0,",",a)') L,GNPBL(L),REGNAMES(L)
      END DO

      Write (98,*)
      Write (98,*)
      Write (98,*) "Market-based GDP to PPP conversion factors"
      Write (98,'(a11)') "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I2)') 87
      Write (98,*) ',,"PPPCONV(L)",'
      Write (98,*) "Region, Value (Millions $1990)"
      DO L = 1,NL
         Write(98,'(I2,",",f10.4,",",a)') L,PPPCONV(L),REGNAMES(L)
      END DO


      Write (98,*)
      Write (98,*)
      Write (98,*) "EOF"

      close(98)
      
      RETURN
      END

! This doesn't work
	Subroutine WriteHeader(Label,ID,Name,Prefix,I1,I2)
      Character(70) Label,Name,Prefix
!       call WriteHeader("Labor productivity growth rate",8,"PROLM","Region",1,9)
      
      Write (98,*)
      N = Len(Label)
      Write(*,*) "Length: ",N
      Write (98,*) Label(1:N)
      Write (98,*) "INPUT_TABLE"
      Write (98,*) "Variable ID"
      Write (98,'(I4)') ID
      Write (98,*) ",,"//Name//","
      
      IF (I1 .eq. 1 .and. i2 .eq. 9) Write (98,*) Prefix//",1975,1990,2005,2020,2035,2050,2065,2080,2095,"
      IF (I1 .eq. 1 .and. i2 .eq. 6) Write (98,*) Prefix//",1975,1990,2005,2020,2035,2050"
      IF (I1 .eq. 2 .and. i2 .eq. 9) Write (98,*) Prefix//",1990,2005,2020,2035,2050,2065,2080,2095,"
      IF (I1 .eq. 2 .and. i2 .eq. 6) Write (98,*) Prefix//",1990,2005,2020,2035,2050"
      
      RETURN
      END
