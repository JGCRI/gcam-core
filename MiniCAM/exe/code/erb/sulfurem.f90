!***********************************************************************
!
      SUBROUTINE SULFUREM
!
!*************************************************************************
!
!	Calculates sulfur emissions.  Based on spreadsheet model developed
!	by Steve Smith.    mj 4/00                                      
!	Modified to incorporate biomass fully. Fixed a few problems. sjs 10/00                                      
!   Expanded to allow easy changes in control fraction or control timescale. sjs 01/01                       
!                                      
!   Fixed IOIL error. (mn) Revised Synfuel s-content exception. sjs 02/01
!**************************************************************************
!
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 SO2ASHR2(4,4,NLPMax)
      REAL*8, SAVE:: SO2GDP0_org(NLPMax),Save_TauAdj(NLPMax)
      REAL*8, SAVE:: Save_ControlAdj(NLPMax), Org_Tau(NLPMax)
      REAL*8 SaveSO2MAXCTRL(4,3,NLPMax), SO2GDP0_org2(NLPMax)	! For debugging
      
      PARAMETER (BioEmFact = 0.01)	! Emissions in TgS/EJ
	IF (SO2TAU(1).EQ.0.0) RETURN  !test if sulfur data read in

	DO L = 1, NLP  !regional do loop
	SO2GDPCAP = 1000*GNPPPP(L,M)/ZLM(L,M)
	SO2GDP90 = 1000*GNPPPP(L,2)/ZLM(L,2)
	
! -------------------------------------------------------------------------------------------------
! -------------------------------------------------------------------------------------------------
! If control fractions are to be modified, adjust gdp_0 so that 1990 values are identical
! (Do only once when M=2)
! This means can only run the sulfur routine ONCE when M=2

	IF (SO2CNTRLADJ(L) .eq. 0) SO2CNTRLADJ(L) = 1.0	! In case wasn't read in at all
	IF (Save_ControlAdj(L) .eq. 0) Save_ControlAdj(L) = 1.0
	IF (SO2TauADJ(L) .eq. 0) SO2TauADJ(L) = 1.0	! In case wasn't read in at all

! Adjust for changed final control fraction
! -----------------------------------------
	IF ((SO2CNTRLADJ(L) .ne. 1.) .and. (M .eq. 2)) THEN
	  SaveSO2MAXCTRL(:,:,L)  = SO2MAXCTRL(:,:,L)*SO2CNTRLADJ(L) 
	  IF (Maxval(SaveSO2MAXCTRL) .ge. 100.0) THEN
        DO ISEC = 1,4
        DO J = 1,3
	      IF (SO2CNTRLADJ(L)*SO2MAXCTRL(ISEC,J,L) .gt. Max(99.d0,SO2MAXCTRL(ISEC,J,L)) .and. &
	         (.not.(ISEC.eq.2 .and. J.eq.2)) ) Then												 ! Don't check for industrial gas which often has high control. Check for this below.
	        Temp_Max = SO2CNTRLADJ(L)*SO2MAXCTRL(ISEC,J,L)
	        SO2MAXCTRL(ISEC,J,L) = Max(99.d0,SO2MAXCTRL(ISEC,J,L))/Temp_Max*SO2MAXCTRL(ISEC,J,L) ! Set max to 99%
            MsgStr = "S Control too large. SO2MAXCTRL scaled Down. "
          	Call MCLog(3,MsgStr,L,ISEC,J,SO2CNTRLADJ(L))
          END IF
        END DO
        END DO
      END IF
      
	  SaveSO2MAXCTRL(:,:,L)  = SO2MAXCTRL(:,:,L) ! Save max control for debugging purposes
	  SO2GDP0_org(L) = SO2GDP0(L)
	  SO2GDP0_org2(L) = SO2GDP0(L)

	  Fact1 = (1 + EXP(-4.394492*(SO2GDP90 - SO2GDP0(L))/SO2TAU(L)))
	  
	  IF (SO2CNTRLADJ(L)*Fact1 .lt. 1.01) THEN
	    SO2CNTRLADJ(L) = 1.01/Fact1	!This limits the decrease in control percentage to 1990 level
	    MsgStr = "S Control Decrease ran into limit "
        Call MCLog(3,MsgStr,M,L,0,SO2CNTRLADJ(L))
	  END IF

	  SO2GDP0_prev = SO2GDP0(L)
	  SO2GDP0(L) = SO2TAU(L)/(4.394492) * Log(SO2CNTRLADJ(L)*Fact1 - 1) + SO2GDP90
	  SO2MAXCTRL(:,:,L) = SO2CNTRLADJ(L) * SO2MAXCTRL(:,:,L)

	  MsgStr = "S controls adjusted to match new Fcontrol. New SO2GDP0:"
      Call MCLog(3,MsgStr,M,L,0,SO2GDP0(L))
	  Save_ControlAdj(L) = SO2CNTRLADJ(L)		! Save adjustment used
	  SO2CNTRLADJ(L)	= 1						! And now zero so won't be adjusted again!

	  ! Check for errors in adjusting maximum control percentage
	  IF (SO2MAXCTRL(2,2,L) .gt. 100.0) SO2MAXCTRL(2,2,L) = SaveSO2MAXCTRL(2,2,L)	! Special case for industrial gas
	  
	 IF (Maxval(SO2MAXCTRL) .ge. 100) THEN
      DO ISEC = 1,4
      DO J = 1,3
	    IF (SO2MAXCTRL(ISEC,J,L) .gt. 100.0) THEN
	        MsgStr = "In Sulfur Control Adjustment. Control > 100%. "
            Call MCLog(1,MsgStr,L,ISEC,J,SO2MAXCTRL(ISEC,J,L))
            SO2MAXCTRL(ISEC,J,L) = Max(99.d0,SaveSO2MAXCTRL(ISEC,J,L))
	    END IF
      END DO
      END DO
     END IF	! Error Check
     
	END IF ! SO2 Control Adj
! -----------------------------------------
	IF (Save_ControlAdj(L) .eq. 1) SaveSO2MAXCTRL(:,:,L)  = SO2MAXCTRL(:,:,L) 

! Adjust for changed control timescale
! -----------------------------------------
	IF ((SO2TauADJ(L) .ne. 1.) .and. (M .eq. 2)) THEN
	  Org_Tau(L) = SO2TAU(L)
	  SO2GDP0_org(L) = SO2GDP0(L)	! Note, this might not be the value read in. Could have been changed above.
	  SO2TAU(L) = SO2TAU(L)*SO2TauADJ(L)
	  
! Soln to: (SO2GDP90-SO2GDP0_org(L))/Org_Tau(L) = (SO2GDP90-SO2GDP0(L))/SO2TAU(L)
      SO2GDP0(L) = SO2GDP90-(SO2GDP90-SO2GDP0_org(L))*SO2TAU(L)/Org_Tau(L)
        	  
	  MsgStr = "S controls adjusted to match new Tau. New SO2GDP0:"
      Call MCLog(3,MsgStr,M,L,0,SO2GDP0(L))
	  Save_TauAdj(L) = SO2TauADJ(L)		! Save adjustment used
	  SO2TauADJ(L)	= 1					! And now zero so won't be adjusted again!
	END IF
! -----------------------------------------

  
! -------------------------------------------------------------------------------------------------
! -------------------------------------------------------------------------------------------------

! Main S Emissions Loop
	 TechChange = 1.0
	 DO MM = 3,M 
	    TechChange = TechChange*(1+SO2_Cntrl_TechChange(L)/100.)**15    
	 End Do
!	 IF (L .eq. 14) Write(*,*) "L: ",L,"  M: ",M,"  Tech Change: ",TechChange
	 
	 DO ISEC = 1,4
	 DO J = 1,3
	  
	  SO2CTRL(ISEC,J,L,M) = (SO2MAXCTRL(ISEC,J,L)/100) / &
        (1 + EXP(-4.394492*(SO2GDPCAP - SO2GDP0(L)/TechChange)/SO2TAU(L)))
      
! Ash retention is zero unless fuel is coal. sjs -- 11/00
      SO2ASHR2(ISEC,J,L) = 0
      If (J .eq. 3) SO2ASHR2(ISEC,J,L) = SO2ASHR(ISEC,L)
   	END DO
	END DO

	ISEC = 4
	J = 3
IF (L .eq. 1111) Write(*,'(a,I2,"  ",12f9.1)') "SO2_Ctrl: ",&
	M,SO2CTRL(ISEC,J,L,M), SO2MAXCTRL(ISEC,J,L),SO2GDPCAP, SO2GDP0(L), SO2TAU(L), SO2GDP0_org(L),&
	SO2GDP0_org2(L),Save_TauAdj(L), Save_ControlAdj(L)
	
! Biomass controls
      SO2CTRL(1,4,L,M) = 0 					! Buildings (no controls)
      SO2CTRL(2,4,L,M) = SO2CTRL(2,3,L,M)/2. 	! Industry (only large uses would have any at all. approx as 1/2)
      SO2CTRL(3,4,L,M) = SO2CTRL(3,3,L,M) 	! Transport (shouldn't be any direct use here. But set to coal just in case.)
      SO2CTRL(4,4,L,M) = SO2CTRL(4,3,L,M)/2.		! Elec Gen. 
	
	IOIL = 1
	DO ISEC = 1,3
	  DO J = 1,3
	     SO2EMSJLM(ISEC,J,L,M) = (FJKLM(J,ISEC,L,M) / &
      	SO2ENDN(ISEC,J,L)) * (SO2SCT(ISEC,J,L)/100) *  &
          (1-SO2ASHR2(ISEC,J,L)/100) * 1000 * (1-SO2CTRL(ISEC,J,L,M))
        IF (J .eq. 1 .and. ISEC .eq. 2) Then	! Ajust for non-energy use of oil in industry sector
           EOilTot = SUM(FJKLM(1,:,L,M))
           EOil_NE_Fract = SFEDIL(IOIL,L)*FJKLM(J,ISEC,L,M)/EOilTot	! Adjust SFEDIL from fract of total to fract o industrial use
           EOil_NE_Fract = min(EOil_NE_Fract, 0.25d0)	! Arbitrarilly set max non-energy fraction to 25%
           SO2EMSJLM(ISEC,J,L,M) = SO2EMSJLM(ISEC,J,L,M) * (1-EOil_NE_Fract)
        End if
	  END DO

!Biomass end-use
	  SO2EMSJLM(ISEC,4,L,M) = FJKLM(4,ISEC,L,M) * BioEmFact * &
                        (1-SO2CTRL(4,ISEC,L,M))
	END DO	! ISec
! Cap industrial gas SO2 levels at 1990 levels -- otherwise these get unrealistically large
      IF (SO2EMSJLM(2,2,L,M) .gt. SO2EMSJLM(2,2,L,2)) THEN
         SO2EMSJLM(2,2,L,M) = SO2EMSJLM(2,2,L,2) * (1-SO2CTRL(2,2,L,M))/(1-SO2CTRL(2,2,L,2))
      END IF

!electricity production
	ISEC = 4 !so2 producing "sector" 4 is elec
	DO J = 1,3
	    SO2EMSJLM(ISEC,J,L,M) = (EDRIKLM(J,1,L,M) / &
      	SO2ENDN(ISEC,J,L)) * (SO2SCT(4,J,L)/100) *  &
          (1-SO2ASHR2(ISEC,J,L)/100) * 1000 * (1-SO2CTRL(ISEC,J,L,M))
	END DO
!	electricity biomass. sjs - 10/00. Changed to put in var like rest of them
	SO2EMSJLM(ISEC,4,L,M) = EDRIKLM(IBMASS,1,L,M) * BioEmFact * &
                            (1-SO2CTRL(4,4,L,M))

! -------------------------------------------------------------------------------------------------
! Correct for high efficiency and carbon scrubbing. sjs -- 12/00
! Efficiencies above 40% are assumed to be partially due to IGCC -- intrinsically no SO2 emission
! Once efficiency reaches 63%, assume its all advanced combusion (rememberin that this is the system efficiency, including line losses)
    
    E_max = 0.55	!%at which assume all generation is advanced-cycle

    E_min = max(0.40d0,GenEff(3,L,2))	! make sure 1990 value, if off, doesn't trigger extra SO2 removal
	EqnCoef = (1./E_max)/(1./E_min-1./E_max)
	CoalHighTechFrac = 0.0
	IF (GenEff(3,L,M) .gt. E_min) CoalHighTechFrac = -EqnCoef +EqnCoef*(1./E_min)*GenEff(3,L,M)
	CoalHighTechFrac = min(CoalHighTechFrac,1.d0)
	
    E_min = max(0.40d0,GenEff(1,L,2))	! make sure 1990 value, if off, doesn't trigger extra SO2 removal
	EqnCoef = ((1./E_max)/(1./E_min-1./E_max))  
	OilHighTechFrac = 0.0
	IF (GenEff(1,L,M) .gt. E_min) OilHighTechFrac = -EqnCoef +EqnCoef*(1./E_min)*GenEff(1,L,M)
	OilHighTechFrac = min(OilHighTechFrac,1.d0)

! Fraction of fuel used for electricity that is used in scrubbed production
 	CoalScrubFrac = ESUILM(JUCSCRUB,L,M)*GUILM(JUCSCRUB,L,M)/EDRIKL(INCOAL,1,L)
 	OilScrubFrac  = ESUILM(JUOSCRUB,L,M)*GUILM(JUOSCRUB,L,M)/EDRIKL(INOIL,1,L)
  	GasScrubFrac  = ESUILM(JUGSCRUB,L,M)*GUILM(JUGSCRUB,L,M)/EDRIKL(INOIL,1,L)

! Fuel used in high tech combustion or scrubbing is assumed to have 99% SO2 removed
 	SCoalRemoved = (CoalScrubFrac + (1-CoalScrubFrac)*CoalHighTechFrac)	
 	SOilRemoved = (OilScrubFrac + (1-OilScrubFrac)*OilHighTechFrac)	

!IF (L .eq. 1) &
! 	Write (*,'(a,f5.2," , ",f5.2,"  ",2(f5.0,"%",","))') "Elec Coal & Oil S-free fraction:",SCoalRemoved,SOilRemoved &	! Debug
! 	,100/GUILM(3,L,M),100/GUILM(1,L,M)

	IF (SCoalRemoved .gt. SO2CTRL(4,3,L,M))  &
        SO2EMSJLM(4,3,L,M) = SO2EMSJLM(4,3,L,M)* &
        ( (1-SCoalRemoved) + 0.01 * SCoalRemoved/(1-SO2CTRL(4,3,L,M)) )		! Split between new and old fractions
	IF (SOilRemoved .gt. SO2CTRL(4,1,L,M))  &
        SO2EMSJLM(4,1,L,M) = SO2EMSJLM(4,1,L,M)* &
        ( (1-SCoalRemoved) + 0.01 * SOilRemoved/(1-SO2CTRL(4,1,L,M)) )		! Split between new and old fractions    


! -------------------------------------------------------------------------------------------------
!	industrial procs -------------------------------------------------
	so2ipctrl(l,m) = (SO2IPMR(L)/100) / &
        (1 + EXP(-4.394492*(SO2GDPCAP - SO2GDP0(L)/TechChange)/SO2TAU(L)))
	so2ipemtemp = SO2IPBASE(L) *  &
                    (GNPMRKT(L,M)/GNPMRKT(L,2)) ** SO2PLG(L)
	SO2INDPROC(L,M) = (1-so2ipctrl(l,m)) * so2ipemtemp /  &
                        (1-so2ipctrl(l,2))

!	traditional biomass ---------------------------------------------
	SO2TRBIO(L,M) = (1-SO2CTRL(4,3,L,M)) * SO2TRBIOB(L) /  &
                      (1-SO2CTRL(4,3,L,2)) !scale to 1990
!	make exception for bunker fuel use in tran oil ---------------------------------------------
      IF (FJKLM(1,3,L,M) .gt. SO2BUNKF(L))  		&!Check to make sure don't go negative in future 
       SO2EMSJLM(3,1,L,M) = SO2EMSJLM(3,1,L,M) * &
          (FJKLM(1,3,L,M) - SO2BUNKF(L)) / FJKLM(1,3,L,M)
!	end bunker fuel use exception

!	make some synfuel exceptions ---------------------------------------------
    synusepct = 1.0 - SynShareJILM(1,1,L,M)	! Share of end-use liquids supplied by syn fuels. SynShareJILM(1,1,..) is fraction of conventional

	IF (SO2ALTLIQ .NE. 1.0d0) THEN !not using alternate liquids sulfur formulation so correct directly for synfuel
	   SO2EMSJLM(1:3,1,L,M) = SO2EMSJLM(1:3,1,L,M) * (1-synusepct)
	ELSE

!   Assume that, if synfuels are used, that sulfur content of conventional fuels is increased
!   but only up to a point.
!   Limit the effective conventional pet S content to the 1990 value

	  DO ISEC = 1,3	! Only do for end-use sectors, since these are now the only ones that use synfuels
	    S_Base = SO2SCT(ISEC,1,L)*(1-SO2CTRL(ISEC,1,L,2))	! Base value not to exceed. Start with 1990 value
	    IF (S_Base .lt. 0.5) S_Base = S_Base + 0.01		! To be conservative, allow for more blending if initial S% was low 

	    S_eff = SO2SCT(ISEC,1,L) * (1-SO2CTRL(ISEC,1,L,M)) / (1-synusepct + 1.e-6)	! To avoid div by zero in extreme situation where no pet is used
	    IF (S_eff .gt. S_Base) THEN ! Reduce assumed conv S% if larger than adjusted 1990 value
	      S_Scale = S_Base/ S_eff
	      SO2EMSJLM(ISEC,1,L,M) = SO2EMSJLM(ISEC,1,L,M) * S_Scale
 	      IF (M .ne. 1 .and. S_Scale .lt. 0.80 .and. ISEC .eq. 1) THEN	
 	            MsgStr = "S content adjusted for synfuel prod by:"
                Call MCLog(3,MsgStr,M,L,ISEC,S_Scale)
	      END IF
	    END IF
	  End Do	! ISec
	END IF	! SO2ALTLIQ branch


!	report synfuel % in this vsn of code  (db varid 950)
	SYNUSE(L,M) = synusepct
!	end synfuel exceptions 
!	now total ---------------------------------------------
	SO2EM(L,M) = SUM(SO2EMSJLM(:,:,L,M)) + SO2INDPROC(L,M) + &
                   SO2TRBIO(L,M) + SO2BIOBURN(L,M)

	END DO  !end regional loop
!	add shipping ems to global total
	SO2EMGLBL(M) = SUM(SO2EM(:,M)) + SO2SHIP(M)
	
      RETURN
      END

