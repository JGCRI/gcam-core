!***********************************************************************
!
      SUBROUTINE   NSET
!
!***********************************************************************
!
!         -- THE PROGRAM CONTROL PARAMETER SUBROUTINE --
!
! THIS SUBROUTINE SETS VALUES FOR THE PROGRAM CONTROL PARAMETERS.
!
! MODEL VERSION:  A.07.31.84
!
!
! INTEGER OUTPUTS: MAXAGN, NCO2, NF, NSYN, NI, NIG, NIM, NBIP, NIS, NJ,
!                  NJUMP, NKKL, NKL, NKMAX, NKKMAX, NL, NMREAD, NMKT,
!                  NOPT, NU
!
! REAL OUTPUT:     TEST
!
!     SUBROUTINES CALLED: NONE
!
!     CODED BY:
!       JAE EDMONDS                    LATEST REVISION:
!       1 JANUARY 1982                  8 JULY 1984 
!     Revised June 1995 by MAW to include read in of initialization
!     parameters that used to be in ORIN.  Call to NSET has been moved
!     to MA.FOR.
!
!    NOTE: Nset sets default flags for several alternative calculations (SO2, energy demand, etc.)
!          Therefore, only call at begining -- before any var's are read in. sjs 11/00
!
!***********************************************************************
!
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!
!
!     Set old control options here rather than read them.

!     Check later to see if all are still needed.
      DO 2 KK=1,9
        NOPT(KK) = 1
    2 CONTINUE
      NM=9

!
! SET THE EXCESS DEMAND LIMIT
!      TEST  = 0.0001D0

!
! SET PARAMETER VALUES
!
      NCO2  = 8
      NF    = 3    !Number of fossil fuels
      NSYN   = 2    !Number of synfuel types
      NI    = 6    !Number of primary energy types
	NNI = NI + 1 !Primary energy including biomass
      NIG   = 6    !Number of grade points for resource base supply curves
      NIM   = 1    !Number of biomass modes
      NBIP   = 5   !biomass supply curve points
      NIS   = 4    !oil, gas, coal, unconventional oil
      NNIS  = 5    !primary constrained fuel supply types
      NJ    = 4    !Number of secondary energy types
      NNJ = NJ+2   !sec energy including biomass and hydrogen
      NJUMP = 15   !period time step
      NKMAX = 3    !Max number of energy final demand sectors
      NKKMAX= NKMAX+1
      NL    = NLP   !Number of regions
      NMREAD= 6    !Number of periods for reading in data
      NMKT  = 3    !Number of global energy markets
 
! No longer set NNU here. This is read-in. sjs - 01/02
! NNU is all elec tech's except for advanced "Solar-type" techs
!      NNU = NUP-NStype     !with biomass and coal co2 scrubber plants, H2

!
! NU is number of elec tech's not counting subsectors (used in Ps.for)
! Changed to set relative to NNU since this relation always has to be true (sjs -- 02/01)

      NU  = NNU-4			! Equal to NNU minus four tech's treated as sub-sectors (3 fossil scrubbed + biomass)
      
      
      DO 10 L=1,NL
        NKL(L)=3   !Number of final demand sectors
        NKKL(L)=NKL(L)+1
10    CONTINUE
      KH2=5  !sector number for hydrogen production

!     Set index of inputs

! Note: (sjs 07/01)
! The order of the markets past market 4 is arbitrary
! BUT, if change order, MUST change the order in variable 2 --> P(IN,L,M=1),
! the initial price for each market. Otherwise results will be really messed up!
! (at least using old AgLU model)


	! If we are using the old AgLU model market index structure
	IF (AGMODEL .EQ. 0) THEN
      INOIL=1
	  INGAS=2
	  INCOAL=3
	  INBMASS=4
	  INCROP=5
	  INLSTOCK=6
	  INFOREST=7
	  INCARB=8
	  NIN=8

	! Otherwise, use the new AgLU model market index structure
	ELSE
      INOIL=1			! 1 = Oil
	  INGAS=2			! 2 = Gas
	  INCOAL=3			! 3 = Coal
	  INBMASS=4			! 4 = Biomass
	  INCARB=5			! 5 = Carbon
	  INFOREST=6		! 6 = Wood
	  INFFOREST=7		! 7 = Forward Wood
	  INFOODGR=8		! 8 = Food Grains
	  INCOARSEGR=9		! 9 = Coarse Grains
	  INOILCROPS=10		! 10 = Oil Crops
	  INMISCCROPS=11	! 11 = Misc Crops
	  INPAST=12			! 12 = Pasture
	  NIN=12
	END IF

!     Utility sector indexes
!	These are read in from ERBINPUT file
!       JUHYDRO = 6
!       JUBMASS=7
!       JUCSCRUB = 8
!       JUOSCRUB = 9
!       JUGSCRUB = 10
!       JUH2GEN = 11
!       JUFUSION = 12


!     Primary fuels index
      IBMASS=7

!     Secondary fuel index
      JSBMASS=5
	  JSH2=6

!     Hydrogen production index
      JHBMASS=4
	  JHELCTRO=5
	  NH2=5       !Number of hydrogen production sectors
	  NNH2=NH2+3  !Number of H2 production sectors + subsectors
	  KH2=5       !Number for H2 in EJKL array

!     Initialize all markets as variable prices

      DO IN=1,NIN
	   DO L=1,NL
	      DO M=1,NM
	         IPFIX(IN,L,M)=0
	      END DO
	   END DO
      END DO   

! Initialize Cement emissions
! If put cement in common block use this.
     Cement(2:9) = (/0.16,0.26,0.32,0.39,0.47,0.52,0.62,0.55/)



!     Since period 1 is not solved, fix its prices

      DO IN=1,NIN
	   DO L=1,NL
            IPFIX(IN,L,1)=1
	   END DO
      END DO   

!     Initilize coal phase-out protocol to off (0)
      IPROTOC=0

! Initialize new fossil supply variable (so that old vers of input files work the same)
	RFosExpan(:,:,:) = 1.0

!	Initialize default flags
!   Don't re-clalibrate base-year values before passing emissions to MAGICC unless calib values are read in
      MagCalb = 0	 !SJS

!	Don't turn on new transportation calcuations unless appropriate variables are read in. sjs 11/00
      NewTransp = 0 

!	Default to "classic" energy demand calculation. 
      ISRESeserDmd = 0  !SJS
      
!	Don't use new labor force percentage method for GDP calcuation
      LFPercMethod = 0  !SJS
      
!	Don't run calibration unless calib data is read in
      DoCalib = 0.  !SJS

!	Default carbon constraint applies only to fossil fuel emissions
      CarbConstraintType = 0.  !SJS

!	Default calibration to primary energy (generally use final energy now)
	FECalib = 0                  

!	Default deforestation emissions are taken from AgLU module
	UserDeforestEm = 0                  

!	Default intensity limits are market based, not PPP-based
	IPPPInten = 0

!   Initiailze to zero
	SRESCALB(:,:) = 0

!	Default is to not include other GHG's in the target, and not get free reductions
	IOTHGASTOG = 0
    IFREEREDUX = 0

!   Default sulfur tech change is zero
	SO2_Cntrl_TechChange(:) = 0

!	Default number of Hydrogen Technologies
	  NH2_New = 0
	  	  
!	Flag to indicate writeout of only some variables
!   0 write out all variables
	  GenRelease = 0

!   Initialize variables for other (non-co2) gases
	OGREPORT = 0.0d0  ! intialize nothing to be computed (toggles to 1 when inputs are read)
	OGCTRLS = 0.0d0  ! initialize all controls to 0
	OGGAMMA = 1.0d0  ! initialize exponent scaling to 1 (direct coef*act multiplication)
	OGCNVRG = -1.0d0 ! turn off convergence as default (turned on if read in case 810)		
	OGGDPPARMS(:,:,4,:) = 0.0d0  !initialize tech change in gdp control to 0
    OGEMISS = 0.0d0 ! 0 emissions

    CCURVADJ = 0.0d0 ! intialize tech change and energy price adjustments to cost curves to 0
    IBZFIRST = 3 ! initialize first cost curve period to 2005

      RETURN
      END
