	SUBROUTINE ALLOTHERGASES

!	this routine controls computation of all other gas emissions  mj 6/02
	
	USE COMMON

!	the Fortran 90 USE ONLY statement seems like a good way to pass things in from the
!	agriculture model common block (Ag2Global8) for use in various emissions drivers.
    USE Ag2Global8, ONLY: qsup, porkprod, poultryprod, carbemiss, pastout, price, kj, savediet, saveland
!   mj 11/02 (let me know if there are any compiler problems, but I think it is standard)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	INTEGER gas, src
	REAL*8 gdpcap, enprdelta, base_ctrl, applytax, bioprice

	ICH4 = 1
	IN2O = 2
	ISO2 = 3
	INOx = 4
	ICO  = 5
	IVOC = 6
	IBC = 7
	IOC = 8
	ICemt = 9

	! If any of these are changed, change also in MagLink
	IH245 = 10	! HFC245fa equiv
	IH134 = 11	! HFC134a equiv
	IH125 = 12	! HFC125 equiv (including HFC227ea)
	IH143 = 13	! HFC143a equiv
	IHSF6 = 14	! SF6 equiv
	IHC2F6 = 15	! C2F6 equiv
	IHCF4 = 16	! CF4 equiv

	basegwp = (/21.0, 310.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
	790.0, 1300.0, 2800.0, 3800.0, 22200.0, 11900.0, 5700.0, 0.0 , 0.0, &
	0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)

	DO L = 1, NL  ! *** do loop over all regions

! ****************************************************************************************
! Commonly needed computations go here at top
! ****************************************************************************************

    gdpcap = 1000*GNPPPP(L,M)/ZLM(L,M)  !ppp gdp per capita
    bioprice = P(INBMASS,L,M)*CVRT90

! -------------------------------------------------------------------------------------------------
! Estimate the fraction of electric generation that is due to advanced, high efficiency technologies
! Efficiencies above 40% are assumed to be partially due to advanced technologies, such as IGCC
    
    E_max = 0.55	!%at which assume all generation is advanced-cycle
    E_min = max(0.40d0,GenEff(3,L,2))	! make sure 1990 value, if higher than 40%, is not counted as advanced
    
	EqnCoef = (1./E_max)/(1./E_min-1./E_max)
	CoalHighTechFrac = 0.0
	IF (GenEff(3,L,M) .gt. E_min) CoalHighTechFrac = -EqnCoef +EqnCoef*(1./E_min)*GenEff(3,L,M)
	CoalHighTechFrac = min(CoalHighTechFrac,1.d0)
	
    E_min = max(0.40d0,GenEff(1,L,2))	! make sure 1990 value, if off, doesn't trigger extra SO2 removal
	EqnCoef = ((1./E_max)/(1./E_min-1./E_max))  
	OilHighTechFrac = 0.0
	IF (GenEff(1,L,M) .gt. E_min) OilHighTechFrac = -EqnCoef +EqnCoef*(1./E_min)*GenEff(1,L,M)
	OilHighTechFrac = min(OilHighTechFrac,1.d0)

    E_min = 0.40
	EqnCoef = ((1./E_max)/(1./E_min-1./E_max))  
	GasHighTechFrac = 0.0
	IF (GenEff(INGAS,L,M) .gt. E_min) GasHighTechFrac = -EqnCoef +EqnCoef*(1./E_min)*GenEff(INGAS,L,M)
	GasHighTechFrac = min(GasHighTechFrac,1.d0)

! Calculate fraction of fuel used for carbon scrubbed electric generation  
 	CoalScrubFrac = 0
 	OilScrubFrac  = 0
  	GasScrubFrac  = 0

 	IF (EDRIKL(INCOAL,1,L)  .ne. 0) &
 	CoalScrubFrac = ESUILM(JUCSCRUB,L,M)*GUILM(JUCSCRUB,L,M)/EDRIKL(INCOAL,1,L)
 	IF (EDRIKL(INOIL,1,L)  .ne. 0) &
 	OilScrubFrac  = ESUILM(JUOSCRUB,L,M)*GUILM(JUOSCRUB,L,M)/EDRIKL(INOIL,1,L)
 	IF (EDRIKL(INGAS,1,L)  .ne. 0) &
  	GasScrubFrac  = ESUILM(JUGSCRUB,L,M)*GUILM(JUGSCRUB,L,M)/EDRIKL(INGAS,1,L)

! Total fuel used in advanced combustion processes
 	TotAdvCoal = (CoalScrubFrac + (1-CoalScrubFrac)*CoalHighTechFrac)	
 	TotAdvOil  = (OilScrubFrac + (1-OilScrubFrac)*OilHighTechFrac)	
 	TotAdvGas  = (GasScrubFrac + (1-GasScrubFrac)*GasHighTechFrac)

! -------------------------------------------------------------------------------------------------
! Share of end-use liquids supplied by syn fuels. SynShareJILM(1,1,..) is fraction of conventional fossil oil.
    SynuseFract = 1.0 - SynShareJILM(1,1,L,M)
 
! Ag model: fraction of beef (in value terms) that is grain fed (as opposed to pasture)  mj 11/02
	FBeefFrac = 1.0 - PASTOUT(L,M) * price(10,L) / price(3,L)



! ****************************************************************************************
! Activities for CH4
! ****************************************************************************************

! src 1 : industrial
	  OGACT(ICH4,1,L,M) = YLM(L,M) !gdp

! src 2 : mobile
	  OGACT(ICH4,2,L,M) = FJKL(1,3,L) !tran oil

! src 3 : coal_mine
	  OGACT(ICH4,3,L,M) = (ESIL(INCOAL,L) + EDRIL(INCOAL,L))/2  ! AVG prod and cons of coal (to smooth prod problems in emissions)

! src 4 : stationary  (IPCC tier 1 coefs)
	  OGACT(ICH4,4,L,M) = &
		 0.010 * FJKL(1,1,L) &  ! buildings
		+0.005 * FJKL(2,1,L) &
		+0.300 * FJKL(3,1,L) &  ! this is residential coef, would be lower for commercial
		+0.000 * FJKL(5,1,L) &  ! omit -- building biomass is separate source
		+0.002 * FJKL(1,2,L) &  ! industry
		+0.005 * FJKL(2,2,L) &
		+0.010 * FJKL(3,2,L) &
		+0.030 * FJKL(5,2,L) &
		+0.003 * EDRIKL(INOIL,1,L) &  ! electricity generation
		+0.001 * EDRIKL(INGAS,1,L) &
		+0.001 * EDRIKL(INCOAL,1,L) &
		+0.030 * EDRIKL(IBMASS,1,L)

! src 5 : nat_gas
	  OGACT(ICH4,5,L,M) = 0.66*EDRIL(INGAS,L) + 0.33*ESIL(INGAS,L) !nat gas cons + prod (weighted)

! src 6 : pet_sys
	  OGACT(ICH4,6,L,M) = (1.0d0-SynuseFract) * ESIL(INOIL,L) !oil prod

! src 7 : landfills
! expression below is landfilled waste/cap * population
! calibrated to get to 1.5 kilos/cap around 15,000/yr/cap in ppp income
! note that CH4 src 8 and N2O src 3 also depend on this value
	  OGACT(ICH4,7,L,M) = 1.5/(1 + EXP(-4.394492*(gdpcap - 3)/6)) * ZLM(L,M)

! src 8 : wastewater
	  OGACT(ICH4,8,L,M) = ZLM(L,M)  !pop

! src 9 : biofuel
	  OGACT(ICH4,9,L,M) = FJKL(JSBMASS,1,L)  ! buildings biomass

! src 10 : landuse
	  OGACT(ICH4,10,L,M) = MAX(1.0d0,CarbEmiss(L,M))  ! land use emissions (positive)

! src 11 : enteric
	  OGACT(ICH4,11,L,M) = qsup(3,L,M)  ! ag2 beef/mutton

! src 12 : manure
	  OGACT(ICH4,12,L,M) = FBeefFrac*qsup(3,L,M)+5*Porkprod(L,M)+5*poultryprod(L,M) ! ag2 total managed meat (assume all pork/poultry mngd)

! src 13 : rice
	  OGACT(ICH4,13,L,M) = qsup(4,L,M) / KJ(6,L,M) ! ag2 food grains / cumul. tech change food grain prod.

! src 14 : ag_residue
	  OGACT(ICH4,14,L,M) = saveland(1,L,M) ! ag2 total crop area

! src 15 : forestfires
	  OGACT(ICH4,15,L,M) = saveland(3,L,M) ! forest land


! ****************************************************************************************
! Activities for N2O
! ****************************************************************************************


! src 1 : stationary  (relative factors from IPCC Tier 1)
	OGACT(IN2O,1,L,M) = &
	0.0014 * (EDRIKL(INCOAL,1,L)+ FJKL(3,1,L) + FJKL(3,2,L)) &   ! coal (stationary)
   +0.0006 * (EDRIKL(INOIL,1,L) + FJKL(1,1,L) + FJKL(1,2,L))   ! oil (stationary)

! src 2 : mobile
	OGACT(IN2O,2,L,M) = FJKL(1,3,L) !tran oil

! src 3 : sewage
	OGACT(IN2O,3,L,M) = ZLM(L,M) * (savediet(3,l,m)+savediet(9,l,m)+savediet(10,l,m)) !meat calories consumed

! src 4 : adipic_acid
	OGACT(IN2O,4,L,M) = YLM(L,M) !gdp

! src 5 : nitric_acid
	OGACT(IN2O,5,L,M) = YLM(L,M) !gdp

! src 6 : soils
	OGACT(IN2O,6,L,M) = &
	! this for N fixing crops (beans etc.)
	(qsup(6,L,M) + .25*qsup(7,L,M)) * 8.12/10**6 + &
	! this mainly for fertilizer (also counts a little bit of crop residue emiss)
	SUM(qsup(4:7,L,M)) * 3.52/10**6
	! the weights used above are the approximate emissions coefficients for each
	! sub-activity drawn from US inventory methodology.  mj 11/02

! src 7 : manure
	OGACT(IN2O,7,L,M) = FBeefFrac*qsup(3,L,M)+Porkprod(L,M)+poultryprod(L,M) ! ag2 total managed meat (assume all pork/poultry mngd)

! src 8 : biomass
	OGACT(IN2O,8,L,M) = EDILM(IBMASS,L,M) ! total biomass consumption

! src 9 : ag_residue
	OGACT(IN2O,9,L,M) = saveland(1,L,M)  ! ag2 total crop area
	
! src 10 : landuse
	OGACT(IN2O,10,L,M) = MAX(1.0d0,CarbEmiss(L,M))  ! land use emissions (positive)

! src 11 : unmanaged manure
	OGACT(IN2O,11,L,M) = (1-FBeefFrac)*qsup(3,L,M)  ! fraction of beef/mutton not managed (e.g. in pastures)

! src 12 : forestfires
	OGACT(IN2O,12,L,M) = saveland(3,L,M) ! forest land




! ****************************************************************************************
! Activities for SO2
! ****************************************************************************************

!   TO DO


! ****************************************************************************************
! Activities for Cement
! ****************************************************************************************
! New CO2 Driver code for cement -JPL
    CmtGamma = 3;
    CementGDPperCap = 1000*GNPPPP(L,M)/ZLM(L,M) /8

    if( CementGDPperCap > 0.0_8 ) then
        CementDenom = CementGDPperCap**CmtGamma + CementGDPperCap**(-CmtGamma)
    else
        write( *, * ) "GDP Per capita 0 in cement emissions calculation. L: ", L, " M: ", M
        CementDenom = 1_8
    endif

! src 1 : cement demand driver for low GDP
    if( CementGDPperCap > 0.0_8 ) then
        OGACT(ICemt,1,L,M) = CementGDPperCap * CementGDPperCap**(-CmtGamma) /CementDenom
    else 
        OGACT(ICemt,1,L,M) = 0.0_8
    endif

! src 2 : cement demand driver for high GDP
	OGACT(ICemt,2,L,M) = ZLM(L,M) * CementGDPperCap**(CmtGamma) /CementDenom


! ****************************************************************************************
! Activities for High GWP Gases (Note that at the moment only 1 source exists per gas,
! and that this source is just a read in number, and totals from each of 4 large macroregions
! are read into one smaller miniCAM region since the data is not split out)
! ****************************************************************************************
!	1	C2F6
!	2	C4F10
!	3	CF4 
!	4	HFC-125
!	5	HFC-134a
!	6	HFC-143a
!	7	HFC-152a
!	8	HFC-227ea
!	9	HFC-23
!	10	HFC-236fa
!	11	HFC-245ca
!	12	HFC-32
!	13	HFC-43-10
!	14	SF6 

! sjs -- do not use unless SRES emissions are read in
    IF  (SUM(HGWPREAD(3,:,:)) .gt. 0) THEN
	  DO II = 1, 14
		OGACT(10+II,1,L,M) = HGWPREAD(II,L,M)/1000  ! convert from thousand tons to million tons
		OGINPUT(10+II,1,L) = 1 ! set coefficient to 1 since we are passing emissions directly
		OGREPORT(10+II,1) = 1
	  END DO
    END IF

! ***************************************************
!        Activities for NOx
!
! src 1 : conventional coal used for electric utility generation
    OGACT(INOx,1,L,M) = EDRIKL(INCOAL,1,L) * (1-TotAdvCoal) 

! src 2 : Coal used for Advanced (IGCC) power generation 
    OGACT(INOx,2,L,M) = EDRIKL(INCOAL,1,L) * TotAdvCoal 

! src 3 : petroleum oil used for conventional electric utility generation
	OGACT(INOx,3,L,M) = EDRIKL(INOIL,1,L) * (1-TotAdvOil)

! src 4 : petroleum oil used for advanced (IGCC) electric utility generation
	OGACT(INOx,4,L,M) = EDRIKL(INOIL,1,L) * TotAdvOil

! src 5 : natural gas used for conventional electric utility generation
	OGACT(INOx,5,L,M) = EDRIKL(INGAS,1,L) * (1-TotAdvGas)

! src 6 : natural gas used for advanced (IGCC) electric utility generation
	OGACT(INOx,6,L,M) = EDRIKL(INGAS,1,L) * TotAdvGas

! src 7 : coal used for industrial purposes
	OGACT(INOx,7,L,M) = FJKL(3,2,L)

! src 8 : petroleum oil used for industrial purposes
!	OGACT(INOx,8,L,M) = FJKL(1,2,L)
! Correction for uncombusted oil for industrial sector
	J = 1 ! Oil 
	ISEC= 2 ! Industrial sector
	EOilTot = SUM(FJKLM(1,:,L,2)) !Here M is set to 2 (base year)
	EOil_NE_Fract = SFEDIL(J,L)*EOilTot/FJKLM(J,ISEC,L,2)     ! Adjust SFEDIL from fract of total to fract o industrial use
	OGACT(INOx,8,L,M) = FJKL(1,2,L)*(1-EOil_NE_Fract)

! src 9 : natural gas used for industrial purposes
	OGACT(INOx,9,L,M) = FJKL(2,2,L)

! src 10 : wood used for industrial purposes
	OGACT(INOx,10,L,M) =  fjkl(5,2,L)		! corrected -- sjs (index issue)

! src 11 : coal used for residential/commercial building purposes
	OGACT(INOx,11,L,M) = FJKL(3,1,L)

! src 12: petroleum oil used for residential/commercial building purposes
	OGACT(INOx,12,L,M) = FJKL(1,1,L)

! src 13: natural gas used for residential/commercial building purposes
	OGACT(INOx,13,L,M) = FJKL(2,1,L)

! src 14: wood used for residential/commercial building
	OGACT(INOx,14,L,M) =  fjkl(5,1,L)		! corrected -- sjs (index issue)

! src 15: coal used for transportation
	OGACT(INOx,15,L,M) = FJKL(3,3,L)

! src 16: petroleum oil used for transportation
	OGACT(INOx,16,L,M) = FJKL(1,3,L)

! src 17: natural gas used for transportation
	OGACT(INOx,17,L,M) = FJKL(2,3,L)
	
! src 18: wood used for transportation
	OGACT(INOx,18,L,M) = FJKL(5,3,L)

! src 19: agriculture (non burning) - crop production
	OGACT(INOx,19,L, M) = ArPro(L,M)

! src 20: agriculture (non burning) - pasture production
	OGACT(INOx,20,L,M) = PasPro(L,M)
	
! src 21: non-combustion industrial processes
	OGACT(INOx,21,L,M) = GNPMRKT(L,M)**0.25

! src 22: savannah burning
	OGACT(INOx,22,L,M) = grassland(L,M) !drive by amount of grassland

! src 23: deforestation
	OGACT(INOx,23,L,M) = DefroR(bioprice, gdpcap, L,M)

! src 24: agricultural waste
	OGACT(INOx,24,L,M) = saveland(1,L,M) 	! total ag land

!---------------------------------
!       Activities for CO
!---------------------------------

! src 1 : conventional coal used for electric utility generation
    OGACT(ICO,1,L,M) = EDRIKL(INCOAL,1,L)

! src 2 : petroleum oil used for conventional electric utility generation
	OGACT(ICO,2,L,M) = EDRIKL(INOIL,1,L)

! src 3 : natural gas used for conventional electric utility generation
	OGACT(ICO,3,L,M) = EDRIKL(INGAS,1,L)

! src 4 : coal used for industrial purposes
	OGACT(ICO,4,L,M) = FJKL(3,2,L)

! src 5 : petroleum oil used for industrial purposes
!	OGACT(INOx,8,L,M) = FJKL(1,2,L)
! Correction for uncombusted oil for industrial sector
	J = 1 ! Oil 
	ISEC= 2 ! Industrial sector
	EOilTot = SUM(FJKLM(1,:,L,2)) !Here M is set to 2 (base year)
	EOil_NE_Fract = SFEDIL(J,L)*EOilTot/FJKLM(J,ISEC,L,2)     ! Adjust SFEDIL from fract of total to fract o industrial use
	OGACT(ICO,5,L,M) = FJKL(1,2,L)*(1-EOil_NE_Fract)

! src 6 : natural gas used for industrial purposes
	OGACT(ICO,6,L,M) = FJKL(2,2,L)

! src 7 : wood used for industrial purposes
	OGACT(ICO,7,L,M) =  fjkl(5,2,L)		! corrected -- sjs (index issue)

! src 8 : coal used for residential/commercial building purposes
	OGACT(ICO,8,L,M) = FJKL(3,1,L)

! src 9: petroleum oil used for residential/commercial building purposes
	OGACT(ICO,9,L,M) = FJKL(1,1,L)

! src 10: natural gas used for residential/commercial building purposes
	OGACT(ICO,10,L,M) = FJKL(2,1,L)

! src 11: wood used for residential/commercial building
	OGACT(ICO,11,L,M) = fjkl(5,1,L)		! corrected -- sjs (index issue)

! src 12: coal used for transportation
	OGACT(ICO,12,L,M) = FJKL(3,3,L)

! src 13: gasoline/diesel used for transportation
	OGACT(ICO,13,L,M) = FJKL(1,3,L)

! src 14: LPG/Gas used for transportation
	OGACT(ICO,14,L,M) = FJKL(2,3,L)
	
! src 15: non-combustion industrial processes
	OGACT(ICO,15,L,M) = GNPMRKT(L,M)**0.25

! src 16: savannah burning
	OGACT(ICO,16,L,M) = grassland(L,M) !drive by amount of grassland

! src 17: deforestation
	OGACT(ICO,17,L,M) = DefroR(bioprice, gdpcap, L,M)

! src 18: agricultural waste
	OGACT(ICO,18,L,M) =  saveland(1,L,M) 	! total ag land

! --------------------------------
! Activities and drivers for NMVOCs
! -------------------------------------------
!
! src 1 : conventional coal used for electric utility generation
    OGACT(IVOC,1,L,M) = EDRIKL(INCOAL,1,L)

! src 2 : petroleum oil used for conventional electric utility generation
	OGACT(IVOC,2,L,M) = EDRIKL(INOIL,1,L)

! src 3 : natural gas used for conventional electric utility generation
	OGACT(IVOC,3,L,M) = EDRIKL(INGAS,1,L)

! src 4 : coal used for industrial purposes
	OGACT(IVOC,4,L,M) = FJKL(3,2,L)

! src 5 : petroleum oil used for industrial purposes
!	OGACT(INOx,8,L,M) = FJKL(1,2,L)
! Correction for uncombusted oil for industrial sector
	J = 1 ! Oil 
	ISEC= 2 ! Industrial sector
	EOilTot = SUM(FJKLM(1,:,L,2)) !Here M is set to 2 (base year)
	EOil_NE_Fract = SFEDIL(J,L)*EOilTot/FJKLM(J,ISEC,L,2)     ! Adjust SFEDIL from fract of total to fract o industrial use
	OGACT(IVOC,5,L,M) = FJKL(1,2,L)*(1-EOil_NE_Fract)

! src 6 : natural gas used for industrial purposes
	OGACT(IVOC,6,L,M) = FJKL(2,2,L)

! src 7 : wood used for industrial purposes
	OGACT(IVOC,7,L,M) =  fjkl(5,2,L)		! corrected -- sjs (index issue)

! src 8 : coal used for residential/commercial building purposes
	OGACT(IVOC,8,L,M) = FJKL(3,1,L)

! src 9: petroleum oil used for residential/commercial building purposes
	OGACT(IVOC,9,L,M) = FJKL(1,1,L)

! src 10: natural gas used for residential/commercial building purposes
	OGACT(IVOC,10,L,M) = FJKL(2,1,L)

! src 11: wood used for residential/commercial building
	OGACT(IVOC,11,L,M) =  fjkl(5,1,L)		! corrected -- sjs (index issue)

! src 12: coal used for transportation
	OGACT(IVOC,12,L,M) = FJKL(3,3,L)

! src 13: gasoline/diesel used for transportation
	OGACT(IVOC,13,L,M) = FJKL(1,3,L)

! src 14: LPG/Gas used for transportation
	OGACT(IVOC,14,L,M) = FJKL(2,3,L)

! src 15: Fossil fuel production: oil
    
	OilCons=EDRIKL(INOIL,1,L)+FJKL(1,2,L)+FJKL(1,1,L)+FJKL(1,3,L)
	GasCons=EDRIKL(INGAS,1,L)+FJKL(2,2,L)+FJKL(2,1,L)+FJKL(2,3,L)
	ToTCons=OilCons+GasCons
	ToTProd=ESIL(INCOAL,L)+ESIL(INOIL,L)+ESIL(INGAS,L)

	OGACT(IVOC,15,L,M) = (TotCons+TotProd)/2

! src 16: non-combustion industrial processes
	OGACT(IVOC,16,L,M) = GNPMRKT(L,M)**0.25

! src 17: savannah burning
	OGACT(IVOC,17,L,M) = grassland(L,M) !drive by amount of grassland

! src 18: deforestation
	OGACT(IVOC,18,L,M) = DefroR(bioprice, gdpcap, L,M)
!	write(1001,*) DefroR(bioprice, gdpcap, L,M),L,M

! src 19: agricultural waste
	OGACT(IVOC,19,L,M) =  saveland(1,L,M) 	! total ag land


! ****************************************************************************************
! Activities for Black Carbon -- sjs
! ****************************************************************************************

!        Activities for BC
!
! src 1 : conventional coal used for electric utility generation
    OGACT(IBC,1,L,M) = EDRIKL(INCOAL,1,L) * (1-TotAdvCoal) 

! src 2 : petroleum oil used for conventional electric utility generation
	OGACT(IBC,2,L,M) = EDRIKL(INOIL,1,L) * (1-TotAdvOil)

! src 3 : natural gas used for conventional electric utility generation
	OGACT(IBC,3,L,M) = EDRIKL(INGAS,1,L) * (1-TotAdvGas)

! src 4 : coal used for industrial purposes
	OGACT(IBC,4,L,M) = FJKL(3,2,L)

! src 5 : petroleum oil used for industrial purposes
!	OGACT(IBC,5,L,M) = FJKL(1,2,L)
! Correction for uncombusted oil for industrial sector
	J = 1 ! Oil 
	ISEC= 2 ! Industrial sector
	EOilTot = SUM(FJKLM(1,:,L,2)) !Here M is set to 2 (base year)
	EOil_NE_Fract = SFEDIL(J,L)*EOilTot/FJKLM(J,ISEC,L,2)     ! Adjust SFEDIL from fract of total to fract o industrial use
	OGACT(IBC,5,L,M) = FJKL(1,2,L)*(1-EOil_NE_Fract)

! src 6 : natural gas used for industrial purposes
	OGACT(IBC,6,L,M) = FJKL(2,2,L)

! src 7 : wood used for industrial purposes
	OGACT(IBC,7,L,M) =  fjkl(5,2,L)		! corrected -- sjs (index issue)

! src 8 : coal used for residential/commercial building purposes
	OGACT(IBC,8,L,M) = FJKL(3,1,L)

! src 9: petroleum oil used for residential/commercial building purposes
	OGACT(IBC,9,L,M) = FJKL(1,1,L)

! src 10: natural gas used for residential/commercial building purposes
	OGACT(IBC,10,L,M) = FJKL(2,1,L)

! src 11: wood used for residential/commercial building
	OGACT(IBC,11,L,M) =  fjkl(5,1,L)		! corrected -- sjs (index issue)

! src 12: coal used for transportation
	OGACT(IBC,12,L,M) = FJKL(3,3,L)

! src 13: petroleum oil used for transportation
	OGACT(IBC,13,L,M) = FJKL(1,3,L)

! src 14: natural gas used for transportation
	OGACT(IBC,14,L,M) = FJKL(2,3,L)

! src 15: wood used for transportation
	OGACT(IBC,15,L,M) = FJKL(5,3,L)

! src 16: savannah burning
	OGACT(IBC,16,L,M) = grassland(L,M) !drive by amount of grassland

! src 17: deforestation
	OGACT(IBC,17,L,M) = DefroR(bioprice, gdpcap, L,M)
!	OGACT(IBC,17,L,M) = 1.0

! src 18: forest fires
	OGACT(IBC,18,L,M) = saveland(3,L,M) + forestFract(L) * saveland(5,L,M)	!total forests

! src 19: agricultural waste
	OGACT(IBC,19,L,M) =  saveland(1,L,M) 	! total ag land
!	OGACT(IBC,19,L,M) =  1.0

!
!
!***********************************************************
!




! ****************************************************************************************
! Activities for Organic Carbon -- sjs
! ****************************************************************************************

!        Activities for BC
!
! src 1 : conventional coal used for electric utility generation
    OGACT(IOC,1,L,M) = EDRIKL(INCOAL,1,L) * (1-TotAdvCoal) 

! src 2 : petroleum oil used for conventional electric utility generation
	OGACT(IOC,2,L,M) = EDRIKL(INOIL,1,L) * (1-TotAdvOil)

! src 3 : natural gas used for conventional electric utility generation
	OGACT(IOC,3,L,M) = EDRIKL(INGAS,1,L) * (1-TotAdvGas)

! src 4 : coal used for industrial purposes
	OGACT(IOC,4,L,M) = FJKL(3,2,L)

! src 5 : petroleum oil used for industrial purposes
!	OGACT(IOC,5,L,M) = FJKL(1,2,L)
! Correction for uncombusted oil for industrial sector
	J = 1 ! Oil 
	ISEC= 2 ! Industrial sector
	EOilTot = SUM(FJKLM(1,:,L,2)) !Here M is set to 2 (base year)
	EOil_NE_Fract = SFEDIL(J,L)*EOilTot/FJKLM(J,ISEC,L,2)     ! Adjust SFEDIL from fract of total to fract o industrial use
	OGACT(IOC,5,L,M) = FJKL(1,2,L)*(1-EOil_NE_Fract)

! src 6 : natural gas used for industrial purposes
	OGACT(IOC,6,L,M) = FJKL(2,2,L)

! src 7 : wood used for industrial purposes
	OGACT(IOC,7,L,M) =  fjkl(5,2,L)		! corrected -- sjs (index issue)

! src 8 : coal used for residential/commercial building purposes
	OGACT(IOC,8,L,M) = FJKL(3,1,L)

! src 9: petroleum oil used for residential/commercial building purposes
	OGACT(IOC,9,L,M) = FJKL(1,1,L)

! src 10: natural gas used for residential/commercial building purposes
	OGACT(IOC,10,L,M) = FJKL(2,1,L)

! src 11: wood used for residential/commercial building
	OGACT(IOC,11,L,M) =  fjkl(5,1,L)		! corrected -- sjs (index issue)

! src 12: coal used for transportation
	OGACT(IOC,12,L,M) = FJKL(3,3,L)

! src 13: petroleum oil used for transportation
	OGACT(IOC,13,L,M) = FJKL(1,3,L)

! src 14: natural gas used for transportation
	OGACT(IOC,14,L,M) = FJKL(2,3,L)

! src 15: wood used for transportation
	OGACT(IOC,15,L,M) = FJKL(5,3,L)

! src 16: savannah burning
	OGACT(IOC,16,L,M) = grassland(L,M) !drive by amount of grassland

! src 17: deforestation
	OGACT(IOC,17,L,M) = DefroR(bioprice, gdpcap, L,M)
!	OGACT(IOC,17,L,M) = 1.0

! src 18: forest fires
	OGACT(IOC,18,L,M) = saveland(3,L,M) + forestFract(L) * saveland(5,L,M)	!total forests

! src 19: agricultural waste
	OGACT(IOC,19,L,M) =  saveland(1,L,M) 	! total ag land
!	OGACT(IOC,19,L,M) =  1.0


!
!
!
!***********************************************************
!

! ****************************************************************************************
! Activities for Halocarbons & High GWP gases -- sjs
! ****************************************************************************************


! CoolDemand(L) = cooling degree days per unit population

!        Activities for Halocarbons, etc.

! HFC245fa equiv
! src 1 : foams
    OGACT(IH245,1,L,M) = GNPPPP(L,M)/1e6
    

! HFC134a equiv
! src 1 : Aerosols
    OGACT(IH134,1,L,M) = ZLM(L,M)/1e6
    
! src 2 : Solvents
    OGACT(IH134,2,L,M) = GNPPPP(L,M)/1e6
    
! src 3 : Transport AC
    OGACT(IH134,3,L,M) = sum(FJKL(1:NNJ,3,L))/1e4	! could replace this with transportation service demand
    
! src 4 : Commertial Buildings AC
    OGACT(IH134,4,L,M) = ZLM(L,M) * CoolDemand(L)/1e9
    

! HFC125 equiv
! src 1 : Fire Extinquishers
    OGACT(IH125,1,L,M) = ZLM(L,M)/1e9
    
! src 2 : Commertial Buildings AC
    OGACT(IH125,2,L,M) = ZLM(L,M) * CoolDemand(L)/1e9
    
! src 3 : Residential Buildings AC
    OGACT(IH125,3,L,M) = ZLM(L,M) * CoolDemand(L)/1e9
    
! src 4 : Food Distribution and Household Appliances
    OGACT(IH125,4,L,M) = ZLM(L,M) * SaveDiet(8,L,M)/1e9	! processed food demand
    

! HFC143a equiv
! src 1 : Food Distribution and Household Appliances
    OGACT(IH143,1,L,M) = ZLM(L,M) * SaveDiet(8,L,M)/1e9	! processed food demand


! SF6 equiv
! src 1 : Electric transformers, T&D
    OGACT(IHSF6,1,L,M) = (SUM(ESILM(4:6,L,M))+ESILM(JUFUSION,L,M) &
             +ESILM(JUWIND,L,M)+ESILM(JUWIND+1,L,M)+ESILM(JUWIND+2,L,M))/1e4
    
! src 2 : HCFC-22 Production
    OGACT(IHSF6,2,L,M) = GNPPPP(L,M)/1e6	! HCFC-22 future use primarilly as feedstock chemical

! C2F6 equiv
! src 1 : Semiconductors
    OGACT(IHC2F6,1,L,M) = SUM(GNPPPP(:,M))/1e9
    
! CF4 equiv
! src 1 : Aluminum & Magnesium Smelting
    OGACT(IHCF4,1,L,M) = GNPPPP(L,M)/1e6
    
! src 2 : Solvents
    OGACT(IHCF4,2,L,M) = GNPPPP(L,M)/1e6

!
!
!***********************************************************

! Following code is a big loop calling the common emissions code for each gas and source

! Three types of emissions "controls" are possible. All three are contained in the array, OGCTRLS.
! 1: OGCTRLS(gas, src, 1, L, M) -- controls based on a PPP logistic curve
! 2: OGCTRLS(gas, src, 2, L, M) -- ghg control curves from user
! 3: OGCTRLS(gas, src, 3, L, M) -- user input "controls", representing calibration factors, efficiency changes, etc.


     if (L .eq. 111) THEN
        src = 1
		gas = IH134
        write(*,'(a,15(f7.3,","))') "1: ",ZLM(L,M)/1e6,OGCOEF(gas,src,L), &
		OGEMISS(gas,src,L,M), OGACT(gas,src,L,M), OGTYPE(gas,src,L), OGINPUT(gas,src,L),&
		OGCTRLS(gas, src, 1, L, M),OGCTRLS(gas, src, 2, L, M),OGCTRLS(gas, src, 3, L, M)
      end if


	DO gas = 1, NOGMax
		DO src = 1, NOGSrcMax

			IF (OGREPORT(gas,src).EQ.0) CYCLE

			! call common code to get control levels:

			OGCTRLS(gas, src, 1, L, M) = GDP_CTRL( &
			  gdpcap, OGGDPPARMS(gas,src,1,L), OGGDPPARMS(gas,src,2,L), OGGDPPARMS(gas,src,3,L), OGGDPPARMS(gas,src,4,L), 15.0d0*(M-2))
			  
			IF (OGCNVRG(gas,src,L).GT.0.AND.M.EQ.2) OGCTRLS(gas, src, 1, L, M) = 0.0d0 !Make sure that if M=2, no convergence controls

			IF (OGCNVRG(gas,src,L).GT.0.AND.M.GT.2) THEN  ! gdp control used for convergence
				! we want to scale the gdp control so that it is 0 in the base period, but has the same fmax
				! formula below is: (current_ctrl-base_ctrl)/(fmax-base_ctrl) * fmax
				
				base_ctrl = GDP_CTRL(1000*GNPPPP(L,2)/ZLM(L,2), & ! must recompute since zeroed above
				OGGDPPARMS(gas,src,1,L), OGGDPPARMS(gas,src,2,L), OGGDPPARMS(gas,src,3,L), OGGDPPARMS(gas,src,4,L), 0.0d0)

				IF (OGGDPPARMS(gas,src,1,L).GT.0) & ! avoid divide by zero error  mj 8/02
				OGCTRLS(gas, src, 1, L, M) = OGGDPPARMS(gas,src,1,L) * &
					(OGCTRLS(gas, src, 1, L, M) - base_ctrl) / &
					(OGGDPPARMS(gas,src,1,L) - base_ctrl)
			END IF

			!if this is co2 only, do not run cost curves
			IF (IOTHGASTOG.EQ.0.AND.IFREEREDUX.EQ.0) THEN
				OGCTRLS(gas,src,2,L,M) = 0.0d0
			ELSE
			!enprdelta is price change in gas converted to TCE scaled by the CCURVADJ factor
			!used to move methane cost curves dynamically with energy prices mj 11/02
			    enprdelta = CCURVADJ(gas,src,3,L) * 9.1 * &
				           (PILM(INGAS,L,M)-PILM(INGAS,L,2))*CVRT90
				IF (IOTHGASTOG.GE.1) THEN
				   applytax = P(INCARB,L,M)
				ELSE 
				   applytax = 0.0d0 !apply 0 tax if other ghgs not in limit
				END IF
				OGCTRLS(gas, src, 2, L, M) = ABATECURVE_CTRL( &
				  M,applytax, CCURV(gas,src,:,1,L), CCURV(gas,src,:,2,L),	 &
				  NLEVCCURV(gas,src,L), IBZFIRST-1, IBZFIRST-1 + IBZSTEPS, 1, OGGWP(gas,M)/basegwp(gas), &
				  enprdelta, CCURVADJ(gas,src,1,L), CCURVADJ(gas,src,2,L) )
			END IF
 
			! call common code to generate emissions

			CALL GAS_EMISS ( &
			  OGEMISS(gas,src,L,M), OGACT(gas,src,L,M), M, 2, OGTYPE(gas,src,L), OGINPUT(gas,src,L), &
			  OGCOEF(gas,src,L), OGCTRLS(gas,src,:,L,M), OGGAMMA(gas,src,L), OGERROR(gas,src,L))

			! code to converge coefficients using GDP control
			IF (OGCNVRG(gas,src,L).GT.0.AND.M.EQ.2) THEN  ! if converge-to coeff read
				IF (OGCOEF(gas,src,L).LE.0) THEN  ! case of no data for base emissions mj 8/02
					OGCOEF(gas,src,L) = OGCNVRG(gas,src,L) ! just use the converge-coef directly
				ELSE 
					! this just sets the fmax parameter (can be negative):
					OGGDPPARMS(gas,src,1,L) = (OGCOEF(gas,src,L)-OGCNVRG(gas,src,L))/OGCOEF(gas,src,L)
				END IF
			END IF
		END DO
	END DO

     if (L .eq. 111) THEN
        src = 1
		gas = IH134
        write(*,'(a,15(f7.3,","))') "2: ", ZLM(L,M)/1e6,OGCOEF(gas,src,L), &
		OGEMISS(gas,src,L,M), OGACT(gas,src,L,M), OGTYPE(gas,src,L), OGINPUT(gas,src,L)
      end if

! *** End do loop over regions ***
	END DO

	RETURN
	END


! ***************************************
! ************* Added by Jigme **********
! *** Function to call activity level for emission for non-conbustion agri-processes ***
	
	FUNCTION ArPro(L,M)	! total ag production
	
	USE Ag2Global8

	REAL*8 ArPro
	INTEGER L,M
	
	    ArPro = AgP(4,L,M)+AgP(5,L,M)+AgP(6,L,M)+AgP(7,L,M)

	RETURN
	END
	
!----------------------

	FUNCTION PasPro(L,M)	! total pasture production
	
	USE Ag2Global8

	REAL*8 PasPro
	INTEGER L,M
	
	PasPro = AgP(3,L,M)   
	
	RETURN
	END
!
!-------------------------
! activity level for deforestation emissions
! = above ground forest + unmanaged biomass change, converted to energy units
! so as to be able to compare emissions
! modified by a function that takes into account use of biomass for energy

	FUNCTION DefroR(biomassprice, gdppercap, L,M)

	USE Ag2Global8
	
	REAL*8 DefroR, gdppercap
	INTEGER L,M

	REAL*8 CarbDensity, EnergyDensity, UseFract, priceFactor, maxUse
    
    CarbDensity = 0.5 ! Assume half of biomass weight is carbon
    EnergyDensity = 17.5 ! GJ/Tonne
    
	DefroR = 0
	If (M.gt.1) then
		if (EmPart(4,L) .gt. 0) DefroR = EmPart(4,L)
		if (EmPart(5,L) .gt. 0) DefroR = DefroR + EmPart(5,L)
    endif

	! Deforested biomass in energy terms
	DefroR = DefroR/CarbDensity * EnergyDensity / 1000
	
	! Now adjust for amount harvested for energy
	
	UseFract = (1d0 - recovForestFrac(L) )	! Basic amount that can be potentially used
	
	UseFract = (gdppercap/15) * UseFract	! Assume that less is used at low incomes

	! Assume that none is used at a low biomass price ($1/GJ), maxing out at $5/GJ
	maxUse = 0.90	! Assume that 90% of the usable biomass is used if price is high enough
	
	priceFactor =  ( biomassprice - 1d0 ) / (5d0 - 1d0) * maxUse
	if (priceFactor .gt. 1d0) priceFactor = maxUse
	if (priceFactor .lt. 0) priceFactor = 0
	
	UseFract = UseFract	* priceFactor
	
	DefroR = DefroR * (1d0 - UseFract)
	
	RETURN
	END
	
!
!-------------------------
!  total grassland

	FUNCTION grassland(L,M)

	USE Ag2Global8
	
	REAL*8 TotalLand, grassL, newAgLand, baseAgLand
	INTEGER L,M
    
	Saveland(5,L,1)=HistLand(L,9)
	If (M.gt.1) then
		TotalLand = sum( Saveland(1:5,L,M) )
		baseAgLand = Saveland(1,L,1)+Saveland(2,L,1)+Saveland(4,L,1)
		newAgLand = Saveland(1,L,M)+Saveland(2,L,M)+Saveland(4,L,M)
				
		grassL = TotalLand * fractSavannah(L)
		grassL = grassL - fractPotentialAgSavannah(L) * (newAgLand - baseAgLand)
		
		if (grassL.lt.0.0) grassL=0.0
    endif
	grassland = grassL	

	RETURN
	END

!
!-------------------------
!  return fraction of unmanaged land that is forest

	FUNCTION forestFract(L)

	USE Ag2Global8
	
	forestFract = unmanForestFrac(L)	

	RETURN
	END