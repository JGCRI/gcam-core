SUBROUTINE Ag2Emiss(T,CarbEmiss1)

USE Ag2Global8

IMPLICIT NONE

REAL(8) CarbEmiss1(NLP,NMP)
REAL(8) CarbEmissSave(4,NLP,NMP)
REAL(8) deforUseMult, biomassprice, deforBioUseFract

INTEGER i,T

! Start in period 3 for everything but forests.
! Carbon emissions are annualized by dividing by STEP, the
! number of years in a time step. 
! We could start forests in period 2 because we are continuously
! cutting and planting during each time period.

! Land use carbon emissions from soils are first calculated for the 
! preceeding time periods (1960 and 1975) so they can be shared out
! over the next few time periods

CarbEmiss(:,T) = 0.0d0
SoilEmiss(1,:,T) = 0.0d0

! Find the 1975 emissions (No biomass in 1975)
IF (T .EQ. 2) THEN
  DO i=1,NL

	SoilEmiss(1,i,1) = 0.0d0	! Initialize soil emissions

  ! 1975 Soil Emissions (to be shared out to 1990 & 2005)
	! Crops
    SoilEmiss(1,i,1) = SoilEmiss(1,i,1) - CDensity(i,6) *  &
                    (HistLand(i,6) - HistLand(i,1)) / STEP
	! Pasture
    SoilEmiss(1,i,1) = SoilEmiss(1,i,1) - CDensity(i,8) *  &
                    (HistLand(i,7) - HistLand(i,2)) / STEP
	! Forests (which we could have started in period 2)
    SoilEmiss(1,i,1) = SoilEmiss(1,i,1) - CDensity(i,9) *  &
                    (HistLand(i,8) - HistLand(i,3)) / STEP
	! Unmanaged land
    SoilEmiss(1,i,1) = SoilEmiss(1,i,1) - CDensity(i,10) *  &
                    (HistLand(i,9) - HistLand(i,4)) / STEP

	CALL SoilDecay(i,1)

  ! 1975 Vegetation Emissions (when T=1)
	! Crops
    CarbEmiss(i,1) = CarbEmiss(i,1) - CDensity(i,1) *  &
                    (HistLand(i,6) - HistLand(i,1)) / STEP
	! Pasture
    CarbEmiss(i,1) = CarbEmiss(i,1) - CDensity(i,3) *  &
                    (HistLand(i,7) - HistLand(i,2)) / STEP
	! Forests (which we could have started in period 2)
    CarbEmiss(i,1) = CarbEmiss(i,1) - CDensity(i,4) *  &
                     (HistLand(i,8) - HistLand(i,3)) / STEP
	! Unmanaged land
    CarbEmiss(i,1) = CarbEmiss(i,T) - CDensity(i,5) *  &
                    (HistLand(i,9) - HistLand(i,4)) / STEP

  ! Total 1975 LUC CO2 Emissions
	CarbEmiss(i,1) = CarbEmiss(i,1) + SoilEmiss(2,i,1)

  ! Fill in the SaveLand array with 1975 historical land data	
	SaveLand(1,i,1) = HistLand(i,6)	! crops
	SaveLand(2,i,1) = HistLand(i,7) ! pasture
	SaveLand(3,i,1) = HistLand(i,8) ! forests
	SaveLand(4,i,1) = 0.0d0			! biomass
	SaveLand(5,i,1) = HistLand(i,9) ! unmgd

  END DO
END IF

! Rest of the time periods
DO i=1,NL

  ! Soil Emissions
	! Crops
    SoilEmiss(1,i,T) = SoilEmiss(1,i,T) - CDensity(i,6) *  &
                    (SaveLand(1,i,T) - SaveLand(1,i,T-1)) / STEP
	! Biomass
	SoilEmiss(1,i,T) = SoilEmiss(1,i,T) - CDensity(i,7) * &
                    (SaveLand(4,i,T) - SaveLand(4,i,T-1)) / STEP					
	! Pasture
    SoilEmiss(1,i,T) = SoilEmiss(1,i,T) - CDensity(i,8) *  &
                    (SaveLand(2,i,T) - SaveLand(2,i,T-1)) / STEP
	! Forests (which we could have started in period 2)
    SoilEmiss(1,i,T) = SoilEmiss(1,i,T) - CDensity(i,9) *  &
                    (SaveLand(3,i,T) - SaveLand(3,i,T-1)) / STEP
	! Unmanaged land
    SoilEmiss(1,i,T) = SoilEmiss(1,i,T) - CDensity(i,10) *  &
                    (SaveLand(5,i,T) - SaveLand(5,i,T-1)) / STEP

	CALL SoilDecay(i,T)

	! Reduce deforestation above ground by half bio use 
	! This is a crude approximation to displacing some fossil and some non-fossil
	biomassprice = price(8,i) / GJperGcal
	deforUseMult = 1d0 - deforBioUseFract(biomassprice, gdpcap(i,t)*2.12/1000d0, i,T)/2
	
  ! Vegetation Emissions
	! Crops
       EmPart(1,i) =  - CDensity(i,1) *  &
                    (SaveLand(1,i,T) - SaveLand(1,i,T-1)) / STEP
	! Biomass
       EmPart(2,i) =  - CDensity(i,2) *  &
                    (SaveLand(4,i,T) - SaveLand(4,i,T-1)) / STEP
	! Pasture
       EmPart(3,i) =  - CDensity(i,3) *  &
                    (SaveLand(2,i,T) - SaveLand(2,i,T-1)) / STEP
	! Forests (which we could have started in period 2)
       EmPart(4,i) =  - CDensity(i,4) *  &
                    (SaveLand(3,i,T) - SaveLand(3,i,T-1)) / STEP
	! Unmanaged land
       EmPart(5,i) =  - CDensity(i,5) *  &
                    (SaveLand(5,i,T) - SaveLand(5,i,T-1)) / STEP

	! Add emissions to total, discounting approximately for forest biomass used for fuel
    CarbEmiss(i,T) = CarbEmiss(i,T) + SUM(EmPart(1:3,i))
    CarbEmiss(i,T) = CarbEmiss(i,T) + SUM(EmPart(4:5,i)) * deforUseMult
    

   CarbEmissSave(1,i,T) = CarbEmiss(i,T)
   CarbEmissSave(2:4,i,T) = SoilEmiss(2:4,i,T)
   
  ! Total LUC CO2 Emissions
	CarbEmiss(i,T) = CarbEmiss(i,T) + SoilEmiss(2,i,T) + SoilEmiss(3,i,T-1)
	IF (T .GT. 2) CarbEmiss(i,T) = CarbEmiss(i,T) + SoilEmiss(4,i,T-2)

	CarbEmiss1(i,T) = CarbEmiss(i,T)	! To be passed back to the MiniCAM

END DO

IF (T .gt. 33) THEN
Write (*,'(55(" "),"LUC Components: ",10(f5.1,","))') &
		SUM(CarbEmissSave(1,1:14,T))/1000, SUM(SoilEmiss(2,1:14,T))/1000, &
		SUM(SoilEmiss(3,1:14,T-1))/1000,SUM(SoilEmiss(4,1:14,T-2))/1000
Write (*,'(55(" "),"LUC Components: ",10(f5.1,","))') &
		SUM(EmPart(:,:),DIM=2)/1000
Write (*,'(55(" "),"Land Uses     : ",10(f6.0,","))') &
		SUM(SaveLand(1,1:14,T))/1000,SUM(SaveLand(4,1:14,T))/1000,SUM(SaveLand(2,1:14,T))/1000, &
		SUM(SaveLand(3,1:14,T))/1000,SUM(SaveLand(5,1:14,T))/1000
Write (*,'(55(" "),"Prod (Wood,FoodGr,CGr,Beef) : ",10(f6.0,","))') &
		SUM(qsup(1,1:14,T))/1000,SUM(qsup(4,1:14,T))/1000,SUM(qsup(5,1:14,T))/1000,&
		(SUM(AgFD(3,1,1:14,T))+SUM(AgFD(3,2,1:14,T)))/1000
		
END IF



BREAK = 1.0d0

END SUBROUTINE Ag2Emiss

! ===================================================================

SUBROUTINE SoilDecay(i,T)

! This subroutine determines the shape of the soil C decay curve

! Carbon emissions from soils are currently shared out over the
! time period the destruction occurs and the next 2 time periods
! This amounts to a 30 year primary C content to min C content time frame
! following the data in R. Houghton's paper

USE Ag2Global8

IMPLICIT NONE

INTEGER i,T

! Simple modeling of soil C emissions now.
! This could be developed to fit an exponential curve better

SoilEmiss(2,i,T) = SoilEmiss(1,i,T) * 0.60d0	! Emissions from when the land use change occurs
SoilEmiss(3,i,T) = SoilEmiss(1,i,T) * 0.30d0	! Emissions the next time period
SoilEmiss(4,i,T) = SoilEmiss(1,i,T) * 0.10d0	! Emissions the 2nd time period (30 years after)


END SUBROUTINE SoilDecay


!
!-------------------------
! Function returns amount of deforested biomass that is used for fuel

	FUNCTION deforBioUseFract(biomassprice, gdppercap, L,M)

	USE Ag2Global8
	
	REAL*8 deforBioUseFract, gdppercap, biomassprice
	INTEGER L,M

	REAL*8 UseFract, priceFactor, maxUse, gdpFact, gdpSplitPoint
    
	UseFract = recovForestFrac(L)	! Basic amount that can be potentially used
	
	gdpFact = 1d0
	gdpSplitPoint = gdppercap_point(L)
	if (gdppercap .lt. gdpSplitPoint ) then
		gdpFact = (gdppercap/gdpSplitPoint) 	! Assume that less is used at low incomes
	endif
	UseFract = UseFract	* gdpFact

	! Assume that none is used at a low biomass price ($1/GJ), maxing out at $5/GJ
	maxUse = 0.90	! Assume that 90% of the usable biomass is used if price is high enough
	
	priceFactor =  ( biomassprice - 1d0 ) / (5d0 - 1d0) * maxUse
	if (priceFactor .gt. 1d0) priceFactor = maxUse
	if (priceFactor .lt. 0) priceFactor = 0
	
	UseFract = UseFract	* priceFactor
	
	If (DeforBioUse .eq. 0) UseFract = 0

	deforBioUseFract = UseFract
	
	if (L .eq. 100) then
		write(97,'(3I6,10(f6.2,", "))') M,L,DeforBioUse,UseFract, gdpFact, priceFactor, recovForestFrac(L)
	!	write(97,'("  -- Pbio,Pop",10(f6.2,", "))') biomassprice, popu(10,m)
		write(97,*) "GDP per cap",gdppercap
	endif
	RETURN
	END

