SUBROUTINE Ag2init(POP,CALP2,popsize)

! Initialization subroutine for the AgLU module

! Created by: Kenny Gillingham 7/10/01

USE Ag2Global8

IMPLICIT NONE

INTEGER T,i,popsize

REAL(8) POP(popsize,NMP+3), CALP2(NLP,10), Mkt_GDP_Calb


CLIMATE = 1.0d0 ! initialize the climate yield multipliers

! Populate the Ag2 arrays with data
CALL Ag2control

! Bring in GDP Data from MiniCAM to overwrite read-in data
! Could calculate GDP here using MiniCAM's labor prod numbers
! GDP in trillions of 1990 dollars
DO i=1, NLP
  DO T=1, NMP
	gdp(i,T) = Mkt_GDP_Calb(i,T)
  END DO
  CALP(i,8) = CALP2(i,8)	! Bring in calib biomass price from the MiniCAM
END DO

! Fill the AgLU's pop array with the MiniCAM's pop data
popu(:,1:NMP) = pop(1:NLP,2:NMP+1)

! Procedure to calculate GDP per capita using MiniCAM data(starting in 1990)
gdpcap(:,2:9) = 1.0d9 * gdp(:,2:9) / popu(:,2:NMP)

! Extend GDP, GDPpercap and Population data for 3 more time steps for fwd forests
CALL Ag2Extend

! Calibrate the AgLU module
CALL Ag2Calibrate

! Send newly calculated calibration prices back to the MiniCAM
CALP2 = CALP

! Initialize tree yields
T = 2
TreeYield(:,T+1) = TreeYield(:,T) * KJ(2,:,T+1)
TreeYield(:,T+2) = TreeYield(:,T) * KJ(2,:,T+2)

BREAK = 1.0d0

END SUBROUTINE Ag2init