
! This module is meant to contain subroutines and functions that are
! of common use to the generation of gas emissions.
! Therefore, it should not contain references to common modules.  mj 7/02


! Three types of emissions "controls" are possible. All three are contained in the array, OGCTRLS.
! 1: OGCTRLS(gas, src, 1, L, M) -- controls based on a PPP logistic curve
! 2: OGCTRLS(gas, src, 1, L, M) -- ghg control curves driven by gdp
! 3: OGCTRLS(gas, src, 1, L, M) -- user input "controls", representing calibration factors, efficiency changes, etc.



	SUBROUTINE GAS_EMISS(emiss, act, period, base_period, input_type, input, &
	                 coef, ctrls, gamma, errcode)


	REAL*8, INTENT(INOUT) :: emiss, act, input, coef, ctrls(3), gamma
	INTEGER, INTENT(INOUT) :: period, base_period, input_type, errcode
	CHARACTER*80 MsgStr

	emiss = 0.0d0  ! initialize emissions
	errcode = 0 ! initialize error reporting


    IF (period.EQ.base_period) THEN !compute implied coefficient

		IF (input_type.EQ.0) THEN  !straight coefficient (need to account for gamma)
			coef = input**(1/gamma) * act**((1d0-gamma)/gamma)
		END IF

		IF (input_type.EQ.1) THEN  !base year emissions
			coef = input**(1/gamma) / act
			IF (act.EQ.0.0.AND.input.GT.0.0) THEN
				errcode = 1  ! 0 activity error
				coef = 0.0d0
			END IF
			IF (input.EQ.0.0) coef = 0.0d0  ! missing data
		END IF

	END IF


	IF (coef.EQ.0) RETURN  ! just return 0 if no coefficient

	IF (act.LT.0) THEN  ! negative activity error check
		MsgStr = "Negative Emissions Driver"
		Call MCLog(1,MsgStr,period,0,0,act)
		RETURN
	END IF

	
	
	emiss = (coef * act) ** gamma  ! do the actual simple multiplication

	! account for the three control types:

	emiss = emiss * (1-ctrls(1)) * (1-ctrls(2)) * (1-ctrls(3))


	RETURN
	END





	REAL*8 FUNCTION GDP_CTRL (gdpcap, fmax, gdp0, tau, techchangepct, techchangeyears)

!***********************************************************************
!	
!	returns an exponential logistic based "control" fraction   coded 6/02 mj
!
!	gdpcap is gdp per capita (in thousands of dollars / year)
!	fmax   is the maximum value of the function
!	       the minimum value is 0
!   gdp0   is the midpoint gdp where gdp_ctrl = fmax/2
!	tau    controls the slope of the logisitic
!   techchangepct  annual percent rate of decline of gdp0
!   techchangeyears  number of years decline at this rate
!   
!
!***********************************************************************

	REAL*8 gdpcap, fmax, gdp0, tau, techchangepct, techchangeyears
	REAL*8 techchange

	GDP_CTRL = 0.0d0  ! initialize

	IF (tau.LE.0.0) RETURN

    techchange = (1+techchangepct/100.)**techchangeyears    


	GDP_CTRL = fmax / (1 + EXP(-4.394492*(gdpcap-gdp0/techchange)/tau))
 
	RETURN
	END



	
	REAL*8 FUNCTION ABATECURVE_CTRL (period,taxin,curvetaxin,curvereduxin, numpoints, &
									 availpd,fullpd,phasein,gwpadjust,enprdelta,tc1,tc2)


!***********************************************************************
!	
!	this function will process an abatement cost curve and parameters
!	to a percent reduction in emissions.   mj 6/02
!   note that the 15 point limit is hard coded since no access to common block
!
!***********************************************************************

	IMPLICIT NONE

	INTEGER period, availpd, fullpd, ipoint, numpoints, phasein, zpoint
	Real*8 tempPeriod
	REAL*8 curvetax(15), curveredux(15), shiftper, tax
	REAL*8 curvetaxin(15), curvereduxin(15), gwpadjust, enprdelta, tc1, tc2, taxin
	Real*8 maxredux

	ABATECURVE_CTRL = 0.0d0  !initialize
	curvetax = curvetaxin   ! avoid overwriting acutal curve data when doing tech change etc.
	curveredux = curvereduxin
    maxredux = 0
	if (numpoints .gt. 0 ) THEN
		maxredux = max(curvereduxin(numpoints-1), curvereduxin(numpoints-2))
	end if

	tax = taxin * gwpadjust  ! adjustment for alternate gwp's
							 ! assumes cost curves always use 100 yr gwp

	IF (numpoints.EQ.0) RETURN  !return 0 if no curve passed
	IF (period.LE.availpd) RETURN  !return 0 before curve is available
	IF (phasein.EQ.0.AND.tax.LE.0) RETURN  !return if no tax and not allowing free emissions

	! this line shifts the curve down by the dollar amount passed (energy price delta)
	curvetax = curvetax - enprdelta !shifts whole curvetax array at once

	! this part shifts the curve in 2 technological change dimensions - see below mj 11/02

	! tc1 is the  reduction in cost of existing options per year, 
	! makes reductions "cheaper" over time -- a value of 0.1 means that the amount
	! of abatement available at $100/ton in 1990 will be available at $99.9/ton in 1991
	
	curvetax = curvetax - 15*(period-2)*tc1

	! alternate code for tc1, rotates curve around the 0 point proportionally
	curvetax = curvetax * (1-tc1)**(15*(period-2))

	! tc2 is the reduction in un-mitigatable emissions per year. This tends to affect
	! higher tax rates more than low ones -- so if at the maximum tax level, 90% can be
	! abated, and tc2 = 0.10, then in 1991 1% is added to EVERY level of mitigation.
	! Note that it is not proportional(could of course code it to be proportional but this
	! seemed better for now so low tax levels don't get moved too much)

    ! org version
	! curveredux = 1 - (1-tc2)**(15*(period-2)) * (1-curveredux)

    ! new version, sjs 11/03. Make relative to 2005 and come in slower.
    ! The choice of 2.2 as the exponent makes a tc2 of 0.01 reduce the unmitagatable emissions by about 1/2 by 2095
    ! Revised 4/09 so that tech change is applied propotionately along cost curve
    tempPeriod = 1d0*period
    if (period .lt. 3) tempPeriod = 3d0 
	if ( maxredux .gt. 0 ) THEN
 	   curveredux = 1 - (1-tc2 * ( curveredux / maxredux ))**&
                   ((tempPeriod**2.2-3**2.2)) * (1-curveredux)
	END IF
	! the next part moves the curve up or down based on the "available" and "full"
	! period.  In the available period the curve is moved up so that the 0 redux
	! point coincides with the 0 tax point.  It moves linearly until the full period
	! when the curve is used as read in (and whatever reduction is associated with
	! a 0 tax will be fully realized)  Set the avail and full periods the same to use the 
	! curve as read in without this scaling effect.

	! if the minimum tax point is a positive reduction, then the reduction at the minimum 
	! point is scaled by the same factor (make sure base year always has 0 redux)  mj

	! first find the true 0 reduction point (in case first few tax levels all have 0 reduction)
	DO ipoint = numpoints, 2, -1  !start at the highest
	   IF (curveredux(ipoint) .EQ. 0.0) EXIT
	END DO
	zpoint = ipoint
    
	IF (period.LT.fullpd) THEN  ! only move the curve up if not yet at fullperiod
		shiftper = 1.0 - 1.0*(period-availpd) / (fullpd-availpd)
		curvetax(:) = curvetax(:) + (-curvetax(zpoint) * shiftper)
		curveredux(1) = (1.0 - shiftper) * curveredux(1) !fix for case where lowest reduction (index 1) is not 0
	END IF


	! code to (linearly) interpolate the curve

    DO ipoint = numpoints, 1, -1 !start at the highest tax

      IF (tax.GE.curvetax(ipoint)) THEN !interpolate

	    IF (ipoint.EQ.numpoints) THEN !tax is higher than maximum point on curve
	      ABATECURVE_CTRL = curveredux(numpoints) !use maximum reduction
        ELSE  !interpolate
	      ABATECURVE_CTRL = &
			((tax-curvetax(ipoint))/(curvetax(ipoint+1)-curvetax(ipoint))) * &
			(curveredux(ipoint+1)-curveredux(ipoint)) + &
			curveredux(ipoint)
        END IF
	    EXIT
      END IF
    END DO !end loop over levels

	RETURN
	END	


