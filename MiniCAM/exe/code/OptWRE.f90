 
 
	SUBROUTINE OPTWRE

!*********************************************************************
!  This subroutine finds a "WRE-like" trajectory that satisfies a 
!  specified emissions target.  
!  WARNING: Global trading is assumed (MRKDEF must be set appropriately)
!*********************************************************************

	USE OPTICOM

	USE COMMON, ONLY: MAGICCCResults, NTAXMODE, CEMTARGS, TAXRLM, EMISSTCE

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
	
	INTEGER MAXITER, MM
	REAL*8 DIFF, TOLDIFF, TARGVAL, LASTTARGVAL, InterpFact, LASTWREPATH(NMP), InterpWREPATH(NMP)

	MAXITER = 100  ! max number of iterations to target
	TOLDIFF = 0.001  ! 0.01 = 1 percent difference from target
	
	! in the "opt" framework, this runs with only one group (assume global trading)
	! check to make sure set accordingly:
	IF (NLG.NE.1 .OR. WHICHTARG(1).LE.1) THEN
		MsgStr = "ERROR: WRE paths for global trading and targets only"
		Call MCLog(1,MsgStr,0,0,0,0)
	END IF
	LG = 1 ! set opt routines to group number 1

	
	!set the first path to try to the base one read in:
	WREPATH = BASEWREPATH

	
	!loop to call the model and update the guess until convergence
	ITER = 1
	DIFF = 1 ! initialize

	DO WHILE (ITER.LE.MAXITER .AND. ABS(DIFF).GT.TOLDIFF)

		NTAXMODE = 4 ! set to 4 (emiss target) for all regions
		DO MM = 2, NMP
			CEMTARGS(1:NLP,MM) = WREPATH(MM)/NLP ! assign 1/nlp emiss to each region (assume global trading)
		END DO
		CALL MCAMMAIN(1) ! call the model
		! Hard code target year to be 2095, unless 2100 data is available, then use 2100
 	    MagInt =1+15/INT(MAGICCCResults(0,2)-MAGICCCResults(0,1))*(9 - 2)
 	    if (INT(MAGICCCResults(0,2)-MAGICCCResults(0,1)) .eq. 5) MagInt = MagInt + 1

		IF (WHICHTARG(LG) .EQ. 2) TARGVAL = MAGICCCResults(2,MagInt)
		IF (WHICHTARG(LG) .EQ. 3) TARGVAL = MAGICCCResults(1,MagInt)
		IF (WHICHTARG(LG) .EQ. 4) TARGVAL = MAXVAL(MAGICCCResults(2,1:MagInt))

		DIFF = (TARGVAL - CUMTARG(LG)) / CUMTARG(LG)
		WRITE(*,*) "val, targ, diff",TARGVAL, CUMTARG(LG), DIFF
		IF (ITER.EQ.1) THEN 
		    InterpWREPATH = SECONDWREPATH
		ELSE
			InterpFact = (CUMTARG(LG) - TARGVAL) / (LASTTARGVAL - TARGVAL)
			InterpWREPATH = WREPATH + InterpFact * (LASTWREPATH - WREPATH)
		END IF

		!now move the current guess into the "last" position, and guess the new
		!wre path for the upcoming iteration:
		LASTWREPATH = WREPATH
		LASTTARGVAL = TARGVAL
		WREPATH = InterpWREPATH ! guess the interpolated path

		ITER = ITER + 1
	END DO

	!the current value for CEMTARGS and model evaluation are the solution
	!to transfer back to the OPT routines, include the final taxes and
	!emissions in case needed for cost computations

	TAXSTEPS(ISTEPS,:,:) = TAXRLM
	EMISSSTEPS(ISTEPS,:,:) = EMISSTCE

	RETURN
	END SUBROUTINE