!***********************************************************************
!

	SUBROUTINE OPTPASSRUNID(IIDUMMY, CDUMMY)
    
      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
	CHARACTER*72 CDUMMY
!	passes the RunID to the optimization/cost stuff
	OPTRUNID = IIDUMMY
	OPTDBNAME = CDUMMY

	RETURN
	END

	SUBROUTINE MITICOST1
!	calls the minicam once to get the control level etc.
!	this runs for wre cases, specified carbon targets, etc.
    
      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   

	CALL MCAMCOSTLINK(0,MCTAX,MCEMISS)
	TAXSTEPS(ISTEPS,:,:) = MCTAX
	EMISSSTEPS(ISTEPS,:,:) = MCEMISS
	
	CALL MCAMMAIN(2) !call all model output (do only cost output at end)

	RETURN
	END

      SUBROUTINE MITICOST

!	sub to derive a marginal cost curve and compute costs as the area under it

      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   

	  REAL*8 AREALM(NLPMax,NMP), EMISSREDUX(ISTEPS,NLPMax,NMP),  &
             intrpcost(NLPMax,1990:(1960+15*NMP))
      INTEGER CT,SCT,VARIDOPT, LL
      CHARACTER*2 INT_CHAR(0:10)
	
	INT_CHAR=(/' 0',' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9', &
      '10'/)

	MCTAX = 0
	CALL MCAMCOSTLINK(1,MCTAX,MCEMISS)
	EMISSSTEPS(0,:,:) = MCEMISS
	TAXSTEPS(0,:,:) = MCTAX

	DO ISTEP = 1, ISTEPS-1
	  MCTAX = ISTEP * (TAXSTEPS(ISTEPS,:,:) / ISTEPS)
	  CALL MCAMCOSTLINK(1,MCTAX,MCEMISS)
	  EMISSSTEPS(ISTEP,:,:) = MCEMISS
	  TAXSTEPS(ISTEP,:,:) = MCTAX
	END DO

!	now write the cost outputs
  100 FORMAT(1I10,1H,,1I4,1H,,1I6,8(1H,,1F20.5))
  200 FORMAT(1I6,1H,,5(A,1H,))

	DO ISTEP = 1, ISTEPS
	  EMISSREDUX(ISTEP,:,:) = EMISSSTEPS(0,:,:) - EMISSSTEPS(ISTEP,:,:)
	END DO

	AREALM(:,:) = 0.5 * TAXSTEPS(1,:,:) * EMISSREDUX(1,:,:)

	DO ISEG = 2, ISTEPS
	  AREALM(:,:) = AREALM(:,:) + 0.5 *  &
        (EMISSREDUX(ISEG,:,:) - EMISSREDUX(ISEG-1,:,:)) * &
        (TAXSTEPS(ISEG,:,:) + TAXSTEPS(ISEG-1,:,:))
      END DO

!	write the snapshot, nondiscounted, costs
      CT = 50
	SCT = 10
	VARIDOPT = CT*10000+SCT*100+1
	WRITE(108,200) VARIDOPT,'OptCost','Snapshot','Regional','Mil90US$'
	DO LL = 1, NLP
	  WRITE(110,100) OPTRUNID,LL,VARIDOPT,AREALM(LL,2:NMP)
	END DO
	WRITE(110,100) OPTRUNID,0,VARIDOPT,SUM(AREALM(:,2:NMP),DIM=1) !GLOBAL


!	interpolate the costs annually...
	do iyear = 1990, (1960 + 15*nmp) - 1
	  mlast = INT((iyear-1960)/15)
	  iyearlast = 1960 + 15*mlast
	  mnext = mlast + 1
	  intrpcost(:,iyear) = AREALM(:,mlast) + ((iyear-iyearlast)/15.0d0) &
                     * (AREALM(:,mnext)-AREALM(:,mlast))
	end do
	intrpcost(:,(1960 + 15*nmp)) = AREALM(:,NMP)  !last year

!	now discount them:
	discrate = 0.05    !DISCOUNT RATE
	idbaseyear = 2000  !BASE YEAR
	do iyear = idbaseyear, (1960 + 15*nmp)
	  intrpcost(:,iyear) = intrpcost(:,iyear) /  &
                            ((1 + discrate) ** (iyear-idbaseyear))
	end do

!	write out the dicounted totals
	VARIDOPT = CT*10000+SCT*100+2
	WRITE(108,200) VARIDOPT,'OptCost','DiscSum','Regional','Mil90US$'
	DO LL = 1, NLP
	  	  WRITE(110,100) OPTRUNID,LL,VARIDOPT,SUM(intrpcost(LL, &
       2000:SUMYEAR))
	END DO
	WRITE(110,100) OPTRUNID,0,VARIDOPT,SUM(intrpcost(:,:)) !global

!	write out the actual cost curve (global only)
!	Tax steps used for cost calculation
	SCT = 11
	VARIDOPT = CT*10000+SCT*100+0
	DO ISTEP = 1, ISTEPS
 	  WRITE(108,200) VARIDOPT+ISTEP,'OptCost','GlobalCurveT','Tax' &
      //INT_CHAR(ISTEP),'90US$/tC'
	  WRITE(110,100) OPTRUNID,0,VARIDOPT+ISTEP,TAXSTEPS(ISTEP,1,2:NMP)
	END DO

!	Emission steps used for cost calculation
	SCT = 12
	VARIDOPT = CT*10000+SCT*100+0
	DO ISTEP = 1, ISTEPS
	 WRITE(108,200) VARIDOPT+ISTEP,'OptCost','GlobalCurveE','EmissRed' &
      //INT_CHAR(ISTEP),'MMT'
	  WRITE(110,100) OPTRUNID,0,VARIDOPT+ISTEP, &
        SUM(EMISSSTEPS(0,:,2:NMP),DIM=1) -  &
        SUM(EMISSSTEPS(ISTEP,:,2:NMP),DIM=1)
	END DO


	RETURN
      END
      
