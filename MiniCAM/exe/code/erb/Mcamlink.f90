!***********************************************************************

      SUBROUTINE MCAMLINK(INDOPT,TAXARG,GNPARG,CARBARG,ILandEm)
      
!***********************************************************************
!
!     Translates the inputs to MCAM arrays, calls the MCAM, translates
!     te results back to OPTMCAM arrays
!
!     Created by Marshall Wise,  9-27-95
!     Updated for regional targets   8-13-96
!
!***********************************************************************

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      REAL*8 TAXARG(NLPMax,NMP),GNPARG(NLPMax,NMP),CARBARG(NLPMax,NMP)
      
!     If run in optimizing mode,  else leave taxes as read in

      IF(INDOPT.GT.0) THEN
!     Translate tax inputs to ERB inputs.

      DO M=3,NM         
         DO L=1,NL
            TAXRLM(L,M) = TAXARG(L,M-2)
         END DO
      END DO
      
      END IF
      
      
!     Set mode =1 which means run the model w/o reading input files
!        or creating output files.

      MODE=1
      CALL MCAMMAIN(MODE)
      
!     Translate results arrays back to OPTMCAM arrays

      DO L=1,NL
         DO M=2,NM
            GNPARG(L,M-1) = GNPFLM(L,M)
            CARBARG(L,M-1) = EMISSTCE(L,M)
            IF (ILandEM .eq. 1) CARBARG(L,M-1) = CARBARG(L,M-1) + CARBLAND(L,M)
         END DO
      END DO
      
      RETURN
      
      END

!	************************************************************************************
!	************************************************************************************

	SUBROUTINE MCAMCOSTLINK(MODECOST,TAXCLNK,EMISSCLNK)
!	links minicam to cost routines -- this could be combined with the opt link
!	in the future
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	      
	INTEGER MODECOST
      REAL*8 TAXCLNK(NLPMax,NMP), EMISSCLNK(NLPMax,NMP)

	IF (MODECOST.EQ.1) THEN !this is a "step" run as opposed to a base run
	  TAXRLM = TAXCLNK !set passed taxes
	  NTAXMODE = 1  !set tax mode to 1
	  !do we have to worry about the markets?  I think IPFIX fixes them anyway?
	END IF

      CALL MCAMMAIN(1)

!	now get the stuff we need back

	TAXCLNK = TAXRLM
	EMISSCLNK = EMISSTCE

      RETURN
      
      END     
