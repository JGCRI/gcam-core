!***********************************************************************
!
!     A couple of MCAM/ERB input and initialization routines, taken out
!     of the main program by Marshall Wise, Oct 1995.
!
!***********************************************************************

!***********************************************************************
      SUBROUTINE DATAIN
      
!***********************************************************************     
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      IUNIT=1
      IOUNIT=0
      JUNIT=6
      MALL=0
!                  +-------------------------------------+
!                  |     CALL DATA INPUT SUBROUTINES     |
!                  +-------------------------------------+
      MDUMP=1
!	Default number of electric technologies (in case not read-in)
      NNU = 13
      NStype = 0
      
!     call initialization routine
      CALL NSET
!     Call File control file
      
      CALL ERBCTRL
      CALL NSET	! Call NSET again to set number of region variables once NLP is read-in
      
      
	  Write(97,*) "****ERB Inputs"		! Write to log file. sjs
      DO INF=1,INFILES
        Write(*,*)  'Opening: ',FILES(INF)
        Write(97,*) 'Opening: ',FILES(INF)
         OPEN(IUNIT,FILE=FILES(INF))
          CALL ERBINPUT(INF)
         CLOSE (IUNIT)
      END DO

!*************
! Create GUILM Values from generation & Distribution efficiencies, if read-in
! Do by technology, since may not read in generation efficiencies for all technologies
! If a generation efficiency was not read in, estimate, so that a value will be 
! available if needed

      DO I=1,NNU
        IF (GenEff(I,1,2) .ne. 0.0) THEN 
          GUILM(I,:,:) = 1.0/(GenEff(I,:,:)*(1-DistLoss(:,:)))
        ELSE	! If wasn't read-in,estimate a value
          GenEff(I,:,:) = (1.0/GUILM(I,:,:))/(1-DistLoss(:,:))
        END IF
      END DO
     
!**********************************************************************
!    APPLY 2050 VALUES TO INPUT VARIABLES FOR LAST THREE PERIODS
!
      DO 1100 M=7,NM
          DO 1110 L=1,NL
               DO 1120 I=1,NNU
                    IF(BSUILM(I,L,M).LE.0.0) BSUILM(I,L,M)=BSUILM(I,L,6)
!     Put in check to see if GUILM and HUILM assigned for later period 
!        in interface first. iF SO, DON'T OVERWRITE.
                    IF (GUILM(I,L,M).LE.0.0) GUILM(I,L,M)=GUILM(I,L,6)
                    IF (HUILM(I,L,M).LE.0.0) HUILM(I,L,M)=HUILM(I,L,6)
 1120 CONTINUE
               DO 1130 I=1,5
                    TXISLM(I,L,M)=TXISLM(I,L,6)
 1130 CONTINUE
               DO 1160 K=1,NKMAX
                    DO 1170 J=1,NJ		! Removed BSJKLM from here so that later shares can be adjusted. sjs. 05/01
                         IF(TXJKLM(J,K,L,M).LE.0.0) THEN
                              TXJKLM(J,K,L,M)=TXJKLM(J,K,L,6)
	                   END IF
 1170 CONTINUE
 1160 CONTINUE
	          DO I=1,NF+1
                    TXILM(I,L,M)=TXILM(I,L,6)
	         END DO

 1110 CONTINUE
 1100 CONTINUE
         
! . . Propagate GJKs and HJKs to all regions and time periods for
! . . builidings and industry.
      DO K=1,2
         DO J=1,NNJ-1      !This assumes H2 already read in.
            DO M=1,NM
               DO L=1,NL
                  GJKLM(J,K,L,M) = GJK(J,K)      
                  HJKLM(J,K,L,M) = HJK(J,K)
               END DO
            END DO
         END DO
      END DO
!

!     Save old tklms for compounding
      DO 1439 M=1,NM
         DO 1440 L=1,NL
            DO 1441 K=1,3
               TKLHOLD(K,L,M)=TKLM(K,L,M)
 1441       CONTINUE
 1440    CONTINUE
 1439 CONTINUE 

! Changed - 08/01
! Conversion factors no longer read in
! Make conversion factors consistent with CO2 coefficients
! Take care of unconventional oil as a special case in fossil supply routine

      TXUILM0(1,1:NL,3:NM) = COI(1)/CVRT90/10	! div by ten to get correct units
      TXUILM0(2,1:NL,3:NM) = COI(2)/CVRT90/10	! div by ten to get correct units
      TXUILM0(3,1:NL,3:NM) = COI(3)/CVRT90/10	! div by ten to get correct units
      TXISLM0(1:NF,1:NL,3:NM) = TXUILM0(1:NF,1:NL,3:NM)
      TXUILM0(1:NF,1:NL,1:2) = 0.0
      TXISLM0(1:NF,1:NL,1:2) = 0.0           
      TXJKLM0(1:NF,1:NKMAX,1:NL,1:NM) = TXJKLM(1:NF,1:NKMAX,1:NL,1:NM)

!     Determine total resource by summing resource in each grade
!     shk 5/3/99
      DO L=1,NL
         DO IS=1,5
            RESOURCE(IS,L)=0.0
	      DO IG=1,NIG
               RESOURCE(IS,L)=RESOURCE(IS,L)+RIGISL(IG,IS,L)
            END DO
         END DO
      END DO

      RETURN
      END
!***********************************************************************
!***********************************************************************
!
      FUNCTION  XNTERP(X,Y,Z,T)
!
!***********************************************************************
!
!     THIS FUNCTION INTERPOLATES BETWEEN AN INITIAL VALUE (X) AND A
!     FINAL VALUE (Y) IF THE CURRENT PERIOD (T) IS LESS THAN THE
!     TRANSITION PERIOD (Z).
!
      REAL*8  X,Y,Z,T,TT,XNTERP
!
      IF (T .GE. Z) XNTERP=Y
      IF (T .GE. Z) RETURN
      TT=T/Z
      XNTERP=X**(1.D0-TT)*Y**TT
!
      RETURN
      END


!***********************************************************************
      SUBROUTINE ScaleCoefs
      
!     Routines to scale B coefficients from their calibrated 1990 values to the 
!     2050 values that are read in. 
!     This overwrites 2005 - 2035 values
!     Values from 2050 and onward are unchanged. sjs 12/01
!
!
!***********************************************************************     
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Do this for BSJKLM, BSSJKLM, & BSUILM

DO L = 1,NL
  DO MM = 3,5
    DO K = 1,3
      DO J = 1,4
         BSJKLM(J,K,L,MM) = B_Lin_INTERP(BSJKLM(J,K,L,2),BSJKLM(J,K,L,6),MM)
      END DO !J Loop

      BSSJKLM(K,1,L,MM) = B_Lin_INTERP(BSSJKLM(K,1,L,2),BSSJKLM(K,1,L,6),MM)
      BSSJKLM(K,2,L,MM) = B_Lin_INTERP(BSSJKLM(K,2,L,2),BSSJKLM(K,2,L,6),MM)
    END DO !K Loop

    DO I = 1,9
      BSUILM(I,L,MM) = B_Lin_INTERP(BSUILM(I,L,2),BSUILM(I,L,6),MM)
    END DO

    BSSUILM(3,3,L,MM) = B_Lin_INTERP(BSSUILM(3,3,L,2),BSSUILM(3,3,L,6),MM)

  END DO !MM loop
END DO !L loop

RETURN
END


Real*8 Function B_Lin_INTERP(B1,B2,M)	! Linearly Interpret between 1990 and 2050. sjs 12/01
REAL*8  B1,B2
INTEGER M

!write(*,'(8f9.4)') B1, B2,((M-2)*1.0)*(B2-B1)/4.,B1 + ((M-2)*1.0)*(B2-B1)/4.
B_Lin_INTERP = B1 + ((M-2)*1.0)*(B2-B1)/4.

RETURN
END

Real*8 Function B_NonLin_INTERP(B1,B2,M)	! Non-linearly Interpret between 1990 and 2050. sjs 12/01
REAL*8  B1,B2
INTEGER M

IF (B1 .ne. 0 .and. B2 .ne. 0) Then
  B_NonLin_INTERP = B1/((1+((B1/B2)**(1/(15*4))-1))**15)**(M-2)
ELSE
  B_NonLin_INTERP =B_Lin_INTERP(B1,B2,M)
END IF
!=C23/(1+((C23/G23)^(1/(15*4))-1))^15

RETURN
END

