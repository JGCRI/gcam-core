!***********************************************************************
!
      SUBROUTINE   TRANSPORT
!***********************************************************************
!
!                  -- THE ENERGY DEMAND MODULE --
!
! THIS SUBROUTINE COMPUTES THE DEMAND FOR TRANSPORTATION SEVICES
! BY 
!
!
!   SUBROUTINES CALLED: NONE
!
!   WRITTEN BY:
!     Sonny Kim                    
!     1 May 2000                 
!                                       
!***********************************************************************
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!  LOCAL VARIABLES
!
      REAL*8  T,TT,X,TEMPCOST, &
              SCALER(NTSERV,NTMODE+1,NLPMax),LND, &
              SCALERUL(NLPMax),TMPCOSTUL
      REAL*8  SHRIS(NTMODE+1),SHRISS(NTSERV,NTMODE+1,NFUEL+1), &
              SHRISUL(NTMODE+1)
      INTEGER NK,NT,ITS

!  +-----------------------------------------+
!  !   INTERPOLATE INCOME ELASTICITY VALUES  !
!  +-----------------------------------------+
!
      NT=(M-1)*NJUMP
      T=NT
      TT=NJUMP
!
      NK=NKL(L)
!      X=YLM(L,M)/(ZLM(L,M)/ZLM(L,1))
      X=YLM(L,M)
      IF (M.EQ.1) THEN
         LND=(ZLM(L,M)/QL(L,M))/(ZLM(L,1)/QL(L,1))
	ELSE
         LND=(ZLM(L,M)/QL(L,M))/(ZLM(L,M-1)/QL(L,M-1))
	END IF


!-----TRANSPORTATION SECTOR--------
!     Transportation Passenger and Freight Service by Mode and by Fuel
!     Use last indices for totals
!     Initialize totals for both service(its=3)
      ITS=3
      TRANFUEL(ITS,NTMODE+1,NFUEL+1,L,M)=0.0
      TRANCOSTUL(L,M)=0.0
      DO I=1,NTMODE
         TRANFUEL(ITS,I,NFUEL+1,L,M)=0.0
      END DO	   
      DO J=1,NFUEL
         TRANFUEL(ITS,NTMODE+1,J,L,M)=0.0
      END DO	   
!	Total service fuel consumption by mode and by fuel
      DO I=1,NTMODE
	    DO J=1,NFUEL
			TRANFUEL(ITS,I,J,L,M)=0.0
		END DO	   
      END DO	   

	DO 100 ITS = 1,NTSERV !PASSENGER AND FREIGHT SERVICE
      TRANCOST(ITS,NTMODE+1,NFUEL+1,L,M)=0.0
	SHRIS(NTMODE+1)=0.0
      DO I=1,NTMODE
         TRANCOST(ITS,I,NFUEL+1,L,M)=0.0
         SHRISS(ITS,I,NFUEL+1)=0.0
!        BY FUEL: CALCULATE SERVICE COST(IN $/MI) AND SHARE
!        TXJKLM(K=3) ADDS CARBON TAXES TO TRANSPORTATION
	   DO J=1,NFUEL
	      TRANFLCT(ITS,I,J,L,M)=TRANEFF(ITS,I,J,L,M) &
                *(PJLM(J,L,M)+TXJKLM(J,3,L,M))*BtuToJ/(1.0E9)*CVRT90
	      TRANCOST(ITS,I,J,L,M)=TRANFLCT(ITS,I,J,L,M) &
                                 +TRANNFCT(ITS,I,J,L,M)
            IF (M.EQ.1 .OR. M.EQ.2) THEN
!              calibrate base year shares
               IF (TRANSERV(ITS,I,NFUEL+1,L,1).EQ.0.0) THEN
                  SHRISS(ITS,I,J)=0.0
               ELSE
                  SHRISS(ITS,I,J)=TRANSERV(ITS,I,J,L,1) &
                                 /TRANSERV(ITS,I,NFUEL+1,L,1)
               END IF
            ELSE
               SHRISS(ITS,I,J)=SHRWTS(ITS,I,J,L,M) &
                              *TRANCOST(ITS,I,J,L,M)**TPELAS(ITS,I,L,M)
            END IF
            SHRISS(ITS,I,NFUEL+1)=SHRISS(ITS,I,NFUEL+1)+SHRISS(ITS,I,J)
         END DO
!        BY MODE: CALCULATE AVG SERVICE COST(IN $/MI) AND SHARE
         IF (SHRISS(ITS,I,NFUEL+1).EQ.0.0) THEN
            SHRIS(I) = 0.0
         ELSE
	      DO J=1,NFUEL
               SHRISS(ITS,I,J)=SHRISS(ITS,I,J)/SHRISS(ITS,I,NFUEL+1)
!              Weighted average cost of each mode for all fuels
               IF (SHRISS(ITS,I,J).NE.0.0) &
               TRANCOST(ITS,I,NFUEL+1,L,M)=TRANCOST(ITS,I,NFUEL+1,L,M) &
                           +SHRISS(ITS,I,J)*TRANCOST(ITS,I,J,L,M) &
                           **TPELAS(ITS,I,L,M)
            END DO
	      TRANCOST(ITS,I,NFUEL+1,L,M)=TRANCOST(ITS,I,NFUEL+1,L,M)
!     &                                **(1.0/TPELAS(ITS,I,L,M))
!           Covert cost into $/pass-mi or $/ton-mi
!           Add time value to average cost of pass service for each mode
!           Annual per capita income divided by average speed for each mode
!           in units of $/mi
!           Freight does not add time value for now
            IF (ITS.EQ.1) THEN
               TRANCOST(ITS,I,NFUEL+1,L,M)=TRANCOST(ITS,I,NFUEL+1,L,M) &
                           /TLOADFAC(ITS,I,L,M) &
                           +GNPBL(L)*YLM(L,M)/ZLM(L,M)/TSPEED(ITS,I,L,M) &
                           *1.0E3/(365.0*24.0)
            ELSE
               TRANCOST(ITS,I,NFUEL+1,L,M)=TRANCOST(ITS,I,NFUEL+1,L,M) &
                           /TLOADFAC(ITS,I,L,M)
            END IF
!           Normalize $/pass-mi or $/ton-mi cost to previous year
            IF (M.EQ.1) THEN
                TEMPCOST=TRANCOST(ITS,I,NFUEL+1,L,M) &
                    /TRANCOST(ITS,I,NFUEL+1,L,1)
		  ELSE
                TEMPCOST=TRANCOST(ITS,I,NFUEL+1,L,M) &
                    /TRANCOST(ITS,I,NFUEL+1,L,M-1)
		  END IF

!           Determine scaler from base data to calibrate
            IF (M.EQ.1 .OR. M.EQ.2) SCALER(ITS,I,L)= &
                TRANSERV(ITS,I,NFUEL+1,L,1) &
                *TEMPCOST**(-TPELAS(ITS,I,L,M))                                      &
                *X**(-TYELAS(ITS,I,L,M))*LND**(-TLELAS(ITS,I,L,M))
            SHRIS(I)=SCALER(ITS,I,L) &
                *TEMPCOST**TPELAS(ITS,I,L,M) &
                *X**TYELAS(ITS,I,L,M) &
                *LND**TLELAS(ITS,I,L,M)
         END IF
         SHRIS(NTMODE+1)=SHRIS(NTMODE+1)+SHRIS(I)
      END DO
!     RECALCULATE SHARES BY MODE AND CALCULATE TOTAL TRANSPORTATION COST
      DO I=1,NTMODE         
         IF (SHRIS(NTMODE+1).NE.0.0) &
             SHRIS(I)=SHRIS(I)/SHRIS(NTMODE+1)
!        Weighted average cost of total licensed transportation service
         IF (SHRIS(I).NE.0.0) &
             TRANCOST(ITS,NTMODE+1,NFUEL+1,L,M)= &
                  TRANCOST(ITS,NTMODE+1,NFUEL+1,L,M) &
                  +(SHRIS(I)*TRANCOST(ITS,I,NFUEL+1,L,M))
!     &            **TPELAS(ITS,NTMODE+1,L,M)
      END DO
!     Weighted average cost of total unlicencsed passenger transportation service
      IF (ITS.EQ.1) THEN
         DO I=2,NTMODE         
             IF (SHRIS(I).NE.0.0) THEN
                SHRISUL(I)=SHRIS(I)/(1.0-SHRIS(1))
                TRANCOSTUL(L,M)=TRANCOSTUL(L,M) &
                +(SHRISUL(I)*TRANCOST(ITS,I,NFUEL+1,L,M))
!     &          **TPELAS(ITS,NTMODE+1,L,M)
             END IF
         END DO
      END IF

!     Trying to implement logit cost instead of straight weighted avg
!     two lines commented out above as well.

!      TRANCOST(ITS,NTMODE+1,NFUEL+1,L,M)=
!     &       TRANCOST(ITS,NTMODE+1,NFUEL+1,L,M)
!     &       **(1.0/TPELAS(ITS,NTMODE+1,L,M))
!      TRANCOSTUL(L,M)=TRANCOSTUL(L,M)
!     &       **(1.0/TPELAS(ITS,NTMODE+1,L,M))
      
!     Normalize avg cost of total service to previous year
	IF (M.EQ.1) THEN
         TEMPCOST=TRANCOST(ITS,NTMODE+1,NFUEL+1,L,M) &
              /TRANCOST(ITS,NTMODE+1,NFUEL+1,L,1)
         TMPCOSTUL=TRANCOSTUL(L,M) &
               /TRANCOSTUL(L,1)
	ELSE
         TEMPCOST=TRANCOST(ITS,NTMODE+1,NFUEL+1,L,M) &
              /TRANCOST(ITS,NTMODE+1,NFUEL+1,L,M-1)
         TMPCOSTUL=TRANCOSTUL(L,M) &
               /TRANCOSTUL(L,M-1)
	END IF

!     Used average cost of transportation service for all modes to determine
!     total demand for transportation service
      IF (M.EQ.1 .OR. M.EQ.2) THEN
	    IF (ITS.EQ.1) THEN
	       SCALER(ITS,NTMODE+1,L) = &
               TRANSERV(ITS,NTMODE+1,NFUEL+1,L,1)*TLICENSE(L,M) &
               *TEMPCOST**(-TPELAS(ITS,NTMODE+1,L,M)) &
               *X**(-TYELAS(ITS,NTMODE+1,L,M))
	       SCALERUL(L) = TRANSERV(ITS,NTMODE+1,NFUEL+1,L,1) &
               *(1.0-TLICENSE(L,M)) &
               *TMPCOSTUL**(-TPELAS(ITS,NTMODE+1,L,M)) &
               *X**(-TYELAS(ITS,NTMODE+1,L,M))
          ELSE
	       SCALER(ITS,NTMODE+1,L) = &
               TRANSERV(ITS,NTMODE+1,NFUEL+1,L,1) &
               *TEMPCOST**(-TPELAS(ITS,NTMODE+1,L,M)) &
               *X**(-TYELAS(ITS,NTMODE+1,L,M))
	    END IF
      END IF  

      IF (ITS.EQ.1) THEN
         TRANSERV(ITS,NTMODE+1,NFUEL+1,L,M) = SCALER(ITS,NTMODE+1,L) &
            *TEMPCOST**TPELAS(ITS,NTMODE+1,L,M) &
            *X**TYELAS(ITS,NTMODE+1,L,M) &
            +SCALERUL(L)*TMPCOSTUL**TPELAS(ITS,NTMODE+1,L,M) &
            *X**TYELAS(ITS,NTMODE+1,L,M)
	ELSE              
         TRANSERV(ITS,NTMODE+1,NFUEL+1,L,M) = SCALER(ITS,NTMODE+1,L) &
            *TEMPCOST**TPELAS(ITS,NTMODE+1,L,M) &
            *X**TYELAS(ITS,NTMODE+1,L,M)
      END IF
!     Share out transportation service by mode and by fuel
!     Calculate demand for fuel using energy intensity
!     (1.0E9) converts to Quads
      TRANFUEL(ITS,NTMODE+1,NFUEL+1,L,M)=0.0
      DO J=1,NFUEL
         TRANFUEL(ITS,NTMODE+1,J,L,M)=0.0
         TRANSERV(ITS,NTMODE+1,J,L,M)=0.0
      END DO
      DO I=1,NTMODE         
         TRANFUEL(ITS,I,NFUEL+1,L,M)=0.0
         TRANSERV(ITS,I,NFUEL+1,L,M)=SHRIS(I) &
                        *TRANSERV(ITS,NTMODE+1,NFUEL+1,L,M)
         DO J=1,NFUEL
            TRANSERV(ITS,I,J,L,M)=SHRISS(ITS,I,J) &
                        *TRANSERV(ITS,I,NFUEL+1,L,M)
            TRANFUEL(ITS,I,J,L,M)=TRANSERV(ITS,I,J,L,M) &
               /TLOADFAC(ITS,I,L,M)*TRANEFF(ITS,I,J,L,M)/(1.0E9)
!           Total fuel demand by mode and by fuel
            TRANFUEL(ITS,I,NFUEL+1,L,M)=TRANFUEL(ITS,I,NFUEL+1,L,M) &
                                       +TRANFUEL(ITS,I,J,L,M)
            TRANFUEL(ITS,NTMODE+1,J,L,M)=TRANFUEL(ITS,NTMODE+1,J,L,M) &
                                       +TRANFUEL(ITS,I,J,L,M)
            TRANSERV(ITS,NTMODE+1,J,L,M)=TRANSERV(ITS,NTMODE+1,J,L,M) &
                                       +TRANSERV(ITS,I,J,L,M)
            TRANFUEL(3,I,J,L,M)=TRANFUEL(3,I,J,L,M) &
                                       +TRANFUEL(ITS,I,J,L,M)
         END DO
!        Total fuel demand for each and all serivce by mode
         TRANFUEL(ITS,NTMODE+1,NFUEL+1,L,M)=       &
            TRANFUEL(ITS,NTMODE+1,NFUEL+1,L,M) &
            +TRANFUEL(ITS,I,NFUEL+1,L,M)
         TRANFUEL(3,I,NFUEL+1,L,M)= &
            TRANFUEL(3,I,NFUEL+1,L,M)       &
            +TRANFUEL(ITS,I,NFUEL+1,L,M)
      END DO
!     Total fuel demand for each and all serivce by fuel
      DO J=1,NFUEL
         TRANFUEL(3,NTMODE+1,J,L,M)= &
            TRANFUEL(3,NTMODE+1,J,L,M)       &
            +TRANFUEL(ITS,NTMODE+1,J,L,M)
      END DO
      TRANFUEL(3,NTMODE+1,NFUEL+1,L,M)= &
              TRANFUEL(3,NTMODE+1,NFUEL+1,L,M)       &
              +TRANFUEL(ITS,NTMODE+1,NFUEL+1,L,M)
  100 CONTINUE !service
!-----TRANSPORTATION SECTOR END-------

      RETURN
      END
