!***********************************************************************
      SUBROUTINE ANTEPER
!***********************************************************************
!
!     Called before eah period solution procedure starts.
!     Updates variables that need to be set at beginning of period
!     but do not change within the iteration procedure.
!     Marshall Wise  4/28/97
!
!***********************************************************************
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8   C,T,TT,R,Q,V,W
!
      INTEGER  INUC,ISNUC,ISOLAR,MM1,MM2
!     see if using demographic routine to generate population data  !  hmp
      if (ifPopRate) Then
         IF (M .eq. 2 .and. L .eq. 1) then
! Demog module doesn't currently produce forward-looking population estimates.  sjs - 07/01
! These are now needed for the Ag module.
! Will need to re-write this so that pop estimates (without any feedbacks) are produced
! Once at the begining of the run.
           MsgStr = "Warning: Demog module needs to be re-written. Ag drivers not correct."
           Call MCLog(1,MsgStr,M,L,0,0)
         end if
         Call DemogMC(M,ZLM,WAgeMC,DemoStatMC)   !    hmp  
      end if
!     For coal phase-out policies, check to see if region is in policy
!     then, reduce it's coal share parameters and remove it from the
!     coal market
      DO L=1,NL
	   IF(ICARBOUT(L,1).NE.0 .AND. ICARBOUT(L,1).LT.M) THEN
!     If first period in policy, make independent coal market
         IF(ICARBOUT(L,1).EQ.(M-1)) THEN
	      IF(IPROTOC.EQ.0) THEN
	         NMRK=NMRK+1
	         MRKDEF(NMRK,1)=NMRK
	         MRKDEF(NMRK,2)=INCOAL
	         MRKDEF(NMRK,3)=1
	         MRKDEF(NMRK,4)=L
	         IPROTOC=1
	      ELSE
	         MRKDEF(NMRK,3) = MRKDEF(NMRK,3)+1
	         MRKDEF(NMRK,3+MRKDEF(NMRK,3)) = L
	      END IF
!     Remove from original coal market (note, only works if global coal
!     starts out as market 3)
	      NR=MRKDEF(INCOAL,3)
	      IMOVE=0
	      DO LLOOP=1,NR
	         L1=MRKDEF(INCOAL,3+LLOOP)
	         IF(L1.EQ.L .AND. IMOVE.EQ.0) THEN
	            IMOVE=1
	            DO L2=LLOOP,NR-1
	               MRKDEF(INCOAL,3+L2) = MRKDEF(INCOAL,3+L2+1)
	            END DO
	         END IF
	      END DO
	      MRKDEF(INCOAL,3) = MRKDEF(INCOAL,3)-1
	   END IF
      
      END IF
      END DO

!     Before each period, compute prices of solar, nuclear, hydro
!     (Code moved from PPPP May 6, 1997  maw)
!
      INUC  =4
      ISNUC =5
      ISOLAR=5
      T     =(M-1)*NJUMP
      TT    =NJUMP
      MM1   =M-1
      MM2   =M-2
      Q=0.D0
! 
!***********************************************************************
      DO 30 L=1,NL
!        +-------------------------------------------------+
!        | SOLAR SUPPLY IS PERFECTLY ELASTIC AT PRICE PILM |
!        +-------------------------------------------------+
!        Solar should have non-fuel cost only and no fuel cost
!        Price of solar is determined by HUILM only.  shk 2/19/99
	   PILM(ISOLAR,L,M)=0.0
!
!                +--------------------------+
!                | SET HYDRO ELECTRIC PRICE |
!                +--------------------------+
!        Hydro should have non-fuel cost only and no fuel cost
!        Price of hydro is determined by HUILM only.  shk 2/19/99
         PILM(NI,L,M)=0.0
!         PILM(NI,L,M)=HYDRO(4,L)
!
!-----------------------------------------------------------------------
!                  LONG-RUN SUPPLY SCHEDULE FOR
!                     IS=5= NUCLEAR
!-----------------------------------------------------------------------
!
!               +------------------------------------------------+
!               |     ESTIMATE CUMULATIVE PRODUCTION TO DATE     |
!               +------------------------------------------------+
         IS=ISNUC
         I=INUC
!            +----------------------------------------------------+
!            |  In the first period cumulative production is zero |
!            +----------------------------------------------------+
         IF(M.EQ.1) QISLM(IS,L,M)=0.D0
!            +------------------------------------------------------+
!            |  After the second period recompute actual cumulative |
!            |  production.                                         |
!            +------------------------------------------------------+
         IF(M.GE.3) QISLM(IS,L,MM1)=QISLM(IS,L,MM2) + &
                       0.5D0*(ESUILM(I,L,MM2)+ESUILM(I,L,MM1))*TT
!            +-------------------------------------------------------+
!            |  For all periods after the first, estimate cumulative |
!            |  production as a simple sum of the previous period's  |
!            |  cumulative production plus the last periods          |
!            |  production rate times TT years.                      |
!            +-------------------------------------------------------+
         IF(M.GE.2) QISLM(IS,L,M)=QISLM(IS,L,MM1) + TT*ESUILM(I,L,MM1)
!            +-----------------------------------------+
!            |  Compute global cumulative production.  |
!            +-----------------------------------------+
         Q=Q+QISLM(IS,L,M)
   30 CONTINUE
!       +-------------------------------------------------------------------+
!       |  USE GLOBAL CUMULATIVE PRODUCTION TO COMPUTE NUCLEAR FUEL PRICE   |
!       +-------------------------------------------------------------------+
!
!                *****************************************
!                ***   FIND RESOURCE GRADE CURRENTLY   ***
!                ***   BEING PRODUCED                  ***
!                *****************************************
!
!     R is total global resource including all grade and regions
!     W is resource of each grade for all regions
!     Q is global cumulative production
            R=0.D0
            DO 50 IG=1,NIG
               W=0.D0
               DO 40 L=1,NL
                  W=W+RIGISL(IG,IS,L)
                  R=R+RIGISL(IG,IS,L)
   40          CONTINUE
               IF(Q.LE.R) GO TO 70
   50       CONTINUE
!
!                ****************************************
!                ***   IF RESOURCE IS EXHAUSTED, SET  ***
!                ***             CIL=100              ***
!                ****************************************
!
         DO 60 L=1,NL
            CIL(IS,L)=100.D0
            PILM(4,L,M)=CIL(IS,L)
   60    CONTINUE
      GO TO 110
!
!                ****************************************
!                ***   CALCULATE EXTRACTION COSTS     ***
!                ***   EXCLUDING TECHNOLOGICAL CHANGE ***
!                ***   AND ENVIROMENTAL COSTS         ***
!                ****************************************
!
   70    CONTINUE
!
         IG1=IG+1
         W=(R-Q)/W
         C=W*CIGIS(IG,IS)+(1.D0-W)*CIGIS(IG1,IS)
!
!                ****************************************
!                ***  CALCULATE TECHNOLOGICAL CHANGE  ***
!                ***    AND ENVIRONMENTAL COSTS       ***
!                ****************************************
!
      DO 80 L=1,NL
	   TECH=1.D0
	   DO MTECH=2,M
            TECH=TECH*(1.D0+STISM(IS,MTECH))**NJUMP
	   END DO
!
         V=VISLM(IS,L,M)+TXISLM(IS,L,M)
!
!        ****************************************
!        ***   COMBINE EXTRACTION COSTS,      ***
!        ***   ENVIROMENTAL COSTS, AND TECH-  ***
!        ***   NOLOGICAL CHANGE TO CALCULATE  ***
!        ***   NUCLEAR COST                   ***
!        ****************************************
!
         CIL(IS,L)=(C+V)/TECH
         PILM(4,L,M)=CIL(IS,L)
   80 CONTINUE
  110 CONTINUE

      	   
	RETURN
	END
