
!**********************************************************************

      SUBROUTINE SOLVE
	 
!**********************************************************************
!
!  This subroutine establishes bounds for the solution price such that
!  the sign of excess demand changes from one bound to the other.  It
!  then calls BRENT to establish the price to the desired accuracy.
!
!  Subroutines Called:   BRENT, MODEL
!
!  Local Variables:  ITR, MSOL
!
!  Coded by hmp  9/16/91

!     Adapted for MiniCAM 4/97 maw
!
!
!**********************************************************************
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      PARAMETER (TINY = .0001,WEE = 1.E-12, SMALL = .01)
      REAL*8 PRICE(NMRKP,30),PROD(NMRKP,30),EXD(NMRKP,30), &
      DELP(30)

	CONV=TEST

!  .  Find a pair of prices for which the sign of ED changes.
!  .  DE has an estimated value for demand elasticity and SE has an
!  .  estimated value for supply elasticity.  Initial values for these
!  .  need to be provided.  MULT is a multiplicative adjustment factor
!  .  to improve the likelihood of getting a bracketing value.
!  .  isol = -1  indicates market excess demand function is ill behaved.
!  .  isol = 0   indicates market solution found.
!  .  isol = 1   indicates solution not found within MAXITER iterations.

      INN = MRKDEF(MRK,2)

!  .  The first region in the market definition is the slot which holds
!  .  the market price in the P array.
	 
      L = MRKDEF(MRK,4)
      ITR = 1
      PRICE(MRK,ITR) = P(INN,L,M)
      PROD(MRK,ITR) = MRKPRD(MRK)
      EXD(MRK,ITR) = MRKDEM(MRK) - MRKPRD(MRK)

  100 CONTINUE

      IF (ITR .EQ. 1) THEN

!  .  .  Compute change in price using tanh(exd/prod).  The constant
!  .  .  limits price change to +/- 50%.

         DELP(ITR) = .5 * TANH(EXD(MRK,ITR) / (PROD(MRK,ITR) + TINY))
         PRICE(MRK,ITR+1) = (1 + DELP(ITR)) *  PRICE(MRK,ITR)
      ELSE
         
         DELP(ITR) = .5 * TANH((ITR * EXD(MRK,ITR)) /  &
            (PROD(MRK,ITR) + TINY))
         PRICE(MRK,ITR+1) = (1 + DELP(ITR)) * PRICE(MRK,ITR)
      END IF


      INN = MRKDEF(MRK,2)
      NR = MRKDEF(MRK,3)
      DO LL = 1,NR
         L = MRKDEF(MRK,3+LL)
         P(INN,L,M) = PRICE(MRK,ITR+1)
      END DO


!  .  Now call model to get new excess demand

      CALL MODEL

      ITR = ITR + 1
      PROD(MRK,ITR) = MRKPRD(MRK)
      EXD(MRK,ITR) = MRKDEM(MRK) - MRKPRD(MRK)

!  .  if demand and supply both zero then have a solution.

      IF (PROD(MRK,ITR) .LT. SMALL  &
         .AND. ABS(EXD(MRK,ITR)) .LT. SMALL) THEN
          
         ISOL = 0
         RETURN
      END IF
	    
!  .  If excess demand sufficiently small then have a solution.
      
      IF (ABS(EXD(MRK,ITR)) .LT. CONV) THEN

!  .  .  Current price satisfies convergence requirement

         ISOL = 0
         RETURN
      END IF	    
      
      IF ((EXD(MRK,ITR) * EXD(MRK,ITR-1)) .LT. 0.) THEN


!  .  .  Bracketed prices achieved, set up to call BRENT

         CALL BRNT(PRICE,PROD,EXD,DELP,IBR,ITR)

         ISOL = IBR

         RETURN
      END IF

!  .  Is this branch needed?  (hmp 8/11/92)

      IF (ABS(EXD(MRK,ITR)) .GT. ABS(EXD(MRK,ITR-1))) THEN

!  .  .  pathologic behavior of excess demand indicated, leave isol = -1

         IF (ITR .GT. 4) THEN 
         
!  .  .  .  No bracket achieved on price after 4 tries.

!  .  .  .  Record error exits for debugging purposes.
!
!            WRITE(17,*)'  Error exit from solve--path exd branch'
!            WRITE(17,'(''  PERIOD'',I5,''  MODL'',I5,
!     &         ''  MARKET'',I5)')M,MODL,MRK 
!            WRITE(17,*)'       Price       Prod       Exd'                 

!            DO IR = 1,ITR
!               WRITE(17,'(2X,F12.5,2F12.0)')PRICE(MRK,IR),PROD(MRK,IR),
!     &            EXD(MRK,IR)
!            END DO

            ISOL = 1
            RETURN
         END IF         
	   
         GO TO 100

      END IF
	 
!  .  Bracketed prices not achieved, continue search.


      IF (ITR .GT. 4) THEN 
         
!  .  .  No bracket achieved on price after 4 tries.

!  .  .  Record error exits for debugging purposes.
!
!         WRITE(17,*)'  Error exit from solve--no bracket branch'
!         WRITE(17,'(''  PERIOD'',I5,''  MODL'',I5,
!     &      ''  MARKET'',I5)')M,MODL,MRK 
!         WRITE(17,*)'       Price       Prod       Exd'                 
!         DO IR = 1,ITR
!            WRITE(17,'(2X,F12.5,2F12.0)')PRICE(MRK,IR),PROD(MRK,IR),
!     &         EXD(MRK,IR)
!         END DO
!         WRITE(17,*)EXPPROF (1,2,2,M)                
         ISOL = 1
         RETURN

      END IF
	    
      GO TO 100
      
      END
!**********************************************************************

      SUBROUTINE BRNT(PRICE,PROD,EXD,DELP,IBR,ITR)
	 
!**********************************************************************
!
!  This subroutine computes the equilibrium price once bracketing values
!  for the price have been found.  It is derived from a similar routine
!  in NUMERICAL RECIPES.  
!
!  Subroutines Called:   Model
!
!  Local Variables:  AA,BB,CC,FAA,FBB,FCC,DD,EE,TOL1,EPS,ITMAX,PP,QQ,
!                    RR,XM,SS
!
!  Coded by hmp  9/13/91
!
!
!**********************************************************************

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      REAL*8 cnv,tol,AA,BB,CC,FAA,FBB,FCC,DD,EE,TOL1,EPS,PP,QQ, &
          RR,XM,SS
	REAL*8 PRICE(NMRKP,50),PROD(NMRKP,50),EXD(NMRKP,50), &
      DELP(50)
      PARAMETER (CNV = .000001, SMALL = .01)
      

      CONV=TEST

!  .  IBR is error return code  IBR = -1 shouldn't occur
!  .  IBR = 0 indicates solution found.
!  .  IBR = 1 indicates maximum number of iterations exceeded.

!  .  Initialize values for code.

      IBR = -1
      INN = MRKDEF(MRK,2)	 
      EPS = 3.E-8
      mxiter = 20 + itr
      AA = PRICE(MRK,ITR)
      BB = PRICE(MRK,ITR-1)
      TOL = CNV * (AA + BB) / 2.


      FAA = EXD(MRK,ITR)
      FBB = EXD(MRK,ITR-1)
      
!  .  Now begin algorithm      
      
      FCC = FBB

      DO 100 ITER = 1, MXITER

!  .  .  Rename AA, BB, CC adjust bounding interval DD

         IF (FBB*FCC .GT. 0) THEN
            CC = AA
            FCC = FAA
            DD = BB - AA
            EE = DD
         END IF

!  .  .  Move best estimate to BB

         IF (ABS(FCC) .LT. ABS(FBB)) THEN
            AA = BB
            BB = CC
            CC = AA
            FAA = FBB
            FBB = FCC
            FCC = FAA
         END IF
	   
!  .  .  Set Tolerance and test for convergence
         
   50    TOL1 = 2. * EPS * ABS(BB) + .5 * TOL
         XM = .5 * (CC - BB)

!  .  .  Check for convergence.  Note that algorithm will fail if
!  .  .  the XM is less than TOL1 and iterations continue.  Because
!  .  .  we define solution in terms of FBB rather than BB, we need to
!  .  .  be able to continue.  So the solution is to make TOL smaller.

         IF (ABS(XM) .LE. TOL1 .AND. ABS(FBB) .GT. CONV) THEN
            TOL = TOL / 2.
            IF (TOL .LT. EPS) THEN
!               WRITE(17,*)'  Change in price < EPS  ',EPS 
               GO TO 150
            ELSE
               GO TO 50
            END IF
         END IF       

         IF (FBB .EQ. 0.0 .OR. ABS(EXD(MRK,ITR)) .LT. CONV) THEN   
            PRICE(MRK,ITR) = BB
            IBR = 0

            RETURN
         END IF
	   
!  .  .  Attempt inverse interpolation

         IF (ABS(EE) .GT. TOL1 .AND. ABS(FAA) .GT. ABS(FBB)) THEN
            SS = FBB / FAA
            IF (AA .EQ. CC) THEN
               PP = 2. * XM * SS
               QQ = 1. - SS
            ELSE
               QQ = FAA / FCC
               RR = FBB / FCC
               PP = SS * (2. * XM * QQ * (QQ - RR) - (BB - AA)  &
                    * (RR - 1.))
               QQ = (QQ - 1.) * (RR - 1.) * (SS - 1.)
            END IF

!  .  .  .  Check if in bounds

            IF (PP .GT. 0.) QQ = -QQ
            PP = ABS(PP)

!  .  .  .  Accept interpolation

            IF (2. * PP .LT. MIN(3. * XM * QQ -ABS(TOL1 * QQ),  &
                                 ABS(EE * QQ))) THEN
               EE = DD
               DD = PP / QQ
               
!  .  .  .  .  Interpolation failed, use bisection

            ELSE 
               DD = XM
               EE = DD
            END IF

!  .  .  .  Bounds decreasing too slowly, use bisection

         ELSE
            DD = XM
            EE = DD
         END IF

!  .  .  Move last best guess to AA

         AA = BB
         FAA = FBB
	   
!  .  .  Evaluate new trial root

         IF (ABS(DD) .GT. TOL1) THEN

            BB = BB + DD

         ELSE

            BB = BB + SIGN(TOL1,XM)

         END IF
         PRICE(MRK,ITR+1) = BB
         INN = MRKDEF(MRK,2)
         NR = MRKDEF(MRK,3)
         DO LL = 1, NR
            L = MRKDEF(MRK,3+LL)
            P(INN,L,M) = BB
         END DO

         CALL MODEL
         
         ITR = ITR + 1
         FBB = MRKDEM(MRK) - MRKPRD(MRK)
         EXD(MRK,ITR) = FBB
         PROD(MRK,ITR) = MRKPRD(MRK)


  100 CONTINUE

!  .  No solution found, iterations exceeded.

!      WRITE(17,*)'  Iteration Limit Exceeded'

  150 IBR = 1


      ITR = ITR + 1
      PRICE(MRK,ITR) = BB
      NR = MRKDEF(MRK,3)
      INN = MRKDEF(MRK,2)
      DO LL = 1, NR
         L = MRKDEF(MRK,3+LL)
         P(INN,L,M) = BB
      END DO

!  .  Record all error exits from market solution so that debugging is
!  .  possible.

!      WRITE(17,'(''  PERIOD'',I5,''  MODL'',I5,''  MARKET'',I5)')
!     &   M,MODL,MRK
!      WRITE(17,*)'  Error exit from BRNT'
!      DO IR = 1,ITR
!         WRITE(17,'(2X,F12.5,2F12.0)')PRICE(MRK,IR),PROD(MRK,IR),
!     &      EXD(MRK,IR)
!      END DO

      RETURN
      END
