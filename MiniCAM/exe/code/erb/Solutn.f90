!**********************************************************************

      SUBROUTINE SOLUTN
!**********************************************************************
!
!	MINICAM VERSION  MARCH 1997
!
!     Modified Newton-Raphson search for market equilibrium prices.
!     N-R is modified to call a non-derivative based line search when
!     it finds a discontinuity that causes the derivative to provide
!     inadequate information for a given market.  Also modified in that
!     derivatives from the previous iteration are recycled for markets
!     that are within tolerance, which reduces model evaluations.
!
!     Created by Marshall Wise  March 1996 for the SGM
!
!**********************************************************************
!
!     Notes and explanations:
!
!
!     Each period, the markets whose price is fixed or which haven't
!     come on line are weeded out to avoid a singular matrix error.
!     Prices are perturbed to get derivatives (elasticities) for the 
!     N-R search.  If, after a few calls, any markets are off by a set
!     amount, the line search SOLVE is called for them before the N-R
!     procedure continues.  SOLVE is also called when an iteration 
!     price causes a market's production to go to zero.  In both cases
!     SOLVE is called because the N-R routine is faced with a discont-
!     inuity.  Also, after 1 iteration, if a market is solved within a set 
!     tolerance, the derivative is recycled for the next iteration,
!     saving a model evaluation. 
!
!     After the first period, the derivatives from the solution set of
!     the previous period are re-used for the first iteration of the
!     current period, also potentially reducing model evaluations.
!     
!     If the procedure is unsuccessful after a number of tries, the 
!     SOLUTA is called to finish the job. 
!
!**********************************************************************

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
!      PARAMETER (TINY = .0001, WEE = 1.E-12)
      PARAMETER (TINY = .0001, WEE = 1.E-11)
      DIMENSION NEWT(NMRKP),ICLOSE(NMRKP),MSOLCALL(NMRKP)
	DIMENSION ISORT(NMRKP),IPRESORT(NMRKP)
      REAL*8 DEMI(NMRKP),SUPI(NMRKP),DP(NMRKP),DELTA,XVEC(NMRKP)
      REAL*8 UIJ(NMRKP,NMRKP),VIJ(NMRKP,NMRKP),W(NMRKP,NMRKP)
      REAL*8 DUMMAT(NMRKP,NMRKP)  !Not used, needed for GAUSSJ      
      REAL*8 PMARK(NMRKP),XGHOST

      DELTAP=0.00001D0   ! Derivative delta
      CONV=TEST  ! Used in SOLVE
      TEST2S=TEST !0.005D0   ! Tolerance for testing to call SOLVE
      TESTCLOS=TEST   !Tolerance for test if derivative can be re-used
      MAXNEWT=NMRK/4      !  Max iterations before quitting 
      maxmodl=NMRK*500   ! Max number of model runs to failure
      WAYOFF=1000.0*TEST   !Tolerance to call solve immediately
!      WAYOFF=100.0*TEST   !Tolerance to call solve immediately
	NUMNUTS = 10*NMRK !Number of Newton iterations before check to call solve
	          ! for markets that are still outside TEST2S
	ISCREEN=0  !Indicator to write detailed solution steps to screen
      IORDOPT=2  !Indicator for calling solve in descending order of dis
         !equilibrium absolute (=1)       7-24-97
	   !relative disequilibrium (=2)

      NFAIL=1        ! Model solve indicator
      NITRNEWT=-1      !No. of iterations
	MODL=0

      sjstest = 0
          
!     Give carbon market a positive starting point if it's on
      DO L=1,NL
	   IF(NTAXMODE(L).EQ.4 .AND. CEMTARGS(L,M).GT.0.0) THEN
	      P(INCARB,L,M) = 1.0
	   END IF
	END DO

!     Make sure all regions in same market share price.

      DO I = 1,NMRK
         IMRK = I
         INN = MRKDEF(IMRK,2)
         NR = MRKDEF(IMRK,3)
         
!        Update all regional prices with the 1st region prices
         L1 = MRKDEF(IMRK,4)
         IF (IPFIX(INN,L1,M).EQ.0) THEN
            DO LL = 1,NR
               L = MRKDEF(IMRK,3+LL)
               P(INN,L,M) = P(INN,L1,M)
            END DO
         END IF
         PMARK(IMRK) = P(INN,L1,M)
      END DO

!     Big Do WHILE loop that runs whole thing until convergence

      DO WHILE(NFAIL.EQ.1)
	      
!     Test to see if max iterations exceeded

	IF (MODL.GT.MAXMODL) THEN

        MsgStr = "N-R SOLUTION FAILED: MAXMODL Exceeded "
        Call MCLog(1,MsgStr,M,0,0,0) 
        STOP

      ELSE IF(NITRNEWT.GT.MAXNEWT) THEN
	   MAXNEWT=MAXNEWT*2
	   WAYOFF = MAX(WAYOFF/10.D0,TEST)

!        Reset all prices
!         DO IMRK = 1,NMRK
!            INN = MRKDEF(IMRK,2)
!            NR = MRKDEF(IMRK,3)           
!         	 IF (IPFIX(INN,L1,M).EQ.0) THEN
!            DO LL = 1,NR
!               L = MRKDEF(IMRK,3+LL)
!               P(INN,L,M) = PMARK(IMRK)
!            END DO
! 	      END IF
!         END DO

      ELSE
        NITRNEWT=NITRNEWT+1 
        if(iscreen.eq.1) then
         write(6,*)'n-r iter ',nitrnewt
         write(6,*)'Market','   Input','  Region','  Period', &
         '   Excess Demand','     Prices'
	  end if
      END IF      
      
!     Get initial values from inital model run
      
      MRK=0
      CALL MODEL   

      
!***********************************************************************
      
!     Loop over markets in Newton-Raphson (N-R) Search, check disequilibrium
      
      isolcall=0 !Flag tells if SOLVE has been called this iteration
      isolcnt=0  !Number of times SOLVE called 
!     Initialize flags to call solve at false (0)
      
      DO I=1,NNEWT
         MSOLCALL(I)=0
      END DO

!     Counter to break out of infinite loop      
	IGOTO10=0  

   10 CONTINUE
   
      izerop=0

!     Assign mapping to markets that do not have prices fixed or zero
!     production and demands in that period (to avoid singular matrix)

!     NNEWT stores the dimension of the matrix
!     NEWT stores pointers from matrix dimensions to the market numbers

      NNEWT=0
      DO IMRK=1,NMRK
         INN = MRKDEF(IMRK,2)
         NR = MRKDEF(IMRK,3)
         L = MRKDEF(IMRK,4)

!        First, check to see if market is the carbon market.  If so, check
!         to see if a carbon tax or restriction is in place

	   IF (INN.EQ.INCARB) THEN

           IF (NTAXMODE(L).EQ.4 .AND. CEMTARGS(L,M).GT.0.0) THEN

!            If emissions are below constraint with tax market turned on,  
!            then set tax = 0 and make carbon market a fixed price market.

!            In certain situations, carbon market is discontinuous when 
!            tax is near zero.  A tax=0 will lead to over>0 and a slighly higher
!            tax will lead to over<0.  Setting tax=0.49 resolves this problem for
!            the moment.  The real problem is in the market and such small changes 
!            in the price should not create such large changes in excess demand.
!            SHK  3/15/99

	       OVER = MRKDEM(IMRK)-MRKPRD(IMRK)
             IF(OVER .LE. 0.0 .AND. P(INCARB,L,M) .LE. 1.0) THEN

                DO LL=1,NR
	            LLL=MRKDEF(IMRK,3+LL)
                  IPFIX(INCARB,LLL,M) = 1
                  P(INCARB,LLL,M) = 0.49
	          END DO

!             If, during iterations, carbon market was set to fixed price
!             but is later found to need a positive carbon tax to reduce
!             emissions, set IPFIX to false and give carbon price a non-zero
!             starting point.

             ELSE IF (IPFIX(INCARB,L,M).EQ.1 .AND. OVER .GT. 0.0) THEN        

                DO LL=1,NR
	            LLL=MRKDEF(IMRK,3+LL)
                  IPFIX(INCARB,LLL,M) = 0
	            call random_seed()
	            call random_number(xghost)
                  P(INCARB,LLL,M) = 10.0*XGHOST
	          END DO

             END IF
           END IF
         END IF


!     Check to see if market is a fix-priced market and take it out of the 
!     solution procedure

         IF(IPFIX(INN,L,M).EQ.1) THEN

!     Do nothing at this point

!         If price is not fixed and market production is non-zero, add
!         market to solution matrix

         ELSE IF(MRKPRD(IMRK).GT.wee .OR.  &
          MRKDEM(IMRK).GT.wee) THEN

	      IF(ABS(MRKPRD(IMRK)-MRKDEM(IMRK)) .GT. wee) THEN

               NNEWT = NNEWT+1
               NEWT(NNEWT) = IMRK
	      
            END IF
         
         END IF
            
      END DO                                                            
            
      NFAIL=0  !Initialize at success, resets to fail if not convererged
      
      DO I=1,NNEWT
         IMRK=NEWT(I)  ! Assign pointer to market index
!        Store demands and supplies for elasticity computations         
         DEMI(I) = MRKDEM(IMRK)
         SUPI(I) = MRKPRD(IMRK)
         
         INN = MRKDEF(IMRK,2)
	   L1 = MRKDEF(IMRK,4)
!     Compute X vector

         IF(SUPI(I).GT.0.D0 .AND. DEMI(I).GT.0.D0) THEN
            XVEC(I) = DLOG(DEMI(I)) - DLOG(SUPI(I))
            if(iscreen.eq.1) then
	        IF (DABS(XVEC(I)) .GT. TEST) &
              write(6,'(I5,I8,I9,I7,G16.4,F14.6,A,F11.3)')imrk,inn,L1,M, &
              xvec(i),P(INN,L1,M),"   d= ",MRKDEM(IMRK)
	      end if
      
!        If either prod. or demand is zero, flag to call SOLVE
            
         ELSE 
           MSOLCALL(I)=1
           izerop=1   !Indicates call to SOLVE due to zero production
!          7-24-97
           XVEC(I)=(SUPI(I)-DEMI(I))/MAX(DABS(SUPI(I)),DABS(DEMI(I)))
           if(iscreen.eq.1) then
             write(6,'(I5,I8,I9,A12,F11.6,A4,G10.4,A4,G10.4)')imrk,inn, &
             L1,'Call Solve',P(INN,L1,M),'d=',demi(i),'s=',supi(i)
	     end if
         END IF
         
!     Check for close to convergence
      
         IF (DABS(XVEC(I)) .GT. TEST)THEN
            NFAIL=1
            ICLOSE(I)=0
            IF(DABS(XVEC(I)).LT.TESTCLOS) THEN
               ICLOSE(I)=1
            END IF
         ELSE
            ICLOSE(I)=1
         END IF   
         
      END DO
      
!     If all markets have converged, return, else continue

      IF(NFAIL.EQ.0) THEN

         IF(M.GT.1 .OR. NITRNEWT.GT.0) THEN         
           WRITE(6,'(A,I4,A,I4,A,I4)')'N-R SOLVED IN PD',M,' IN ', &
           NITRNEWT,' ITERATIONS AND MODL ',MODL
           
           MsgStr = "N-R SOLVED. # model calls:"
            Call MCLog(3,MsgStr,M,0,0,MODL*1d0)
!           Write(97,*) "   (in ",NITRNEWT," Iterations)"
           RETURN         
         END IF
         
      END IF

!     If solve has been called this iter., better redo all derivatives
      
      IF (ISOLCALL.EQ.1) THEN
         DO I=1,NNEWT
            ICLOSE(I)=0
         END DO
      END IF

!     After a few passes, if any single market is off by a lot, solve it 
!        separately first before doing the elasticities.  Also, solve
!        any market whose production went to zero on last iteration.
      
      ISOLCALL=0
      IF(NITRNEWT.GE.NUMNUTS)THEN
!        Stick at it a few times, then bump up numnuts
         IF(INUM.LE.5) THEN
	      INUM=INUM+1
	   ELSE
            NUMNUTS=NUMNUTS*1.4
            INUM=0
	   END IF
         DO I=1,NNEWT
            IF (DABS(XVEC(I)).GT.TEST2S) THEN
               MSOLCALL(I)=1
            END IF
         END DO
      END IF

!     If solve called too much this pass, could be bouncing
!     so skip call and redo derivatives (unless call was to be made
!     to avoid zero production)

      if(isolcnt.gt.(nmrk/4) .and. izerop.eq.0) then
         do i=1,nnewt
            msolcall(i)=0
         end do 
         isolcnt=0
      end if

!     if a market is ever way way off, solve it so it doesn't drag others
!     out with it, regardless of no. of iterations

      if(nitrnewt.gt.0) then
        do i=1,nnewt
         if(dabs(xvec(i)).gt.wayoff) then
!         if(dabs(xvec(i)).gt.test) then
            msolcall(i)=1
         end if
        end do
      end if

!************************************************
!     Compute order for calling solve  7-24-97

      NISOLVE=0
      DO I=1,NNEWT   !First map all being called 
	   IF(MSOLCALL(I).EQ.1) THEN
	      NISOLVE=NISOLVE+1
	      IPRESORT(NISOLVE)=I
	   END IF
	END DO

!     perform a sort for descending order of diseqilibrium
      DISMAX=0.0
	DO I=1,NISOLVE
         DISMAX=0.0
         DO J=1,(NISOLVE+1-I)
	      IMAP=IPRESORT(J)
	      TESTDIS = DABS(XVEC(IMAP))
	      IF(IORDOPT.EQ.2)  &
          TESTDIS = (1.0+TESTDIS)*MAX(DABS(SUPI(IMAP)),DABS(DEMI(IMAP))) 
            IF(TESTDIS.GT.DISMAX) THEN
	         DISMAX=TESTDIS
	         IMAX=IMAP
	         JMAX=J
	      END IF
	   END DO
	   IF(IORDOPT.GE.1) THEN
	      ISORT(I) = IMAX
	      DO J2=JMAX,NISOLVE-I
	         IPRESORT(J2) = IPRESORT(J2+1)
	      END DO
	   ELSE
            ISORT(I) = IPRESORT(I)
	   END IF      
      END DO
	   
      
!     Call SOLVE for those in need of assistance

      DO I2=1,NISOLVE  !7-24-97
	      
         I=ISORT(I2)  !7-24-97
         ISOLCALL=1
         MSOLCALL(I)=0   !Reset
	   isolcnt=isolcnt+1
         isolvec=0
   
   20	   MRK=NEWT(I)
         CALL SOLVE
	   isolvec=isolvec+1

!        Verify solve criterion (since it can kick out early)
         IMRK=NEWT(I)  ! Assign pointer to market index
!        Store demands and supplies for elasticity computations         
         DEMI(I) = MRKDEM(IMRK)
         SUPI(I) = MRKPRD(IMRK)
         XVEC(I) = DLOG(max(DEMI(I),wee)) - DLOG(max(SUPI(I),wee))

      IF ((DABS(XVEC(I)).LT.TEST) .and. (iscreen.eq.1)) Then
             INN2 = MRKDEF(IMRK,2)
             write(6,'(I5,I8,I9,I7,G16.4,F14.6,A,F12.3,F12.3)')imrk,INN2,L1,M, &
              xvec(i),P(INN2,L1,M), " solved. d= ",MRKDEM(IMRK)
       End If
         IF (ISOLVEC .GT. 30) THEN
	      IF(ISCREEN.EQ.1) THEN
	         WRITE(6,*)'Call to SOLVE exceeds 30'
	      END IF
	      GO TO 30
	     END IF

         IF (DABS(XVEC(I)).GT.TEST) GO TO 20  !Pardon my goto

!         IF (DABS(XVEC(I)).GT.TEST) THEN
!            ISOLCALL=1    
!            GO TO 20  !Pardon my goto
!         ELSE
!            ISOLCALL=0
!         END IF

   30    ISOL=0 !To not screw up output in outexcel.for

      END DO

!     Recheck convergence and compute new excess demands
!     If go to 10 exceeds 5 times then perturb prices to
!     avoid infinite loop.  shk 8/17/99
      
      IF(ISOLCALL.EQ.1 .AND. IGOTO10.LT.10) THEN
	   IGOTO10=IGOTO10+1
	   if(iscreen.eq.1) then
            WRITE(6,*)'Solve called'
!	      do i2=1,nisolve
!	         write(6,*)isort(i2)
!	      end do
	   end if
         GO TO 10  !Father forgive me.
      END IF


      CALL UNPERTURB(0)
!***********************************************************************

!     Here is the meat of the Newton-Raphson

!     Re-use last period's derivatives for first iteration

      IF(M.LE.2 .OR. NITRNEWT.GE.1) THEN
      
!     Compute numerical elasticities

      DO I=1,NNEWT   ! Loop over markets (matrix rows)


      CALL UNPERTURB(1)


!     Only compute new deriv if indiv. market is not close
!     (or if it's first pass)

      IF (ICLOSE(I).EQ.0 .OR. NITRNEWT.EQ.0) THEN
      
         IMRK = NEWT(I)
         INN = MRKDEF(IMRK,2)
         NR = MRKDEF(IMRK,3)
         
!        Perturb prices
         L1 = MRKDEF(IMRK,4)
	   PMRK = P(INN,L1,M)
	   DELTA = PMRK*DELTAP
         DO LL = 1,NR
            L = MRKDEF(IMRK,3+LL)
            P(INN,L,M) = PMRK + DELTA
         END DO

!     Call the model with the price perturbation
         MRK=IMRK
         CALL MODEL

         INN = MRKDEF(IMRK,2)
         NR = MRKDEF(IMRK,3)

         L1 = MRKDEF(IMRK,4)
!     Loop over markets and compute the own- and cross-elasticities         
         
         DO J=1,NNEWT   ! Fills out matrix columns
         
           JMRK = NEWT(J)  !Assign pointer to market
            
           DEMTMP = MRKDEM(JMRK)
           UIJ(J,I)=(DEMTMP-DEMI(J))/DEMI(J)/(DELTA/(P(INN,L1,M)-DELTA))

           SUPTMP = MRKPRD(JMRK)
           VIJ(J,I)=(SUPTMP-SUPI(J))/SUPI(J)/(DELTA/(P(INN,L1,M)-DELTA))

         END DO
         
!        Return prices to original value

 	   pmrk = p(inn,l1,m)
         DO LL = 1,NR
            L = MRKDEF(IMRK,3+LL)
            P(INN,L,M) = pmrk - DELTA
         END DO
      
      END IF  !If Block that checks if market already close or solved
      END DO

! This is code to print out a sample supply/demand iteration for illustration purposes
IF (MODL .gt. 1 .and. MRK .eq. 2.and.sjstest.eq. -1) THEN
  INN = MRKDEF(IMRK,2)
  PSavesjs = P(INN,L,M)
    write (*,'(4I3," Gas Price, Demand, Supply: ",10f7.2)') M,L,1,MRK, &
    MRKDEM(MRK),MRKPRD(MRK),P(MRKDEF(MRK,2),L,M)
  P(INN,L,M) = PSavesjs*1.2
  CALL MODEL
    write (*,'(4I3," Gas Price, Demand, Supply: ",10f7.2)') M,L,2,MRK, &
    MRKDEM(MRK),MRKPRD(MRK),P(MRKDEF(MRK,2),L,M)
  P(INN,L,M) = PSavesjs/1.2
  CALL MODEL
    write (*,'(4I3," Gas Price, Demand, Supply: ",10f7.2)') M,L,3,MRK, &
    MRKDEM(MRK),MRKPRD(MRK),P(MRKDEF(MRK,2),L,M)
  P(INN,L,M) = PSavesjs
  CALL MODEL
  sjstest = 1.0
END IF
            
!     Compute W matrix, invert, multiply by X vector to get new prices
!        for next iteration.

      DO I=1,NNEWT
         DO J=1,NNEWT
            W(I,J)=UIJ(I,J)-VIJ(I,J)
         END DO
      END DO
      
      
!     Call matrix inversion subroutine to invert W.

      CALL GAUSSJ(W,NNEWT,NMRKP,dummat,1,1)
      
    IF(M.eq.666) Write(*,'(a2,I4,a7,11f6.1)') " ",MODL, " P: ", P(1:11,1,M)

      END IF  !If Block to check if first iteration
      
!     Multiply W inverse by XVEC to get price changes.

      DO I=1,NNEWT
         DP(I)=0.0
         DO J=1,NNEWT
            TEMP=-W(I,J)*XVEC(J)
            DP(I)=DP(I)+TEMP
         END DO
         IF(DP(I).LT.-1.D0) DP(I)=-0.9D0
!	if(dp(i).gt.2.0) dp(i)=2.0
      END DO
      
!     Compute new prices.

      if(nfail.eq.1) then
       DO I=1,NNEWT
         IMRK=NEWT(I)
         INN = MRKDEF(IMRK,2)
         NR = MRKDEF(IMRK,3)

         L1 = MRKDEF(IMRK,4)
         pmrk = p(inn,l1,m)

         DO LL = 1,NR
            L = MRKDEF(IMRK,3+LL)
            P(INN,L,M) = pmrk*(1.D0+DP(I))
         END DO
       END DO                                                            
      end if
      
!***********************************************************************      

      END DO  ! End of big Do While loop
      
      RETURN
      END

   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     GAUSSJ is a Numerical Recipes routine used here for inverting a
!     matrix.  It is used here instead of MINV from the ERB because
!     it is more general in that it allows the order of the matrix to
!     be less than the memory allocation dimension of the matrix.
!
!     a is the two-dimensional matrix to be inverted
!     n is the order of the matrix
!     np is the physical dimension of the 'a' array from the main program
!     b is a matrix (not used here)
!     m,mp are the columns of b (not used here)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      SUBROUTINE gaussj(a,n,np,b,m,mp)
      INTEGER m,mp,n,np,NMAX
      REAL*8 a(np,np),b(np,mp)
      PARAMETER (NMAX=40)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      REAL*8 big,dum,pivinv
      CHARACTER*80 MsgStr	! Need to add this def for error routine. sjs
      
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (dabs(a(j,k)).ge.big)then
                  big=dabs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                MsgStr = "singular matrix in gaussj "
        		Call MCLog(1,MsgStr,M,0,0,0) 
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) THEN
            MsgStr = "singular matrix in gaussj "
        	Call MCLog(1,MsgStr,M,0,0,0) 
        END IF
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END
!  (C) Copr. 1986-92 Numerical Recipes Software ]2o<,~n.
!***********************************************************************    

