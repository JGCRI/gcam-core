!***********************************************************************
!
!     Library of functions used in the MiniCAM optimization.
!
!     Marshall Wise  9-29-95
!
!     expanded: sjs 06/01
!     regional groups can come in at different times
!     also
!	  can use combination of cumulative and global (conc or temp, but not both) targets
!     but cumulative emissions target MUST COME FIRST
!
!     cumulative emissions can also include land-use emissions
! WARNING!!!!!
! Inclusion of other GHG's in emissions totals is currently controled by argument
! 	IOTHGASTOG in MiniCAM
!
!***********************************************************************
!*********************************************************************

      SUBROUTINE HOTELCUM

!*********************************************************************
!  This subroutine finds an initial vector of taxes that satisfies the
!  constraint by searching for a scalar. 
!*********************************************************************

      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      
!     Adapted for flexible regional groupings with individual targets
!     maw 2/14/96 

!     5-25-96, maw added logic to set tax to 0 if solution is negative tax

      INTEGER LGFAIL(9) !Marks if a group failed its ind. test
      REAL*8 TAXHOLD(9)  !Saves starting point for search
      
      XACC = 0.005
      XACC2 = 0.05	! Org val
      XACC2 = 0.014	! Higher accuracy in 2nd loop is useful

! uncomment these for even higher accuracy
!       XACC  = 0.0035
!       XACC2 = 0.007

      NTRY=1000      
      XINCR0 = 5
      NJTRY=1000


!     Begin main loop
      
      DO I=1,NTRY
      
         DO LG=1,NLG
            LGFAIL(LG)=0
            TAXHOLD(LG) = TAX(LGMEMBC(LG,1),1)
         END DO
      
      XINCR1 = XINCR0/FLOAT(I)
      write(*,*)'trial # ', i

!     Loop over regional capping groups

      NLG_max = 1	! Default, don't loop over groups if global target
      DO LG=1,NLG	! Only loop over groups if target is cum emissions
      	IF (WHICHTARG(LG) .LE. 1) NLG_max = LG+1
      END DO
      IF (NLG_max .GT. NLG) NLG_max = NLG
 
! Start loop over reginal groups     
      DO LG = 1,NLG_max 

      XINCR=XINCR1
      LGCONV=0
      samdir = 1
      
!     Check with a zero tax to see if group is constrained at all, if not
!     pass through and go to next group      
      
      CALL TAXHOTEL(0.D0)
      FTEST = CUMDIFF(1)
      IF(FTEST.LT.0.D0) THEN         
         LGCONV=1
         GO TO 35
      END IF                           
      
!     Compute total group emissions and compare to target.

      X1=TAXHOLD(LG)
      
      CALL TAXHOTEL(X1)
      F1 = CUMDIFF(1)
      
!     If converged, mark as solved, also check to see if tax below 0.

      IF(ABS(F1).LE.XACC) THEN
         LGCONV=1
      END IF   
      
!     Iterate to get this market group to converge to target.
      
      jtry=0
      DO WHILE (LGCONV.EQ.0) 
         
         jtry=jtry+1
         
         IF (jtry .eq. 2 .and. f2 .gt. 0.05) THEN	! Starting too low -- up the increment. sjs
           XINCR = 250*f2
         END IF
         
         X2 = X1 + XINCR

         CALL TAXHOTEL(X2)
         
         F2 = CUMDIFF(1)
         write(*,*)
         write(*,*)"jtry, basetax, CumDiff: ",jtry,tax(lgmembc(lg,1),txstart2(lgmembc(lg,1))),f2               


! . . . .Check if new value is a root.  Also check to see if tax below 0.

         IF (ABS(F2).LE.XACC) THEN
            LGCONV=1

!        Check to see if root found between.  If so, do bisection.

         ELSE IF((F1*F2).LT.0.0) THEN
            
            IF(F1.LT.0.0) THEN
               RTBIS=X1
               DXBI=X2-X1
            ELSE
               RTBIS=X2
               DXBI=X1-X2
            END IF
            
            DO WHILE(LGCONV.EQ.0)
               jtry=jtry+1
               DXBI=DXBI/2.0
               XMID=RTBIS+DXBI

               CALL TAXHOTEL(XMID)
         
               FMID = CUMDIFF(1)
               write(*,*)'in bisect'
               write(*,*)"jtry, basetax, CumDiff: ",jtry,tax(lgmembc(lg,1),txstart2(lgmembc(lg,1))),fmid               
               IF(FMID.LE.0.0)RTBIS=XMID
!               IF(ABS(DXBI).LT.(XACC/10.D0) .OR. ABS(FMID).LE.XACC) THEN
!               IF(ABS(FMID).LE.XACC) THEN
               IF(ABS(DXBI).LT.(0.001D0) .AND. FMID.LT.0.D0  &
                         .OR. ABS(FMID).LE.XACC) THEN
                   LGCONV=1
!                  write(*,*)'bisect solved it!'
                   IF(ABS(DXBI).LT.(0.001D0)) LGFAIL(LG)=1
               END IF
               if(jtry.gt.1000) then
               	 MsgStr = "Solution failed in bisection"
                 Call MCLog(1,MsgStr,0,0,0,0)
                 write(*,*)'% off = ',fmid
                 write(*,*)'tax =    ',tax(lgmembc(lg,1),1)
!                stop
               end if

            END DO

! . . . .else new value is closer to root, continue in same direction.

         ELSE IF (ABS(F2).LT.ABS(F1)) THEN
            F1 = F2
            X1 = X2
            
            IF (mod(jtry,5) .eq. 0 .and. samdir .eq. 1) THEN	! Increase increment if not converging
                  XINCR = XINCR*2.0
            END IF
            
! . . . .Else, change direction of search, and cut increment in half.

         ELSE
            XINCR = - XINCR / 2.0
            samdir = 0		! Flag that have changed directions
         END IF
         
         if(jtry.gt.njtry/i) then
               	 MsgStr = "Solution failed"
                 Call MCLog(1,MsgStr,0,0,0,0)
            write(*,*)'target = ',cumtarg(lg)
            write(*,*)'actual = ',cumemfnc(dum)
            write(*,*)'tax =    ',tax(lgmembc(lg,1),1)
!            stop
            lgfail(lg)=1
            go to 35
         end if
         
      END DO  !For iteration loop within group
      
      write(*,*)'solved group ',lg
   35 continue
      
      END DO  !For loop over groups
      
!     Test all groups for convergence, if so, exit, if not do again
      
      ICNVRGE = 1
      DO LG=1,NLG               

         FTEST=CUMDIFF(0)   
         IF (abs(FTEST) .GT. XACC2) THEN
            IF(FTEST.GT.0.D0 .OR. TAX(LGMEMBC(LG,1),1).NE.0.D0) THEN
               IF(LGFAIL(LG).NE.1) THEN
                  ICNVRGE = 0
               END IF
             END IF
          END IF
       
      END DO
      
      IF(ICNVRGE.EQ.1) RETURN
         
      END DO  ! End of big loop over whole trials

!      WRITE(*,*)'carbon target solution failed'
!      STOP

	RETURN
      END
!***********************************************************************

!***  internal function definition to compute diff. of target and cum
!        emissions (Calls CUMEFNC function to get cum. emissions)
!
! Possible Target Options
!	-1 = Cumulative Emissions
!	1 = Cumulative Emissions
!	2 = CO2 Concentrations
!	3 = Temp Change (since PIL?, whatever comes out of magicc is used)
!	4 = CO2 Concentration Max


      FUNCTION CUMDIFF(IRUN)

      USE OPTICOM

	  USE COMMON, ONLY: MAGICCCResults  ! pass in all the MAGICC results

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  

	  ICType = 0  ! don't include carbon from land use
      IF(IRUN.GT.0) THEN
          IF (WHICHTARG(LG) .eq. -1) ICType = 1	! Add land use carbon to emissions total
          CALL MCAMLINK(INDOPT,TAX,GNP,CARB,ICType) ! run the minicam model once
      END IF
      

	  IF (WHICHTARG(LG) .LE. 1) THEN ! this is just the usual cumulative emissions target
	      CUMDIFF = (CUMEMFNC(DUM) - CUMTARG(LG)) / CUMTARG(LG)
	
      ELSE  ! these are targets from MAGICC
	  ! the target below is set for 2095, which is time index _9_ in minicam
 	      MagInt = 15/INT(MAGICCCResults(0,2)-MAGICCCResults(0,1))*(9 - 1)

		  IF (WHICHTARG(LG) .EQ. 2) TARGVAL = MAGICCCResults(2,MagInt)
	      IF (WHICHTARG(LG) .EQ. 3) TARGVAL = MAGICCCResults(1,MagInt)
	      IF (WHICHTARG(LG) .EQ. 4) TARGVAL = MAXVAL(MAGICCCResults(2,1:MagInt))

		  CUMDIFF = (TARGVAL - CUMTARG(LG)) / CUMTARG(LG)	
		  WRITE(*,*) "val, targ, diff",TARGVAL, CUMTARG(LG), CUMDIFF

	  END IF
	  
	  RETURN
      END

!***********************************************************************

!***********************************************************************
      
      FUNCTION CUMEMFNC(DUM)
      
      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      REAL*8 CARBLG(9)
      
      DO M=1,8			! Note that year index here is different from either MC or Opt routines...
        CARBLG(M)=0.D0
        DO LING=1,NLINGC(LG)
           L=LGMEMBC(LG,LING)
           CARBLG(M) = CARBLG(M) + CARB(L,M)
        END DO
      END DO

      CUMEMFNC = SUM(CARBLG(:))*15. - (CARBLG(8)+CARBLG(1))*15./2.0
      
      RETURN
      END
!***********************************************************************
!***********************************************************************
      
      SUBROUTINE TAXHOTEL(TAXBASE)
      
      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      REAL*8 TAXBASE
!     set to 0 if negative
      IF(TAXBASE.LT.0.D0) TAXBASE = 0.D0
      
      NLG_min = LG	
      NLG_max = NLG	! Default loop over all groups if global target
      IF (WHICHTARG(LG) .LE. 1) NLG_max = LG		! Don't loop over groups if target is cum emissions
      
      FirstTax = 10
      DO LocLG = NLG_min,NLG_max 
         FirstTax = Min(1d0*txstart2(LocLG),FirstTax)	! First period in which any c-tax is levied.
      END DO
      

      DO LocLG = NLG_min,NLG_max
      DO LING=1,NLINGC(LocLG)
      DO M=1,NTAXES
       L=LGMEMBC(LocLG,LING)
       IF (M .ge. txstart2(LocLG)) TAX(L,M) = TAXBASE*(1+IRATE)**(15*(M-FirstTax))
       IF (M .lt. txstart2(LocLG)) TAX(L,M) = DefaultTax(L,M)	! Optional read-in tax
      END DO
       
!         write(*,*) "default tax: ",(DefaultTax(L,MM), MM =1,NTaxes)
!        write(*,*) "taxes, reg ",L," : ",(TAX(L,MM), MM =1,NTaxes)

      END DO
      END DO

      RETURN
      END
!***********************************************************************
