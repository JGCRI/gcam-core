!***********************************************************************
!
      SUBROUTINE   DDDD
!***********************************************************************
!
!                  -- THE ENERGY DEMAND MODULE --
!
! THIS SUBROUTINE COMPUTES THE DEMAND FOR PRIMARY AND SECONDARY ENERGY
!     BY REGION AND SECTOR.
!
!
! INTEGER INPUTS: M, NF, NSYN, NI, NJ, NKKL, NKL, NKKMAX, NKMAX, NL, NM,
!                 NU
!
! REAL INPUTS:    BSJKLM, BSKL, ESIL, GIJ, GJ, GJK, GUILM,
!                 PJL, PJKL, PKL, RPJ, RPJK, RPK, RPKK, RYJ, RYJKLM,
!                 RYKLT, RYKLM, SCIL, SUIL, TESIL, YLM, ZLM
!
! REAL OUTPUTS:   EDIKL, EDIL, EDRIKL, EFJKL, ESIL, FJKL, FJL, SJKL, SKL
!
!   SUBROUTINES CALLED: NONE
!
!   WRITTEN BY:
!     JAE EDMONDS                      LATEST REVISION:
!     1 JANUARY 1982                  1 SEPT 88 TO INCLUDE COMMON
!                                       20 JAN 90 TO LIMIT BIOMASS
!                                       PRODUCTION TO REGIONAL SOLIDS
!                                       DEMAND.
!                                    3 JAN 91 TO PROVIDE THREE SECTORS 
!                                         TO ALL REGIONS
!                                    7 JAN 91 ADDED TIME SUBSCRIPTS TO 
!                                       RYJKLM AND RYKLM
!                                    13 APR 94  MADE TKLM COMPOUND FROM
!                                       BEGINNING OF MODEL RUN
!     Incorporated end-use demand for hydrogen  2/2/98 maw
!***********************************************************************
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
!  LOCAL VARIABLES
!
      REAL*8  E,FK(NKP),RATIO,SHARE,Sh_SUM,T,TT,X,TECH
      INTEGER NK,NT
      REAL*8 SUIL_Temp(NUP)
      
!
!     Test to see if carbon scrubbing policy is on
      IF(ICARBOUT(L,1).NE.0 .AND. M.GT.ICARBOUT(L,1)) THEN
	   ISCRUB=1
!        see if conversion to h2 is on
         IF(ICARBOUT(L,2).NE.0 .AND. M.GT.ICARBOUT(L,2)) THEN
	      IGOH2=1
	   ELSE
	      IGOH2=0
	   END IF
	ELSE
	   ISCRUB=0
	END IF
      EDSUM=0.D0
!  +-----------------------------------------+
!  !   INTERPOLATE INCOME ELASTICITY VALUES  !
!  +-----------------------------------------+
!
      NT=(M-1)*NJUMP
      T=NT
      TT=NJUMP
!
!      DO 150 L=1,NL
      NK=NKL(L)
      X=YLM(L,M)/(ZLM(L,M)/ZLM(L,1))
	if ( m .gt. 1) then
         XX = (ylm(l,m)/zlm(l,m)) / (ylm(l,m-1)/zlm(l,m-1))
	else
	   xx = 1
	end if
!  +------------------------------------------------------------+
!  |  CALCULATE FK, THE DEMAND FOR ENERGY SERVICES BY SECTOR K  |
!  +------------------------------------------------------------+
      ESERV(L,M)=0.0
	DO 30 K=1,NK
	! Changed so that prices work from 1990 base.
	! This is so changes in parameters create consistant base values of energy service
	! Otherwise has no effect since 1990 energy use calibrates TKLM. sjs -- 11/01
	
         FK(K)=BSKL(K,L)*(PKLM(K,L,M)/PKLM(K,L,2))**RPKL(K,L)*X**RYKLM(K,L,M)
         FK(K)=FK(K)*ZLM(L,M)/ZLM(L,1)
         IF(K.EQ.2) FK(K)=BSKL(K,L)*(PKLM(K,L,M)/PKLM(K,L,2))**RPKL(K,L)
         IF(K.EQ.2) FK(K)=FK(K)*YLM(L,M)**RYKLM(K,L,M)
      
       IF (ISRESeserDmd .eq. 1) then   ! Alternative formulation used in org. SRES scenarios
	   temprp = RPKL(K,L)
	   if (k .ne. 2) then
	      if (m .eq.1) then
               FK(K) = BSKL(K,L) * PKLM(K,L,M) ** temprp &
                           * x ** RYKLM(K,L,M)
	      else
               FK(K) = ESERVK(K,L,M-1)  &
                           * (PKLM(K,L,M)/PKLM(K,L,M-1)) ** temprp &
                           * xx ** RYKLM(K,L,M)
               FK(K)=FK(K)*ZLM(L,M)/ZLM(L,m-1)
	      end if
         else IF(K.EQ.2) then
	      if (m .eq. 1) then
               FK(K) = BSKL(K,L) * PKLM(K,L,M) ** temprp
	      else
               FK(K) = ESERVK(K,L,M-1)  &
                           * (PKLM(K,L,M)/PKLM(K,L,M-1)) ** temprp
               FK(K) = FK(K) * xx ** RYKLM(K,L,M) &
                           * ZLM(L,M) / ZLM(L,m-1)
	      end if
	   end if        
       END IF
	   ESERV(L,M) = ESERV(L,M) + FK(K)
         ESERVK(K,L,M)=FK(K)
       
   30 CONTINUE
!
!      COMPUTE NUMERATOR AND DENOMINATOR OF SJKL.
!      USE SJKL AS TEMPORARY NUMERATOR.
!
   39 DO 90 K=1,NK
         Sh_SUM=0.D0
         DO 70 J=INOIL,INGAS
            SJKL(J,K,L)=BSJKLM(J,K,L,M)*PJKLM(J,K,L,M)**RPJK(J,K) &
                    *X**RYJKLM(J,K,L,M)
            Sh_SUM=Sh_SUM+SJKL(J,K,L)
          
         !for debugging
   70    CONTINUE
!     coal vs. biomass subsectors for solid fuels
	   SUMSS=0.0
         J=INCOAL
	   PSSJKLM(K,1) = PJKLM(J,K,L,M)
	   PSSJKLM(K,2) = PJKLM(JSBMASS,K,L,M)
         DO ISS=1,2
	      SSJKL(ISS) = BSSJKLM(K,ISS,L,M)*PSSJKLM(K,ISS) &
                        **RSSPJK(K,ISS)
            SUMSS = SUMSS + SSJKL(ISS)
         END DO

!     Compute subsector shares and avg. price for doing sector share
	   PJKLM(J,K,L,M) = 0.0
         DO ISS=1,2
	      SSJKL(ISS) = SSJKL(ISS)/SUMSS
	      PJKLM(J,K,L,M) = PJKLM(J,K,L,M)+SSJKL(ISS)*PSSJKLM(K,ISS)
	   END DO   
!     Compute solids sector share
         SJKL(J,K,L)=BSJKLM(J,K,L,M)*PJKLM(J,K,L,M)**RPJK(J,K) &
                    *X**RYJKLM(J,K,L,M)
         Sh_SUM=Sh_SUM+SJKL(J,K,L)
!     Now do electricity as a secondary fuel
         J=NJ
         SJKL(J,K,L)=BSJKLM(J,K,L,M)*PJKLM(J,K,L,M)**RPJK(J,K) &
                    *X**RYJKLM(J,K,L,M)
         Sh_SUM=Sh_SUM+SJKL(J,K,L)
!     Hydrogen as a secondary fuel (same elasticity as electricity since H2 is envisoned as fuel cells producing electricity)	
         J=JSH2
         SJKL(J,K,L)=BSJKLM(J,K,L,M)*PJKLM(J,K,L,M)**RPJK(NJ,K) &
                    *X**RYJKLM(NJ,K,L,M)
         Sh_SUM=Sh_SUM+SJKL(J,K,L)

! Re-normalize
         DO 80 J=1,NJ  !Loop thru electric
            SJKL(J,K,L)=SJKL(J,K,L)/Sh_SUM
   80    CONTINUE
         J=JSH2       !Hydrogen
         SJKL(J,K,L)=SJKL(J,K,L)/Sh_SUM

!     Split solids demand between coal and biomass
	   J=INCOAL
         SJKL(JSBMASS,K,L) = SSJKL(2) * SJKL(J,K,L)
	     SJKL(J,K,L) = SJKL(J,K,L) - SJKL(JSBMASS,K,L)
   90 CONTINUE	! End end-use sector "k" loop
   
   
!     ***** Transportation
       IF (NewTransp .eq. 1) CALL TRANSPORT
!
!     COMPUTE TOTAL FUEL DEMANDS
      DO 140 J=1,NNJ  !(Secondary energy including Biomass and H2)                                      
         FJL(J,L)=0.D0
         DO 130 K=1,NK
            TECH=1.0
            DO MTECH = 2,M
              TECH = TECH * (1.D0+TKLM(K,L,MTECH))**NJUMP
            END DO   
!           Test new transportation module for US only, convert to EJ
          IF (K.EQ.3 .AND. L.EQ.1 .and. NewTransp .eq. 1) THEN
               FJKL(J,K,L)=TRANFUEL(3,NTMODE+1,J,L,M)*1.055/1.05	! Convert to EJ, and then from HHV to LHV
		  ELSE           
               FJKL(J,K,L)=SJKL(J,K,L)*FK(K)/TECH
               FJKL(J,K,L)=FJKL(J,K,L)*GJKLM(J,K,L,M)
          END IF
  120       FJL(J,L)=FJL(J,L)+FJKL(J,K,L)
  130    CONTINUE
  140 CONTINUE
! 
!  150 CONTINUE
!
      NK=NKL(L)
      NKK=NKKL(L)

!
!
!    +----------------------------------------------------------+
!    |   REALIGN ELECTRIC UTILITY SHARES TO CONFORM TO SUPPLY   |
!    |   ASSUMPTIONS AND FINAL DEMAND LEVELS                    |
!    +----------------------------------------------------------+
!
!     Check to see if a carbon scrubbing policy is on, if so consider
!     fixed (from g-father clause) supply of unscrubbed fossil power
      FIXEDCAP=0.0
      IF(ISCRUB.EQ.1) THEN
	   DO I=INOIL,INCOAL
	      FIXEDCAP = FIXEDCAP + ESUILM(I,L,M)
	   END DO
      END IF
	FIXEDCAP = FIXEDCAP + ESIL(NI,L)  !Hydro always fixed
!     Scale down if fixed capacity exceeds demand
      IF(FIXEDCAP .GT. FJL(NJ,L)) THEN
         SCALE=FJL(NJ,L)/FIXEDCAP
         FIXEDCAP=FJL(NJ,L)
	   IF(ISCRUB.EQ.1) THEN
	      DO I=INOIL,INCOAL  !fossil fuels
	         ESUILM(I,L,M) = ESUILM(I,L,M) * SCALE
	      END DO
	   END IF
	   ESIL(NI,L) = ESIL(NI,L) * SCALE  !Scale back hydro
	END IF
 
!  Adjust to get proper hydro share
!  Note that, up till this point, SUIL(NI,L) is not actual hydro share
        
        Call RebalanceEShares(NI,ESIL(NI,L)/FJL(NJ,L),SUIL(NI,L))
      
!     Correct shares of carbon vs scrubbed under policy
      IF(ISCRUB.EQ.1 .AND.IGOH2.EQ.0) THEN
!        Oil
         SUIL(JUOSCRUB,L)=SUIL(JUOSCRUB,L)-(ESUILM(INOIL,L,M)/FJL(NJ,L) &
                     - SUIL(INOIL,L))
	   SUIL(INOIL,L) = ESUILM(INOIL,L,M)/FJL(NJ,L)
!        Gas
         SUIL(JUGSCRUB,L)=SUIL(JUGSCRUB,L)-(ESUILM(INGAS,L,M)/FJL(NJ,L) &
                     - SUIL(INGAS,L))
	   SUIL(INGAS,L) = ESUILM(INGAS,L,M)/FJL(NJ,L)
!        Coal
         SUIL(JUCSCRUB,L)=SUIL(JUCSCRUB,L)-(ESUILM(INCOAL,L,M)/FJL(NJ,L) &
                     - SUIL(INCOAL,L))
	   SUIL(INCOAL,L) = ESUILM(INCOAL,L,M)/FJL(NJ,L)
      END IF
! ************************************************************************************
! Solution to simultaneity problem for H2. sjs -- 03/01
! Because H2 can be used to produce electricity AND electricity can be used to produce H2.
! Implements an algebraic solution to the problem, 
!		accounting for possible consistancy problem with electric generation
!		(must be enough non-H2 electricity to generate H2 for fuel cells!)
         SUIL_Temp(:) = SUIL(:,L)	! Save elec gen limits
!   These inputs are generally not changed by routines below. Can set only once.
      DH_eu  = FJL(JSH2,L)	! Total H2 demanded by direct end users
      DEl_eu = FJL(NJ,L) 	! Total e- demanded by direct end users
      b1 = SHIL(JHELCTRO,L)*GHILM(JHELCTRO,M) 	!Coefficient for the amount of e- needed to generate H2 by electrolysis
      
! This code interacts with the capacity limits in a complex way. A few times through is generally sufficient for convergence.
	IDUM = 1
	FOld = 0
	IMax = 100 	! Maxium number of iterations in this loop
	convtest = 0.01	! Converge to within this fraction
	testval = convtest * 10.
    
	DO While (IDUM .lt. IMax .and. testval .gt. convtest)
      FOld = SUIL(JUH2GEN,L)	! Old share of H2 Fuel cell electric generation
      IF (FJL(JSH2,L) .eq. 0 .and. 1 .eq. 2) THEN	! Default - this is turned off to allow automatic H2 storage
        SHIL(JHELCTRO,L) = 0		! If only elec demand for H2, then set electrolytic H2 to zero
        SumHsh = SUM(SHIL(:,L))		! Redistribute
        SHIL(:,L) = SHIL(:,L)/SumHsh
      END IF
      a1 = SUIL(JUH2GEN,L)*GUILM(JUH2GEN,L,M) 	!Coefficient for the amount of H2 needed to generate e- through fuel cells
      											!This can be changed below, so need to set every time
      ! The equations to solve are:
      ! H2_Dem_Tot = DH_eu  + a1 * El_Dem_Tot (last term: H2 needed to gen e-) 
      ! El_Dem_Tot = DEl_eu + b1 * H2_Dem_Tot (last term: e- needed to gen H2 through electrolysis)
 
! These equations combine:
! El_Dem_Tot = DEl_eu + b1 * (DH_eu  + a1 * El_Dem_Tot) 
		! The term in brakets is the e- needed to gen H2 through electrolysis
		! b1 * DH_eu is amount of elec needed to generate electroly. H2 to supply end-use demands
		! a1*b1 * El_Dem_Tot is amount of elec needed to generate electrolytic H2 used to power electric fuel cells
		!    (a1*b1 = H2 Fuel cell elec share * electrolytic H2 prod share * conversion losses for both)
! This solution fails if a1*b > 1 
		! When a1*b1 gets too large this means most of H2 is generated by electrolysis,
		!     and most of electric generation is by H2 Fuel cells
		! This is a problem. There is a limit to the amount of elec that can be generated through this loop,
		!     since non-fuel cell elec must be used to generate the H2 used in the fuel cells!
        ! This problem is fixed after the routine below. 
        ! Need to try not considering this limit first to see if result is below the limit.
      IF (a1*b1 .lt. 1) THEN
        FJL(JSH2,L) = (DH_eu  + a1 * DEl_eu)/(1-a1*b1)
        FJL(NJ,L)   = (DEl_eu + b1 * DH_eu)/(1-a1*b1)
        H2_electrolGen = FJL(JSH2,L)*SHIL(JHELCTRO,L)		! Amount of H2 generated by Electrolysis      
! If turned on, the code below limits H2_electrolGen <= DH_eu so that all electrolytic H2 is used in end uses 
! But H2 can also be used for elec storage, so turned off by default
        IF (H2_electrolGen .gt. DH_eu .and. 1.eq.2) THEN 	
          S_ELCTRO = DH_eu/H2_electrolGen*SHIL(JHELCTRO,L)
          SumHsh = SUM(SHIL(:,L)) - SHIL(JHELCTRO,L)		
          C = (1.0-S_ELCTRO)/SumHsh
          SHIL(:,L) = SHIL(:,L)*C	! Redistribute so that shares sum to unity
          SHIL(JHELCTRO,L) = S_ELCTRO
          
          b1 = SHIL(JHELCTRO,L)*GHILM(JHELCTRO,M) 	!Coefficient for the amount of e- used to generate H2 by electrolysis
          FJL(JSH2,L) = (DH_eu  + a1 * DEl_eu)/(1.0-a1*b1)
          FJL(NJ,L)   = (DEl_eu + b1 * DH_eu)/(1.0-a1*b1)
        END IF
        
! Re-set shares to take account of fixed Hydro share
        Call RebalanceEShares(NI,ESIL(NI,L)/FJL(NJ,L),SUIL(NI,L)) 	! Re-adust for fixed hydro share
! Now, need to both adjust for capacity limits (which increases fuel cell share and H2 demand)
!               and let capacity limited sources (PV, wind, etc.) contribute to H2
 
        SElectro = b1 * FJL(JSH2,L)/FJL(NJ,L)	! Share of electricity that goes to electrolytic hydrogen. No capacity limits on this.  
        SUIL(:,L) = SUIL_Temp(:) 	! Restore to old elec gen shares, without capacity limits so that capacity limited generation can be used to generate H2
        Call CapCheck(SElectro) 				! Re-adjust elec generation for capacity limits
        
        a1 = SUIL(JUH2GEN,L)*GUILM(JUH2GEN,L,M)
         
       END IF ! a1*b1 < 1 loop
      Hlimit = FJL(NJ,L) - FJL(NJ,L)*SUIL(JUH2GEN,L)*SHIL(JHELCTRO,L)
      HElectroy = b1*FJL(JSH2,L) 
! Check if exceeded Limits (too much H2 by electrolysis.)
! If so need to use alternative solution where:
!       b1 * H2_Dem_Tot = El_Dem_Tot - El_Dem_Tot*SUIL(JUH2GEN,L)*SHIL(JHELCTRO,L)
! or, electricity demanded for electrolytic H2 generation = (total elec demadn - elec produced by fuel cells using electrolytic H2)
      ShF2H2_Old = SUIL(JUH2GEN,L)
      IF ((a1*b1 .gt. 1) .or. (HElectroy .gt. Hlimit)) THEN
!        Write(*,*) "----Exception branch---"
      	  MsgStr = "H2: Too much Electrolytic H2. H2 Fuel cell elec gen adjusted. a1*b1 = "
          Call MCLog(3,MsgStr,M,L,JSH2,a1*b1)
          gbar = 1.0/(1.0+1.0/(GUILM(JUH2GEN,L,M)*GHILM(JHELCTRO,M)))
          DEl_tot = b1*DH_eu + DEl_eu/(1.0-gbar)
          DH2_tot = (DEl_tot-DEl_eu)/b1
          SH2FS_new = (1.0-b1*DH_eu/(b1*DH_eu+b1*DEl_eu/(1.0-gbar))) * (gbar/(GUILM(JUH2GEN,L,M)*b1))
! Re-set H2 share and re-distribute so that shares sum to unity (taking account of fixed Hydro share)
          HydroSh = SUIL(NI,L)
          Call RebalanceEShares(0,HydroSh + SH2FS_new,HydroSh+SUIL(JUH2GEN,L))
          SUIL(JUH2GEN,L) = SH2FS_new
          SUIL(NI,L) = HydroSh
          a1 = SUIL(JUH2GEN,L)*GUILM(JUH2GEN,L,M)
! Limits should be ok now          
        IF ((a1*b1 .gt. 1)) THEN	! But if not, print a warning
      	  MsgStr = "Hydrogen loop: H2 not balanced. Something is seriously wrong. a1*b1 = "
          Call MCLog(1,MsgStr,M,L,JSH2,a1*b1)
        ELSE
          FJL(JSH2,L) = (DH_eu  + a1 * DEl_eu)/(1-a1*b1)
          FJL(NJ,L)   = (DEl_eu + b1 * DH_eu)/(1-a1*b1)
        END IF
! If changed H2 Fuel cell share, need to check capacity limits again
        SElectro = b1 * FJL(JSH2,L)/FJL(NJ,L)      ! Share of electricity that goes to electrolytic hydrogen. No limits on this.    
        Call CapCheck(SElectro) ! Adjust elec generation above capacity limits
      END IF	! Fuel-cell elec gen limit check
      IF (SUIL(JUH2GEN,L) .ne. 0) then
         testval = abs(SUIL(JUH2GEN,L)-FOld)/SUIL(JUH2GEN,L)
      else
         testval = 0
      end if
      IDUM = IDUM + 1
      END DO !IDUM loop    
      
      Call CapCheck(SElectro)	! Afer Fuel cell shares converge, do a final capacity adjustment
      
! Finished:	Total H2 demand is now sum of end-use plus use by elec utilities 
! 			Total elec demand is now sum of end-use plus use by H2 generation sector (through electrolysis) 
! ************************************************************************************

      IF (M .ge. 22 .and. l.lt. 2) then	!Debugging printout (goes in SH2FS_new branch above)
         Write(*,'(I2,5(a,f6.1))') L," Hlimit: ",Hlimit,", HElectroy:",HElectroy
         Write(*,'(5(a,f6.1))') "   H2FS Eqn. gbar: ",gbar,", DEl_tot:",DEl_tot, &
             				 	", DEl_eu:",DEl_eu,", SH2FS_new part:",(1.0+b1)*DH_eu/DEl_tot
         Write(*,'(5(a,f6.1))') "   DEl_tot1: ",DEl_tot,", DEl_tot2:",(DEl_eu + b1 * DH_eu)/(1-a1*b1), &
             				 	", DH2_tot1:",DH2_tot,", DH2_tot2:",(DH_eu  + a1 * DEl_eu)/(1-a1*b1)
         Write(*,'((a,2f8.2))') "   H2 Elec Share (new,old): ",SUIL(JUH2GEN,L),ShF2H2_Old    
      END IF
      IF (M .ge. 82 .and. l.lt. 2) then	!Debugging printout
         Write(*,'(a,7f8.1)') "   H2 End Use Dem, Tot: ",DH_eu, FJL(JSH2,L)         
         Write(*,'(5(a,f6.1))') "   H2 Check: ",b1 * FJL(JSH2,L)," <= ", FJL(NJ,L)-FJL(NJ,L)*SUIL(JUH2GEN,L)*SHIL(JHELCTRO,L)     
      END IF
      IF (M .ge. 89 .and. l.lt. 2) then	!Debugging printout
         Write(*,'(a,7f8.1)') "   H2 End Use Dem, Tot: ",DH_eu, FJL(JSH2,L)         
         Write(*,'(5(a,f6.1))') "   a1,b1, H2 Check: ",a1,",",b1,",",b1 * FJL(JSH2,L)," <= ",  &
                                FJL(NJ,L)-FJL(NJ,L)*SUIL(JUH2GEN,L)*SHIL(JHELCTRO,L)     
      END IF 
!
!CCCCCCC     COMPUTE HYDROGEN DEMAND FOR FUELS
!
	  ESHILM(:,L,M) = 0	! sjs -- 08/02
      TOTH2 = FJL(JSH2,L)
      DO I=1,NNH2 + NH2_New	! sjs -- 08/02
	   ESHILM(I,L,M)=SHIL(I,L)*TOTH2
	END DO
!     Fossil fuel input for H2 production(scrubbed and unscrubbed)
	K=KH2
      DO I=1,NF  
         EFJKL(I,K,L)=ESHILM(I,L,M)*GHILM(I,M) &
               + ESHILM(I+NH2,L,M)*GHILM(I+NH2,M)
      END DO
!     Biomass input for H2 production
 	J=JSBMASS
	IH=JHBMASS
         EFJKL(J,K,L)=ESHILM(IH,L,M)*GHILM(IH,M)
!     Added hydrogen demand for electrolitic inputs. sjs 1/01
!     Otherwise electrolitic H2 comes out of thin air. 
!     Section moved previous to elec demand section so this could be added
 
!     Electricity input for H2 production by electrolysis
      IH=JHELCTRO
      J=NJ
         EFJKL(J,K,L)=ESHILM(IH,L,M)*GHILM(IH,M)           
!     End hydrogen demand for fuels

!
! COMPUTE ELECTRIC UTILITY DEMANDS FOR FOSSIL FUELS
!
      DO 160 I=1,NI
         ESUILM(I,L,M)=SUIL(I,L)*FJL(NJ,L)
  160 CONTINUE
!     Additional subsectors, Biomass to wind+others
      DO I=7,NNU+NStype	!Include advanced "Solar-type" tech's
         ESUILM(I,L,M)=SUIL(I,L)*FJL(NJ,L)
      END DO
      SHARE=0.D0
      E=0.D0 !E is demand for fossil fuels
      EFJKL(NJ,1,L)=0.D0
      DO 170 I=1,NF
         EFJKL(I,1,L)=FJL(NJ,L)*SUIL(I,L)*GUILM(I,L,M)
         EDRIKL(I,1,L)=EFJKL(I,1,L)*GIJ(I)
         EDRIL(I,L)=EDRIKL(I,1,L)
         E=E+EDRIKL(I,1,L)
         SHARE=SHARE+SUIL(I,L)
  170 CONTINUE
!     Additional subsectors
      I=IBMASS
	J=JSBMASS
	IU=JUBMASS
         EFJKL(J,1,L)=FJL(NJ,L)*SUIL(IU,L)*GUILM(IU,L,M)
         EDRIKL(I,1,L)=EFJKL(J,1,L)*GIJ(J)
         EDRIL(I,L)=EDRIKL(I,1,L)
         E=E+EDRIKL(I,1,L)
         SHARE=SHARE+SUIL(IU,L)
!     Hydrogen fuel cells to generate e-
      IU=JUH2GEN
      J=JSH2
         EFJKL(J,1,L)=FJL(NJ,L)*SUIL(IU,L)*GUILM(IU,L,M)   
      I=INCOAL
	IU=JUCSCRUB  !scrub subsector, add to regular coal
         EFJKL(I,1,L)=FJL(NJ,L)*SUIL(IU,L)*GUILM(IU,L,M) + EFJKL(I,1,L)
         EDRSAVE=EDRIKL(I,1,L)  !Save coal value
         EDRIKL(I,1,L)=EFJKL(I,1,L)*GIJ(I)
         EDRIL(I,L)=EDRIKL(I,1,L)
         E=E+EDRIKL(I,1,L) - EDRSAVE  !Don't double count coal
         SHARE=SHARE+SUIL(IU,L)
      I=INOIL
	IU=JUOSCRUB  !scrub subsector, add to regular oil
         EFJKL(I,1,L)=FJL(NJ,L)*SUIL(IU,L)*GUILM(IU,L,M) + EFJKL(I,1,L)
         EDRSAVE=EDRIKL(I,1,L)  !Save Oil value
         EDRIKL(I,1,L)=EFJKL(I,1,L)*GIJ(I)
         EDRIL(I,L)=EDRIKL(I,1,L)
         E=E+EDRIKL(I,1,L) - EDRSAVE  !Don't double count oil
         SHARE=SHARE+SUIL(IU,L)
      I=INGAS
	IU=JUGSCRUB  !scrub subsector, add to regular gas
         EFJKL(I,1,L)=FJL(NJ,L)*SUIL(IU,L)*GUILM(IU,L,M) + EFJKL(I,1,L)
         EDRSAVE=EDRIKL(I,1,L)  !Save Gas value
         EDRIKL(I,1,L)=EFJKL(I,1,L)*GIJ(I)
         EDRIL(I,L)=EDRIKL(I,1,L)
         E=E+EDRIKL(I,1,L) - EDRSAVE  !Don't double count gas
         SHARE=SHARE+SUIL(IU,L)

!              +---------------------------------------+
!              |   COMPUTE PRIMARY EQUIVALENCE RATIO   |
!              |     FOR FOSSIL FUELS IN ELECTCITY     |
!              +---------------------------------------+
      IF (ESIL(NI,L)-FJL(NJ,L))   175, 176, 176
  175 RATIO=E/(SHARE*FJL(NJ,L))
      GO TO 177
  176 RATIO=1.D0						! Old version. Causes discontinuity in energy statistics. Replace with last foss efficiency. sjs-01/01
  	  Ratio = SUM(GUILM(1:3,L,M))/3.	! New version. Make equal to average fossil efficiency instead
  177 CONTINUE
      IF (SHARE .eq. 0)  Ratio = SUM(GUILM(1:3,L,M))/3.
!
! COMPUTE PRIMARY EQUIVALENCE FOR RENEWABLES
!
      DO 180 I=NF+1,NI
         ESRILM(I,L,M)=FJL(NJ,L)*SUIL(I,L)*RATIO
         ESIL(I,L)=ESRILM(I,L,M)
         EDRIKL(I,1,L)=ESRILM(I,L,M)
         EDRIL(I,L)=EDRIKL(I,1,L)
         EDIKL(I,1,L)=EDRIKL(I,1,L)
         EDIL(I,L)=EDRIL(I,L)
  180 CONTINUE
!     Compute primary energy equivalent for fusion
      I=JUFUSION
	ESRILM(I,L,M)=FJL(NJ,L)*SUIL(I,L)*RATIO
      ESIL(I,L)=ESRILM(I,L,M)
      
!     Compute primary energy equivalent for wind
      I=JUWIND
	ESRILM(I,L,M)=FJL(NJ,L)*SUIL(I,L)*RATIO
      ESIL(I,L)=ESRILM(I,L,M)
!    If present, compute primary energy equivalent for SWStor 
      IF (NStype .ge. 1) THEN
        I=JUWIND+1
	    ESRILM(I,L,M)=FJL(NJ,L)*SUIL(I,L)*RATIO
        ESIL(I,L)=ESRILM(I,L,M)
      End if 
!    If present, compute primary energy equivalent for next technology 
      IF (NStype .ge. 2) THEN
        I=JUWIND+2
	    ESRILM(I,L,M)=FJL(NJ,L)*SUIL(I,L)*RATIO
        ESIL(I,L)=ESRILM(I,L,M)
      End if 
!  +-------------------------------------------------------------------+
!  | COMPUTE PRIMARY ENERGY DEMAND REQUIREMENTS FOR HYDROGEN PRODUCTION|
!  +-------------------------------------------------------------------+
	K=KH2
!     Total fossil fuel demand (both scrubbed and unscrubbed)
	DO I=1,NF 
	   EDRIKL(I,K,L)=EFJKL(I,K,L)*GIJ(I)
         EDRIL(I,L)=EDRIL(I,L)+EDRIKL(I,K,L)   
	END DO
      
!     Biomass
      I=IBMASS
	J=JSBMASS
      EDRIKL(I,K,L)=EFJKL(J,K,L)*GIJ(J)
      EDRIL(I,L)=EDRIL(I,L)+EDRIKL(I,K,L)
               
!
!
!  +------------------------------------------------------------------+
!  | COMPUTE REFINABLE ENERGY DEMAND REQUIREMENTS FOR END-USE SECTORS |
!  +------------------------------------------------------------------+
!
      DO 220 K=1,NK
         KK=K+1
         DO 200 I=1,NF
            J=I
            EFJKL(J,KK,L)=FJKL(J,K,L)
            EDRIKL(I,KK,L)=EFJKL(I,KK,L)*GIJ(I)
            EDRIL(I,L)=EDRIL(I,L)+EDRIKL(I,KK,L)
  200   CONTINUE
!     Biomass
	   I=IBMASS
	   J=JSBMASS
         EFJKL(J,KK,L)=FJKL(J,K,L)
         EDRIKL(I,KK,L)=EFJKL(J,KK,L)*GIJ(J)
         EDRIL(I,L)=EDRIL(I,L)+EDRIKL(I,KK,L)
!     put hydrogen into efjkl array for sector totalling later
	   J=JSH2
         EFJKL(J,KK,L)=FJKL(J,K,L)

! ASSIGN ELECTRICITY
!
         EFJKL(NJ,KK,L)=FJKL(NJ,K,L)
!
! ALLOCATE ZERO PRIMARY ELECTRICITY TO DIRECT END USE
!
         DO 210 I=NF+1,NI
            EDIKL(I,KK,L)=0.D0
            EDRIKL(I,KK,L)=0.D0
  210    CONTINUE
  220 CONTINUE
!  230 CONTINUE
!

      RETURN
      END

	Subroutine CapCheck(SElectro) 
! Check for elec generation above capacity limits for direct generation of electricity
! SElectro is fraction of electrolytic hydrogen in total electric end-use
!          No limit on this
!
! This routine is called twice. Once in PS.for, which sets consumer price.
!
! Is called again in dd.for to adjust for electrolytic hydrogen production
! 	 This second adjustment does not reflect back on electricity price, 
!    which should be interpreted as the consumer price of electricity
!
! Note that the capacity limits set in variable ECapLim apply to end-use electricity only, not
! to the total electricity generated, which includes electricity used to make H2. So need to adjust
! for this. This is done via variable ScaleFact
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Real*8 ShSave(NUP)
      SHSave(:) = SUIL(:,L)
      
      Stot = Sum(SUIL(:,L))
      Hyd_Sh = SUIL(NI,L)
      IF (abs(Stot -1) .gt. 0.001) THEN		!Temp code, adjust if not equal to one, not counting hydro share
         Tot1 = Stot - Hyd_Sh
!         SUIL(:,L) = (1-Hyd_Sh)*SUIL(:,L)/Tot1
      END IF
      
      DO IDUM = 1,3 	! Loop a few times in case meet up with multiple limits
       SMaxed = 0.0	! Share of capacity that reached max value
       SOver = 0.0  ! Capacity that has to be re-distributed
       
       DO I=1,NNU+NStype	! First find out how much is over
        IF (ECapLim(I) .eq. 0) ECapLim(I) = 1	! Check: If this wasn't set, set to 1
        IF (SUIL(I,L) .gt. ECapLim(I)*(1.-SElectro)) &
              SOver = SOver + (SUIL(I,L) - ECapLim(I)*(1.-SElectro))
       END DO 
       
       SH2_distr = 0
       IF (SOver .lt. SElectro) SH2_distr = SElectro-SOver	! This is the amount of Elec for H2 that the over capacity tech's can't supply
! Set factor that converts end-use shares to total elec shares
       ScaleFact = (1.-SElectro)/(1. - SH2_distr)
       SOver = 0.0
       
! Now do for real, using appropriate scale factor
       
       DO I=1,NNU+NStype	! First find out how much is over
        IF (SUIL(I,L) .gt. ECapLim(I)*ScaleFact) THEN
             SOver = SOver + (SUIL(I,L) - ECapLim(I)*ScaleFact)
             SMaxed = SMaxed + ECapLim(I)*ScaleFact
        END IF
       END DO 
       
! Check, if hydro is supplying all elec, then skip adjustment
       IF (Hyd_Sh-SOver-SMaxed .lt. 0.999) THEN
          EScale = 1./(1.-Hyd_Sh-SOver-SMaxed) ! Denominator is current sum of modes not over capacity
       ELSE
          SOver = SElectro	! skip adjustment. 
       END IF
       
       S2 = SMaxed + (SElectro-SH2_distr)	! Since can always make H2
       SOver = SOver - (SElectro-SH2_distr)
       
       E_save  = EScale
       IF (SOver .gt. 0.001) THEN	! Now actually adjust if need be
        EScale = (1.-Hyd_Sh-S2)*EScale
         
         DO I=1,NNU+NStype	!Include advanced "Solar-type" tech's
           IF (SUIL(I,L) .gt. ECapLim(I)*ScaleFact) THEN
              SUIL(I,L) = ECapLim(I)*S2/SMaxed*ScaleFact
           ELSE
              SUIL(I,L) = SUIL(I,L)*EScale
           END IF
         END DO ! I loop
         SUIL(NI,L) = Hyd_Sh
       ELSE
        
        SUIL(NI,L) = Hyd_Sh	! Reset hydro share
        Stot = Sum(SUIL(:,L))	! Should equal 1    
        IF (abs(Stot -1) .gt. 0.01) THEN	! significant error, write a warning
	      MsgStr = "ECapLim-Elec shares off by:"
          Call MCLog(1,MsgStr,M,L,MODL,Stot -1)
!          write (*,'(a,10(f8.3,", "))') "Shares: ",SOver,SMaxed,E_save,SElectro,S2,EScale
!           write (*,'(a,2(I2,","),": ",16(f6.3,", "))') " Before: ",L,M,(SHSave(I), I=1,nup)
!           write (*,'(a,2(I2,","),": ",16(f6.3,", "))') " After: ",L,M,(SUIL(I,L), I=1,nup)
          SUIL(:,L) = SUIL(:,L)/Stot
          SUIL(NI,L) = Hyd_Sh
        ELSE ! Small error, correct
          SUIL(:,L) = SUIL(:,L)/Stot
          SUIL(NI,L) = Hyd_Sh
        END IF
        
        RETURN	! Stop once fixed 
       END IF	! SOver > 0
       
      END DO ! IDUM 
      SUIL(NI,L) = Hyd_Sh	! Reset hydro share
      
        Stot = Sum(SUIL(:,L))	! Should equal 1
        IF (abs(Stot -1) .gt. 0.01) THEN	! significant error,  a warning
	      MsgStr = "ECapLim-Elec shares off by (2):"
          Call MCLog(1,MsgStr,M,L,MODL,Stot -1)
!          write (*,'(a,10(f8.3,", "))') "Shares: ",SOver,SMaxed,E_save,SElectro,S2,EScale
!           write (*,'(a,2(I2,","),": ",16(f6.3,", "))') " Before: ",L,M,(SHSave(I), I=1,nup)
!           write (*,'(a,2(I2,","),": ",16(f6.3,", "))') " After: ",L,M,(SUIL(I,L), I=1,nup)
           
          SUIL(:,L) = SUIL(:,L)/Stot
          SUIL(NI,L) = Hyd_Sh
          STOP
        ELSE ! Small error, correct
          SUIL(:,L) = SUIL(:,L)/Stot
          SUIL(NI,L) = Hyd_Sh
        END IF
      RETURN
      END

	Subroutine RebalanceEShares(Index,NewFixedShare,OldFixedShare) 
!
! Routine re-adjusts electric generation shares given one share that is fixed.
! This is done for Hydro several times, but also needs to be done for others occasionally
! Re-scales shares, taking out the FixedShare first
! If I is in bounds, then sets that share to FixedShare
! --- If I = 0, then doing something else -- user must fix other shares after call
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Real*8 NewFixedShare,OldFixedShare
      
        IF (NewFixedShare .gt. 0) THEN
          ShareError = abs(1.0 - SUM(SUIL(:,L)))
          
          IF (ShareError .gt. 0.01) Write(*,'(a,f8.3,";",17f8.3)') "   SUIL-Error1: ",ShareError,(SUIL(I,L),I=1,NNU+NStype)	! Debugging code
          SumEsh = SUM(SUIL(:,L)) - OldFixedShare
          C = (1.0-NewFixedShare)/SumEsh
          IF (SumEsh .ne. 0.0) Then
             SUIL(:,L) = SUIL(:,L)*C
             IF (Index .ne. 0) SUIL(Index,L) = NewFixedShare
          END IF
         END IF
        
      RETURN
      END
      
     
