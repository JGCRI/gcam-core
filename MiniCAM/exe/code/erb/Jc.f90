!***********************************************************************
!
      SUBROUTINE CO2
!
!*************************************************************************
!
!                --  THE CARBON EMISSION SUBROUTINE  --
!
! THIS MODULE CALCULATES GROSS CARBON EMISSIONS TO THE ATMOSPHERE IN A
! GIVEN PERIOD FROM ENERGY PRODUCTION AND CONSUMPTION DATA
!
!   INTEGER INPUTS:  NF, NL
!   REAL INPUTS:     COI, EFJL, ESIL, ESIL2M,ESRILM, SBURNL, SFEDIL,
!                    SFLRL, SHALE
!
!   INTERNAL INTEGERS:  IOIL, IGAS, ICOAL, ISHL, ISYN, IFLR, IBIO
!                       IIOIL, IISOIL, IISYNO, IICOAL, IISYNG, IIGAS
!
!   REAL OUTPUTS:    CO2IL, CO2IIL, CO2IM, CO2IIM, CO2LM, CO2M
!
!   SUBROUTINES CALLED:  NONE
!
!   CODED BY
!       JAE EDMONDS                    LATEST REVISION:
!       1 JANUARY 1982                  1 JANUARY 1982
!                                       31 MARCH 1989-(CO2 BY DEMAND)
!                                       22 AUGUST 89-TO CORRECT SYNFUEL
!                                         OVERFLOW IN EMISSIONS REPORT
!                                       9 APRIL 91-CORRECTION TO FEEDSTOCK
!                                        REDUCTION IN SYNFUELS PROCESSES
!     Aug 96, added carbon free backstop
!     4/97  New accounting for biomass split from coal
!**************************************************************************
!
!
! COMMON BLOCKS
!
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!  --  LOCAL VARIABLES
!
      REAL*8 A,B(NLPMax),C,D,CO2IL(11,NLPMax),CO2IIL(11,NLPMax),CO2DL(NLPMax),CO2D
!
!
! -- INITIALIZE FUEL DESIGNATORS, ZERO CO2 TOTALS, AND SET IUNIT VALUE
!
      IUNIT=IOUNIT
      IOIL=1
      IGAS=2
      ICOAL=3
      ICL=4
      ICG=5
      ISHL=6
      IBIO=7
      IFLR=8
!
      NCAT=10
      IIOIL=1
      IISOIL=2
      IICSYN=3
      IICOAL=4
      IIBSYN=5
      IIGAS=6
	IICSCRUB=7
	IIBMASS=8
      IIFLR=9
      IIGSYN=10 !synfuel liquid production from n. gas

!     INITIALIZE
      DO K=1,6
	   DO J=1,6
            CO2JKLM(J,K,NNLP,M)=0.0
         END DO
	   CO2KLM(K,NNLP,M)=0.0
      END DO
	CO2LMT(NNLP,M)=0.0
!
!
      CO2D =0.D0
      CO2M(M)=0.D0
      A=0.D0
      C=0.D0
      D=0.D0
      DO 10 I=1,NCAT
         CO2IM(I,M)=0.D0
         IF(I.LE.6)CO2IIM(I,M)=0.D0
   10 CONTINUE
!
      T=(M-1)*NJUMP
!
!      FIND WEIGHTED AVERAGE VALUE FOR (1-SFEDIL)=A
      DO 50 L=1,NL
          B(L)=EDRIL(IOIL,L)-ESIL2M(IOIL,L,M)-SYNILM(IOIL,L,M)
          C=C+((1.D0-SFEDIL(IOIL,L))*B(L))
          D=D+B(L)
  50  CONTINUE
      A=C/D

      DO I2=1,4
	   CARBSEQM(I2,M)=0.0
         CARBFUEL(I2,NL+1,M)=0.0
	END DO

      DO 100 L=1,NL

!  --  INTERPOLATE FLARED GAS PARAMETER VALUES
!
         SB=XNTERP(SBURNL(L,1),SBURNL(L,2),SBURNL(L,3),T)
!         SH=XNTERP(SHALE(L,1),SHALE(L,2),SHALE(L,3),T)	! This var no longer used
!
!            -------------------------------------
!            --  COMPUTE REGIONAL EMISSIONS BY  --
!            --          END-USE SOURCE         --
!            -------------------------------------

! ************************************************************************
! Emissions are of each end-use fuel, evaluated at the conventional fossil coefficient
! Conversions are accounted for separately
! ************************************************************************

!**** CONVENTIONAL OIL CONSUMPTION
!     5-14-96 Base oil (liquids) emissions on refinable
!     Subtract shale oil and report separately

      CO2IL(IIOIL,L) = (1.D0-SFEDIL(IOIL,L))*COI(IOIL)*EDRIL(IOIL,L)
      CO2ILM(IIOIL,L,M) = CO2IL(IIOIL,L)
!     Net emissions by fuel
      CARBFUEL(INOIL,L,M) = CO2IL(IIOIL,L) 


!**** SHALE OIL PRODUCTION
! Emissions released in production.
! 	-- Assumed to be difference in shale and oil coefficients. sjs -- 04/02
!   -- Carbon in the resulting oil ends up wherever the oil is used
      CO2IL(IISOIL,L) = ESIL2M(IOIL,L,M)*(COI(ISHL)-COI(IOIL))*(1.D0-SFEDIL(IOIL,L))
      CO2ILM(IISOIL,L,M) = CO2IL(IISOIL,L)
!     Net emissions by fuel
      CARBFUEL(INOIL,L,M) = CARBFUEL(INOIL,L,M) + CO2IL(IISOIL,L)

!**** NATURAL GAS LIQUIFACTION PROCESS
!     Net Emissions released from Syngas process
      CO2IL(IIGSYN,L) = SYNINPUT(IGAS,L,M)*COI(IGAS) &
            - SYNFUEL(IGAS,IOIL,L,M)*COI(IOIL)*(1.D0-SFEDIL(IOIL,L))
      CO2ILM(IIGSYN,L,M) = CO2IL(IIGSYN,L)

!**** CONVENTIONAL GAS CONSUMPTION
!     5-14-96 Base gas emissions on refinable, but leave credit for
!     syngas back to coal emissions 
      RefGasCons = EDRIL(IGAS,L) - SYNINPUT(IGAS,L,M)	! Direct consumption of natural gas. Correction for new synfuel formulation. sjs - 08/01
      CO2IL(IIGAS,L) = (1.D0-SFEDIL(IGAS,L))*COI(IGAS)*RefGasCons
      CO2ILM(IIGAS,L,M) = CO2IL(IIGAS,L)
!     Total CO2 emissions from natural gas
!     direct consumption plus emissions from syn-liquid process
      CARBFUEL(INGAS,L,M) = CO2IL(IIGAS,L)+CO2IL(IIGSYN,L)


!**** COAL LIQUIFACTION AND GASIFICATION PROCESS
!     Net Emissions released from Syn Coal process
!       --- subtracting any emissions sequestered
      CO2IL(IICSYN,L) = SYNINPUT(ICOAL,L,M)*COI(ICOAL) &
            - SYNFUEL(ICOAL,IOIL,L,M)*COI(IOIL)*(1.D0-SFEDIL(IOIL,L)) &
            - SYNFUEL(ICOAL,IGAS,L,M)*COI(IGAS)*(1.D0-SFEDIL(IGAS,L))
      CO2IL(IICSYN,L) = CO2IL(IICSYN,L)*(1.0-REMFRACP(1,L))
      CO2ILM(IICSYN,L,M) = CO2IL(IICSYN,L)
      CARBSEQ(1,L,M) = CO2IL(IICSYN,L)*REMFRACP(1,L)
!     Net emissions by fuel
      CARBFUEL(INCOAL,L,M) = CO2IL(IICSYN,L)

!**** CONVENTIONAL COAL CONSUMPTION
!     no more biomass in there 
      RefCoalCons = EDRIL(ICOAL,L) - SYNINPUT(ICOAL,L,M)	! Direct consumption of coal. Correction for new synfuel formulation. sjs - 08/01
      CO2IL(IICOAL,L) = (1.D0-SFEDIL(ICOAL,L))*COI(ICOAL)*RefCoalCons
      CO2ILM(IICOAL,L,M) = CO2IL(IICOAL,L)
!     Net emissions by fuel
      CARBFUEL(INCOAL,L,M) = CARBFUEL(INCOAL,L,M) + CO2IL(IICOAL,L)


!**** BIOMASS SYNFUELS PROCESS
!     5-14-96 credit carbon in synliq and syngas production
!     from the total emissions from synfuels.
      CO2IL(IIBSYN,L) = SYNINPUT(INBMASS,L,M)*COI(IBIO) &
            -SYNFUEL(INBMASS,IOIL,L,M)*COI(IOIL)*(1.D0-SFEDIL(IOIL,L)) &
            -SYNFUEL(INBMASS,IGAS,L,M)*COI(IGAS)*(1.D0-SFEDIL(IGAS,L)) 
      CO2ILM(IIBSYN,L,M) = CO2IL(IIBSYN,L)
!     Net emissions by fuel
      CARBFUEL(INBMASS,L,M) = CO2IL(IIBSYN,L)

!**** STRAIGHT BIOMASS BURNING (IF NOT ASSUMED ZERO NET EMISSIONS)
      RefBioCons = EDRIL(IBMASS,L) - SYNINPUT(INBMASS,L,M)	! Direct consumption of natural gas. Correction for new synfuel formulation. sjs - 08/01
      CO2IL(IIBMASS,L) = RefBioCons*COI(IBIO)
      CO2ILM(IIBMASS,L,M) = CO2IL(IIBMASS,L)

!     Slot not currently used
      CO2ILM(7,L,M) = CO2IL(IICSCRUB,L)

!**** VENTING AND FLARING
      CO2IL(IIFLR,L) = SB*SFLRL(L)/(1.D0-SFLRL(L))*(ESIL(IGAS,L) &
                      +0.0001)*COI(IGAS)
     
!     Add on separate exogenous flaring emissions term
      CO2IL(IIFLR,L) = CO2IL(IIFLR,L) + EXOFLARE(L,M)           
      CO2ILM(IIFLR,L,M) = CO2IL(IIFLR,L)


!*********************************************************
!     EMISSIONS REMOVED BY UTILITIES SCRUBBER TECHNOLOGY  

!**** COAL POWER CARBON CAPTURE
	I=JUCSCRUB
	J=INCOAL
	CSCRUB = ESUILM(I,L,M)*GUILM(I,L,M)*GIJ(J)
	CARBSEQ(2,L,M) = COI(ICOAL)*CSCRUB*REMFRAC(2,L)
!     Net emissions by fuel
      CARBFUEL(INCOAL,L,M) = CARBFUEL(INCOAL,L,M) -  &
                         COI(ICOAL)*CSCRUB*REMFRAC(2,L)

!**** OIL POWER CARBON CAPTURE
	I=JUOSCRUB
	J=INOIL
	CSCRUB = ESUILM(I,L,M)*GUILM(I,L,M)*GIJ(J)
	CARBSEQ(2,L,M) = CARBSEQ(2,L,M)+COI(IOIL)*CSCRUB*REMFRAC(2,L)
!     Net emissions by fuel
      CARBFUEL(INOIL,L,M) = CARBFUEL(INOIL,L,M) -  &
                         COI(IOIL)*CSCRUB*REMFRAC(2,L)

!**** GAS POWER CARBON CAPTURE
	I=JUGSCRUB
	J=INGAS
	CSCRUB = ESUILM(I,L,M)*GUILM(I,L,M)*GIJ(J)
	CARBSEQ(2,L,M) = CARBSEQ(2,L,M)+COI(IGAS)*CSCRUB*REMFRAC(2,L)
!     Net emissions by fuel
      CARBFUEL(INGAS,L,M) = CARBFUEL(INGAS,L,M) -  &
                         COI(IGAS)*CSCRUB*REMFRAC(2,L)


!**** EMISSIONS REMOVED BY HYDROGEN PRODUCTION SCRUBBER TECHNOLOGY
	CARBSEQ(3,L,M)=0.0  !H2 conversion scrubbing
	DO J=INOIL,INCOAL
	   I=J+NH2
	   CSCRUB = ESHILM(I,L,M)*GHILM(I,M)*GIJ(J)
	   CARBSEQ(3,L,M) = CARBSEQ(3,L,M)+COI(J)*CSCRUB*REMFRAC(2,L)
!     Net emissions by fuel
         CARBFUEL(J,L,M) = CARBFUEL(J,L,M) -  &
                         COI(J)*CSCRUB*REMFRAC(2,L)
      END DO

!**** Emissions removed by policy that forces conversion of all fossil fuels
	DO J = INOIL,INCOAL
         CSCRUB = FJL(J,L)*H2FRAC(L)
	   CARBSEQ(3,L,M) = CARBSEQ(3,L,M)+COI(J)*CSCRUB*REMFRAC(2,L)
         CARBFUEL(J,L,M) = CARBFUEL(J,L,M) -  &
                         COI(J)*CSCRUB*REMFRAC(2,L)
	END DO

!     ------------------------------------------------
!     --  COMPUTE CO2 EMISSIONS BY SUPPLY ACTIVITY  --
!     ------------------------------------------------

      CO2IIL(IIOIL,L)  = ESIL1M(IOIL,L,M)*COI(IOIL)*A
      CO2IILM(1,L,M) = CO2IIL(IIOIL,L)
      CO2IIL(IISOIL,L) = ESIL2M(IOIL,L,M)*COI(ISHL)*(1.D0-SFEDIL(IOIL,L))
                        
      CO2IILM(2,L,M) = CO2IIL(IISOIL,L)
      CO2IIL(IICSYN,L) = CO2IL(IICSYN,L)
      CO2IILM(3,L,M) = CO2IIL(IICSYN,L)
      CO2IIL(IICOAL,L) = ESIL1M(ICOAL,L,M)*COI(ICOAL)*(1.D0-SFEDIL(ICOAL,L))
      
      CO2IILM(4,L,M) = CO2IIL(IICOAL,L)
      CO2IIL(IIBSYN,L) = CO2IL(IIBSYN,L)
      CO2IILM(5,L,M) = CO2IIL(IIBSYN,L)
      CO2IIL(IIGAS,L) = ESIL1M(IGAS,L,M)*COI(IGAS)*(1.D0-SFEDIL(IGAS,L)) + CO2IL(IIFLR,L)
      CO2IILM(6,L,M) = CO2IIL(IIGAS,L)

!     -------------------------------------
!     --  TOTAL REGIONAL CO2 PRODUCTION  --
!     -------------------------------------

      CO2LM(L,M)=0.D0
      CO2DL(L) =0.D0
      DO I=1,NCAT
         CO2DL(L) =CO2DL(L)+CO2IL(I,L)
         CO2DLM(L,M)=CO2DL(L)               
         CO2IM(I,M) =CO2IM(I,M)+CO2IL(I,L)
      END DO
!     total sequestered carbon and subtract off
	SEQTOT=0.0
      DO I2=1,3
	   SEQTOT = SEQTOT + CARBSEQ(I2,L,M)
	END DO
	CO2DLM(L,M) = CO2DLM(L,M) - SEQTOT

	DO I=1,NI
         CO2LM(L,M) = CO2LM(L,M)+CO2IIL(I,L)
         CO2IIM(I,M) = CO2IIM(I,M)+CO2IIL(I,L)
	END DO
         
      CO2D = CO2D+CO2DL(L)
!     subtract off sequestered
      CO2D = CO2D - SEQTOT
      CO2DM(M)= CO2D
      CO2M(M) = CO2M(M)+CO2LM(L,M)

      DO I2=1,3
	   CARBSEQM(I2,M) = CARBSEQM(I2,M) + CARBSEQ(I2,L,M)
	END DO

!     Add regional net emissions by fuel to global sum
      DO IN=1,INBMASS
	   CARBFUEL(IN,NL+1,M) = CARBFUEL(IN,NL+1,M) + CARBFUEL(IN,L,M)
	END DO


  100 CONTINUE
  
  
!     Check Backstop considerations.

!     Set gross emissions (before backstop)
      DO L=1,NL
         CO2DLMG(L,M) = CO2DLM(L,M)
      END DO
      CO2MG(M)=CO2DM(M)
      
!     Adjust emissions for backstop removal, set to group target

      IF (CFBSTOP(M).NE.0.D0) THEN
         DO L=1,NL 
!        Test to see if region's tax is at backstop
            IF (TAXRLM(L,M) .EQ. CFBSTOP(M)) THEN
!           Check to see that gross emissions are greater than the target
               IF (CO2DLMG(L,M) .GE. CEMTARGS(L,M)) THEN
!              Scale region back to target and adjust global total
                  CO2DLM(L,M)=CEMTARGS(L,M)
	            CO2DM(M) = CO2DM(M) - (CO2DLMG(L,M) - CO2DLM(L,M))
	            IPFIX(INCARB,L,M)=1   !Fix price
	         ELSE   
!              Unfix price (only if model not run with fixed tax)
                  IF(NTAXMODE(L).EQ.4) THEN
                     IPFIX(INCARB,L,M)=0
	               p(incarb,l,m)=cfbstop(m)*0.99
	            END IF
	         END IF
            END IF
         END DO
      END IF            
!
      RETURN
      END
