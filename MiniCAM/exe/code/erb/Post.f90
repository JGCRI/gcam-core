!***********************************************************************
!
      SUBROUTINE  POST
!
!***********************************************************************
!
!             -- THE REPORT VARIABLE ASSEMBLY MODULE --
!
! THIS MODULE GENERATES TOTALS AND SUBTOTALS OF ENERGY CONSUMPTION
! BY VARIOUS CATEGORIES OF GLOBAL CONSUMERS.
!
! VERSION:  A.31.07.84
!
! INTEGER INPUTS: NI, NJ, NKKL, NKKMAX, NL
!
! REAL INPUTS:    EDIKL, EDJKL, EDRIKL, EFJKL, ESIL, EXIL
!
! REAL OUTPUTS:   EDKL, EDIL, EDI, EDL, ED, EDRIL, EFKL, EFJL, EFJ,
!                 EFL, ES, ESI, ESL, ESR1M, ESR2M, EXI, EXL, EX
!
! SUBROUTINES CALLED: NONE
!
! CODED BY:
!   JAE EDMONDS                        LATEST REVISION:
!   1 JANUARY 1982                      31 JULY 1984
!                                       BY:  JAE EDMONDS
!                                       *POPULATION NUMBERS ARE TAKEN
!                                        DIRECTLY FORM THE MODEL NOW
!                                        AND NOT CONVERTED FROM AN INDEX
!                                       4 SEPT 88-TO INCLUDE COMMON
!                                         16 JAN 90: TO PUT IF EDILM,ESILM,
!                                         EFJLM,EDRILM,EXILM,AND EDRIKLM FOR
!                                         USE BY PRTLOTUS
!                                        31 MAY 1994  M. WISE
!                                          ADD COMPUTATION FOR REGIONAL
!                                          CONSUMPTION OF PRIMARY BIOMASS
! **********************************************************************
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	REAL*8 EXPRTONLY(NIP,NMP)
!

!   +--------------------------------+
!   |   INITIALIZE VARIABLE VALUES   |
!   +--------------------------------+
!
      NLL=NL+1
!
!   +-------------------------------------+
!   |   COMPUTE ENERGY TRADE STATISTICS   |
!   +-------------------------------------+
      DO 20 L=1,NL

         EXLM(L,M)=0.D0
         GNPLM(L,M)=GNP(L,M)*GNPBL(L)/(1.0D3)
   20 CONTINUE
      DO 50 I=1,NNI !primary energy plus biomass
         EXIM(I,M)=0.D0
	   EXPRTONLY(I,M)=0.D0
         DO 40 L=1,NL
            EXIL(I,L)=EDRIL(I,L)-ESRILM(I,L,M)
            EXIM(I,M)=EXIM(I,M)+EXIL(I,L)
	      IF(EXIL(I,L).LE.0.0)  &
                    EXPRTONLY(I,M) = EXPRTONLY(I,M)-EXIL(I,L)
            EXILM(I,L,M)=EXIL(I,L)
            EXLM(L,M)=EXLM(L,M)+EXIL(I,L)
   40    CONTINUE
   50 CONTINUE
!
      EXM(M)=0.D0
      DO 109 I=1,NF
         EXM(M)=EXM(M)+EXIM(I,M)
  109 CONTINUE

!     Biomass trade
         EXM(M)=EXM(M)+EXIM(IBMASS,M)
      
!
!   +----------------------------------------------+
!   |   COMPUTE PRIMARY ENERGY DEMAND STATISTICS   |
!   +----------------------------------------------+
      DO 110 I2=1,NF+1

!     Add index for biomass
	   IF(I2.LE.NF) THEN
	      I=I2
	   ELSE
	      I=IBMASS
	   END IF

         TEMP=0.D0
         DO 80 L=1,NL
            IF (EXIL(I,L).GT.0.D0)   GO TO 80
               IF(ESRILM(I,L,M).GT.0.0) THEN
                  FACT=ESIL(I,L)/ESRILM(I,L,M)
	         ELSE
	            FACT=1.0
	         END IF
            EDIL(I,L)=FACT*EDRIL(I,L)
            NKK=NKKL(L)
            DO 70 K=1,NKK+1  !Include hydrogen production input demand
               EDIKL(I,K,L)=FACT*EDRIKL(I,K,L)
   70       CONTINUE
            IF(EXPRTONLY(I,M).GT.0) THEN
               TEMP=TEMP-FACT*EXIL(I,L)/EXPRTONLY(I,M)
	      END IF
   80    CONTINUE
!
         DO 100 L=1,NL
            IF (EXIL(I,L).LE.0.D0)   GO TO 100
            EDIL(I,L)=ESIL(I,L)+TEMP*EXIL(I,L)
            IF(EDRIL(I,L).GT.0.0) THEN
               FACT=EDIL(I,L)/EDRIL(I,L)
	      ELSE
	         FACT=1.0
	      END IF
            NKK=NKKL(L)
            DO 90 K=1,NKK +1 !Include hydrogen production input demand
               EDIKL(I,K,L)=FACT*EDRIKL(I,K,L)
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE

   

!   +-------------------------------------------+
!   |   COMPUTE SYNFUEL PRODUCTION STATISTICS   |
!   +-------------------------------------------+

      SYNM(M)=0.D0
!     Global total of fuel input for synfuel production
      SYNINPUT(:,NL+1,M)=0.0
      
      SYNIM(:,M)=0.D0
	  SYNJIM(:,:,M)=0.0
      SYNJIM(3,I,M)=0.0
      SYNJIM(4,I,M)=0.0

      DO 130 L=1,NL
         SYNLM(L,M)=0.D0

         SYNILM(1,L,M) = SUM(SYNFUEL(:,1,L,M))
         SYNILM(2,L,M) = SUM(SYNFUEL(:,2,L,M))
         SYNILM(3,L,M) =  - SYNINPUT(3,L,M)
         SYNILM(IBMASS,L,M) =  - SYNINPUT(INBMASS,L,M)

! Old Ver. This formula gives:
! For coal and biomass --> negative synfuel inputs
! For gas and oil --> synfuel produced
!         DO 120 I=1,NNI !NNI=7
!            SYNILM(I,L,M)=ESRILM(I,L,M)-ESIL(I,L)

         DO 119 I=1,NSYN !NSYN=2
            SYNLM(L,M)=SYNLM(L,M)+SYNILM(I,L,M)
            SYNIM(I,M)=SYNIM(I,M)+SYNILM(I,L,M)
!           Global total of liquid and gas synfuel from n.gas (2), coal(3) & biomass(4)
            SYNJIM(2,I,M)=SYNJIM(2,I,M)+SYNFUEL(2,I,L,M)
            SYNJIM(3,I,M)=SYNJIM(3,I,M)+SYNFUEL(3,I,L,M)
            SYNJIM(4,I,M)=SYNJIM(4,I,M)+SYNFUEL(4,I,L,M)
  119    CONTINUE
         SYNIM(INCOAL,M)=SYNIM(INCOAL,M)+SYNILM(INCOAL,L,M)
         SYNIM(IBMASS,M)=SYNIM(IBMASS,M)+SYNILM(IBMASS,L,M)
         
         SYNINPUT(:,NL+1,M)=SYNINPUT(:,NL+1,M)+SYNINPUT(:,L,M)

  130 CONTINUE
      DO 135 I=1,NSYN
         SYNM(M)=SYNM(M)+SYNIM(I,M)
  135 CONTINUE

!   +-------------------------------------------+
!   |   SAVE PRIMARY ENERGY EQUIVALENT FOR      |
!   |   EACH PERIOD                             |
!   +-------------------------------------------+

      DO L=1,NL
         DO I=1,NNI !number of primary energy
            ESILM(I,L,M)=ESIL(I,L)
         END DO
         !save primary energy equivalent for fusion by period
	   I=JUFUSION
         ESILM(I,L,M)=ESIL(I,L)
         !save primary energy equivalent for wind by period
	   I=JUWIND
         ESILM(I,L,M)=ESIL(I,L)
         !save primary energy equivalent for SWStor by period
	   I=JUWIND+1
         IF (NStype .ge. 1) ESILM(I,L,M)=ESIL(I,L)
         !save primary energy equivalent for next tech
	   I=JUWIND+2
         IF (NStype .ge. 2) ESILM(I,L,M)=ESIL(I,L)
      END DO


!
!   +-----------------------------------------------------------+
!   |   COMPUTE ENERGY DEMAND AND SUPPLY STATISTICS BY COUNTRY  |
!   +-----------------------------------------------------------+
      EDM(M)=0.D0
      DO 250 L=1,NL
         EDLM(L,M)=0.D0
         EDRLM(L,M)=0.D0
         ESLM(L,M)=0.D0
         ESRLM(L,M)=0.D0
         NKK=NKKL(L)
         DO 190 K=1,NKK + 1  ! with hydrogen production
            EDKL(K,L)=0.D0
            EFKL(K,L)=0.D0
            EDRKLM(K,L,M)=0.D0
            DO 150 I=1,NNI
               EDKL(K,L)=EDKL(K,L)+EDIKL(I,K,L)
               EDRIKLM(I,K,L,M)=EDRIKL(I,K,L)  
               EDRKLM(K,L,M)=EDRKLM(K,L,M)+EDRIKL(I,K,L)
  150       CONTINUE
            DO 170 J=1,NNJ
               EFKL(K,L)=EFKL(K,L)+EFJKL(J,K,L)
  170       CONTINUE
            EDLM(L,M)=EDLM(L,M)+EDKL(K,L)
            EDRLM(L,M)=EDRLM(L,M)+EDRKLM(K,L,M)
  190    CONTINUE
         DO 220 I=1,NNI 
           EDRILM(I,L,M)=EDRIL(I,L)
  220    CONTINUE
         EDM(M)=EDM(M)+EDLM(L,M)
  250 CONTINUE

!********MAW IPCC 1994  Add period fuel consumption by sector

      DO K=1,NKMAX
          DO J=1,NNJ
              FJKM(J,K,M) = 0.0
              DO L=1,NL
                  FJKLM(J,K,L,M) = FJKL(J,K,L)
                  FJKM(J,K,M) = FJKM(J,K,M) + FJKLM(J,K,L,M)
              END DO
          END DO
      END DO
!***************************************************************          
              
!
!   +--------------------------------------------------------+
!   |   COMPUTE ENERGY DEMAND AND SUPPLY STATISTICS BY FUEL  |
!   +--------------------------------------------------------+
      ESM(M)=0.D0
      ESRM(M)=0.D0
      EDRM(M)=0.D0
      EDRKM(M)=0.D0
      DO 350 I=1,NNI
         EDIM(I,M)=0.D0
         EDRIM(I,M)=0.D0
         EDRIKM(I,M)=0.D0
         ESIM(I,M)=0.D0
         ESRIM(I,M)=0.D0
         DO 300 L=1,NL
            EDIM(I,M)=EDIM(I,M)+EDIL(I,L)
            EDILM(I,L,M)=EDIL(I,L)
            EDRIM(I,M)=EDRIM(I,M)+EDRIL(I,L)
            ESIM(I,M)=ESIM(I,M)+ESIL(I,L)
            ESRIM(I,M)=ESRIM(I,M)+ESRILM(I,L,M)
            EDRIKM(I,M)=EDRIKM(I,M)+EDRIKL(I,1,L)
            ESLM(L,M)=ESLM(L,M)+ESIL(I,L)
            ESRLM(L,M)=ESRLM(L,M)+ESRILM(I,L,M)
  300    CONTINUE
         ESM(M)=ESM(M)+ESIM(I,M)
         ESRM(M)=ESRM(M)+ESRIM(I,M)
         EDRM(M)=EDRM(M)+EDRIM(I,M)
         EDRKM(M)=EDRKM(M)+EDRIKM(I,M)
  350 CONTINUE
!     Sum regional primary equivalent for fusion
!     use ESIL calculated in PS.for fusion's 
!     primary energy equivalent demand
      I=JUFUSION
	EDIM(I,M)=0.0
	DO L=1,NL
	   EDIM(I,M)=EDIM(I,M)+ESIL(I,L)
	END DO
!     Add fusion's primary equivalent to global total
	EDM(M)=EDM(M)+EDIM(I,M)
!
!   +---------------------------------------------+
!   |    COMPUTE ELECTRICITY SUPPLY STATISTICS    |
!   +---------------------------------------------+
!
      ESUM(M)=0.D0
      DO 360 I=1,NNU+NStype	!Include advanced "Solar-type" tech's
         ESUIM(I,M)=0.D0
  360 CONTINUE
      DO 380 L=1,NL
         ESULM(L,M)=0.D0
         DO 370 I=1,NNU+NStype	!Include advanced "Solar-type" tech's
            ESULM(L,M)=ESULM(L,M)+ESUILM(I,L,M)
            ESUIM(I,M)=ESUIM(I,M)+ESUILM(I,L,M)
  370    CONTINUE
  380 CONTINUE
      DO 390 I=1,NNU+NStype	!Include advanced "Solar-type" tech's
         ESUM(M)=ESUM(M)+ESUIM(I,M)
  390 CONTINUE

!   +---------------------------------------------+
!   |    COMPUTE HYDROGEN SUPPLY STATISTICS       |
!   +---------------------------------------------+

      DO I=1,NNH2 + NH2_New	! sjs -- 08/02
	   ESHIM(I,M)=0.D0
	END DO
      DO L=1,NL
         DO I=1,NNH2 + NH2_New	! sjs -- 08/02
            ESHIM(I,M)=ESHIM(I,M)+ESHILM(I,L,M)
         END DO
	END DO


!
!   +------------------------------------------------+
!   |   ASSEMBLE SECONDARY ENERGY DEMAND VARIABLES   |
!   +------------------------------------------------+
      DO 804 J=1,NNJ
         EFJM(J,M)=0.D0
         DO 802 L=1,NL
            EFJL(J,L)=0.D0
            NKK=NKKL(L)
            DO 801 K=2,NKK
               EFJL(J,L)=EFJL(J,L)+EFJKL(J,K,L)
  801       CONTINUE
            EFJLM(J,L,M)=EFJL(J,L)
            EFJM(J,M)=EFJM(J,M)+EFJL(J,L)
  802    CONTINUE
  804 CONTINUE
      EFM(M)=0.D0
      DO 803 L=1,NL
         EFLM(L,M)=0.D0
         DO 501 J=1,NNJ
            EFLM(L,M)=EFLM(L,M)+EFJL(J,L)
 501     CONTINUE
         EFM(M)=EFM(M)+EFLM(L,M)
  803 CONTINUE
!
!   +-------------------------------------------------------------+
!   |   COMPUTE END OF PERIOD GNP (MILLIONS OF 1975 US DOLLARS)   |
!   |   AND ENERGY USE PER DOLLAR GNP AND PER CAPITA              |
!   +-------------------------------------------------------------+
!
      ZLM(NLL,M)=0.D0
      EPGLM(NLL,M)=0.D0
      EPCLM(NLL,M)=0.D0
      GNPPCM(NLL,M)=0.D0
      GNPLM(NLL,M)=0.D0
      GNPFLM(NLL,M)=0.D0
      DO 550 L=1,NL
         GNPFLM(L,M)=GNPBL(L)*YLM(L,M)/(1.0D3)
         EPGLM(L,M)=EDLM(L,M)/GNPFLM(L,M)*(1.0D3)
         EPCLM(L,M)=EDLM(L,M)/ZLM(L,M)
         GNPPCM(L,M)=GNPFLM(L,M)/ZLM(L,M)
         ZLM(NLL,M)=ZLM(NLL,M)+ZLM(L,M)/(1.0D3)
         GNPLM(NLL,M)=GNPLM(NLL,M)+GNPLM(L,M)
         GNPFLM(NLL,M)=GNPFLM(NLL,M)+GNPFLM(L,M)
  550 CONTINUE
      EPGLM(NLL,M)=EDM(M)/GNPFLM(NLL,M)*(1.0D3)
      EPCLM(NLL,M)=EDM(M)/ZLM(NLL,M)*(1.0D3)
      GNPPCM(NLL,M)=GNPFLM(NLL,M)/ZLM(NLL,M)*(1.0D3)
!
!   +----------------------------------------------+
!   |   COMPUTE SUMMARY ENERGY SUPPLY STATISTICS   |
!   +----------------------------------------------+
!
!  --  AGGREGATE BY REGION
!
      ESR1M(M)=0.D0
      ESR2M(M)=0.D0
      DO 351 L=1,NL
         ESR1M(M)=ESR1M(M)+ESRL1M(L,M)
         ESR2M(M)=ESR2M(M)+ESRL2M(L,M)
         ESL2M(L,M)=0.D0
         DO 352 I=1,NF
            ESL2M(L,M)=ESL2M(L,M) + ESIL2M(I,L,M)
  352    CONTINUE
  351 CONTINUE
!
      NII=NF+1
      DO 353 L=1,NL
         ESL1M(L,M)=0.D0
         DO 354 I=1,NF
            ESL1M(L,M)= ESL1M(L,M)+ESIL1M(I,L,M)
  354    CONTINUE
         DO 355 I=NII,NNI
            ESL1M(L,M)= ESL1M(L,M) + ESIL(I,L)
  355    CONTINUE
  353 CONTINUE
!
!  --  AGGREGATE BY FUEL TYPE
!
      DO 652 I=1,NF
         ESI2M(I,M)=0.D0
         DO 651 L=1,NL
            ESI2M(I,M)=ESI2M(I,M) + ESIL2M(I,L,M)
  651    CONTINUE
  652 CONTINUE
!
      DO 356 I=1,NF
         ESI1M(I,M)=0.D0
         DO 357 L=1,NL
            ESI1M(I,M)= ESI1M(I,M)+ESIL1M(I,L,M)
  357    CONTINUE
  356 CONTINUE
      DO 358 I=NII,NNI
         ESI1M(I,M)=0.D0
         DO 359 L=1,NL
            ESI1M(I,M)= ESI1M(I,M) + ESIL(I,L)
  359    CONTINUE
  358 CONTINUE

!     Separate accounting for biomass

      I=IBMASS
	ESI1M(I,M)=0.D0
	ESI2M(I,M)=0.D0
	DO L=1,NL
	   ESI1M(I,M)= ESI1M(I,M)+ESIL1M(I,L,M)
         ESI2M(I,M)= ESI2M(I,M)+ESIL2M(I,L,M)   
	END DO

!     Compute global totals
!     Initialize global variables
	ESERV(NNLP,M)=0.0
      DO K=1,3
         ESERVK(K,NNLP,M)=0.0
      END DO
	
      DO L=1,NL
	   DO K=1,3
           ESERVK(K,NNLP,M)=ESERVK(K,NNLP,M)+ESERVK(K,L,M)
	   END DO
         ESERV(NNLP,M)=ESERV(NNLP,M)+ESERV(L,M)
	END DO


      RETURN
      END

