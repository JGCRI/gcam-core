!***********************************************************************
!
      SUBROUTINE   PPPP
!
!***********************************************************************
!
!            -- THE PRIMARY ENERGY PRICE PREPROSSESSOR --
!
! THIS SUBROUTINE COMPUTES THE PRICE OF PRIMARY ENERGY BY REGION
! AND FUEL TYPE FOR: OIL, GAS, SOLIDS, SOLAR AND HYDRO.  NUCLEAR
! ENERGY PRODUCTION COSTS ARE DETERMINED IN THE SUPPLY MODULE.
!
!
! VERSION:  A.31.07.84
!
! INTEGER INPUTS: NF,NCATCH,NHXIL,NI,NM,NXIL
! REAL INPUTS:    PIM,TRI,TXILM
!
! REAL OUTPUTS:   PILM
!
! LOCAL REALS:    T
!
! LOCAL INTEGERS: ISOLAR,MM1,NX,NT
!
! SUBROUTINES CALLED: NONE
!
! WRITTEN BY:
!   JAE EDMONDS                        LATEST REVISION:
!   1 JANUARY 1982                      31 JULY 1984
!                                       BY: JAE EDMONDS
!                                       * SECONDARY ENERGY AND GNP
!                                         ARE NO LONGER CALCULATED
!                                         IN THIS SUBROUTINE. THEY
!                                         ARE NOW CALCULATED IN
!                                         SUBROUTINE PSPS.
!                                       1 SEPTEMBER 88
!                                       BY DAVE BARNS
!                                         TO INCLUDE COMMON
!                                       15 AUGUST 1989
!                                       BY:  JAE EDMONDS
!                                         TO CORRECT PROBLEMS IN
!                                         THE NUCLEAR SUPPLY PRICE
!                                         CALCULATION.
!                                        7 DECEMBER 1989 TO INCORPORATE
!                                          SEVERANCE TAX TXISLM                          
!
!     Change PIMs to Ps to use in solution algorithm (international
!     market rather tha just global prices)  maw 3/28/97
!***********************************************************************
!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 TRI0(4)  !Base year 1975 transportation costs (75$/GJ)

!	DATA TRI0/0.1397,0.5,0.3409,0.681/
!	DATA TRI0/0.1397,0.5,0.14,0.681/


! Change TRI0 to equal transporation costs for other years.   sjs - 09/01
! No reason to change these since 75 data are not calibrated or used.

      TRI0(1:4) = TRI(1:4)

!              +---------------------------------+
!              |    SET PRIMARY ENERGY PRICES    |
!         +-------------------------------------------+
!         |    COMPUTE REGIONAL FOSSIL FUEL PRICES    | 
!         +-------------------------------------------+
         DO 10 I=1,NF
            IF (M.NE.1) THEN
	         PILM(I,L,M)=(P(I,L,M)+TRI(I))*TXILM(I,L,M)
            ELSE 
               PILM(I,L,M)=(P(I,L,M)+TRI0(I))*TXILM(I,L,M)
            END IF
   10    CONTINUE

	      I=IBMASS
	      IN=INBMASS
!     For biomass
            IF (M.NE.1) THEN
	         PILM(I,L,M)=(P(IN,L,M)+TRI(IN))*TXILM(IN,L,M)
            ELSE 
               PILM(I,L,M)=(P(IN,L,M)+TRI0(IN))*TXILM(IN,L,M)
            END IF

      RETURN
      END
