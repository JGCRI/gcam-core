!**********************************************************************
!**********************************************************************

      SUBROUTINE POSTPER
!**********************************************************************
!     Run after each period is solved to update stuff
!     Created by Marshall Wise for the MiniCAM 3/97

!**********************************************************************

!  .  Include Common Blocks

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	REAL*8 PPPDELTA,PPPDCVRT

	NLL=NL+1

!              +-----------------------+
!              |   SUMMARIZE RESULTS   |
!              +-----------------------+
!
      CALL POST

!  .  Update starting prices for the next period.  This overides
!  .  the prices read in.  IPFIX allows selective use
!  .  of override for case where market price is given.  This is used
!  .  where imports supply gap at fixed price.  eg. where there is a world
!  .  price for oil.  IPFIX = .true. is to use read in value.

      IF (M .LT. NM) THEN
            DO L=1,NL
               DO IN = 1,NIN
                  IF (IPFIX(IN,L,M+1) .NE. 1) THEN
                     P(IN,L,M+1) = P(IN,L,M)
                  END IF
               END DO
            END DO
      END IF

!    TEST UPDATE PRICE ON TREND

      ITREND=1
      IF (M.GT.1 .AND. M.LT.NM .AND. ITREND.EQ.1) THEN
             DO L=1,NL
               DO IN = 1,NIN
                  IF (IPFIX(IN,L,M+1) .NE. 1) THEN
                     P(IN,L,M+1) = (P(IN,L,M)**2)/(P(IN,L,M-1)+.0001)
                  END IF
               END DO
            END DO
      END IF



!***********************SAVE VALUES OF VARIABLES***********************
!     

!     Zero so not summed when called repeatedly by optimizer.
      GNPFM(M)=0.0
	GNPMRKT(NLL,M)=0.0
	GNPPPP(NLL,M)=0.0

      DO K=1,NKMAX+1
         EFKM(K,M)=0.0
      END DO
      
      DO 890 L=1,NL
      IF (M.LE.2) THEN
          TAXLM(L,M)=0.0
      ELSE
          TAXLM(L,M)=TAXRLM(L,M)
      ENDIF
          GNPFM(M)=GNPFM(M)+GNPFLM(L,M)
          DO 891 K=1,NKMAX+1
               EFKLM(K,L,M)=EFKL(K,L)
               EFKM(K,M)=EFKM(K,M)+EFKLM(K,L,M)
  891 CONTINUE

! PPP conversion moved to xxxx so that PPP is always available. sjs - 02/02
!         OLD WAY
!	    GNPPPP(L,M) = GNPFLM(L,M) * PPPCONV(L)
          GNPMRKT(L,M) = GNPFLM(L,M)

      IF (M.GE.2) THEN	! Makes GDP_ppp and GDP_mkt equal at $15,000 per cap ($1990)
        Crosspt = 0.015
		
		PPPDELTA = LOG(PPPCONV(L)) / LOG(((GNPFLM(L,2)/ZLM(L,2)) / Crosspt)) ! Corrected. Needs to be 1990 base GDP/cap, not 1975
		PPPDCVRT = ((GNPFLM(L,M)/ZLM(L,M)) / Crosspt) ** PPPDELTA

	    GNPPPP(L,M) = GNPFLM(L,M) * PPPDCVRT
	    IF (GNPMRKT(L,M)/ZLM(L,M).GT.Crosspt) GNPPPP(L,M) = GNPMRKT(L,M)
	    IF (PPPCONV(L).LT.1.2) GNPPPP(L,M) = GNPMRKT(L,M) * PPPCONV(L)		! Leaves out currently most industrialized countries. Adjusted, sjs -- 01/02

      ELSE
        GNPPPP(L,M) = 0.0	! Conversion not defined before 1990 base year
      END IF
      
!         SUM FOR GLOBAL
	    GNPMRKT(NLL,M) = GNPMRKT(NLL,M) + GNPMRKT(L,M)
	    GNPPPP(NLL,M) = GNPPPP(NLL,M) + GNPPPP(L,M)

  890 CONTINUE
 

!	CALL SULFUREM	-- Call to SulfurEM moved to MCamMain. sjs 10/01
!	CALL NONGHGEMISS	-- Generic non-CO2 GHG emissions routine


      RETURN
	END
