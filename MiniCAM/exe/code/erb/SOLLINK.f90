!**********************************************************************

      SUBROUTINE SOLLINK
!**********************************************************************
!     Links erb and ag/land use market indexes to the market definition
!     system used in the solution algorithm
!
!     Created by Marshall Wise for the MiniCAM 3/97

!**********************************************************************

!  .  Include Common Blocks

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	  Real(8) ExcessD(NMRKP)
	  Real(8) Temp_GDP(2),Temp_Em(2),CI1,CI2	! Carbon & GHG Intensity Vars

      MRKSAVE=MRK

!     
!     Initialize markets to zero

      DO MRK=1,NMRK
	    MRKDEM(MRK)=0.0
	    MRKPRD(MRK)=0.0
	END DO

!     Loop over markets and compute demands and supplies

      DO MRK=1,NMRK
	    IN=MRKDEF(MRK,2)

	    IF(IN.LE.INCOAL) THEN                    
              NR = MRKDEF(MRK,3)
              DO LL=1,NR
	            L=MRKDEF(MRK,3+LL)
	            MRKDEM(MRK) = MRKDEM(MRK) + EDRIL(IN,L) 
	            MRKPRD(MRK) = MRKPRD(MRK) + ESRILM(IN,L,M)
	        END DO

	    ELSE IF(IN.EQ.INBMASS) THEN
              NR = MRKDEF(MRK,3)
	          I=IBMASS
              DO LL=1,NR
	            L=MRKDEF(MRK,3+LL)
	            MRKDEM(MRK) = MRKDEM(MRK) + EDRIL(I,L) 
	            MRKPRD(MRK) = MRKPRD(MRK) + ESRILM(I,L,M)
	        END DO

! Branches for various possible carbon targets
! CarbConstraintType = 0	-- Basic carbon target
! CarbConstraintType = 1	-- Total GHG target
! CarbConstraintType = 2	-- CO2 concentration target
! CarbConstraintType = 3	-- Total Forcing Target
! CarbConstraintType = 4	-- GHG Forcing Target
!
! CarbConstraintType = 5	-- Carbon Intensity Target
! CarbConstraintType = 6	-- Carbon Intensity Rate Target
! CarbConstraintType = 7	-- GHG Intensity Target
! CarbConstraintType = 8	-- GHG Intensity Rate Target
!  
! If alter these constraints, update function IOTHERGHG_Const below
!
         
	    ELSE IF(IN.EQ.INCARB) THEN
			NR = MRKDEF(MRK,3)
	        IF (CarbConstraintType .le. 1) THEN
				DO LL=1,NR
	              L=MRKDEF(MRK,3+LL)
	              MRKDEM(MRK) = MRKDEM(MRK) + EMISSTCE(L,M) 
	              MRKPRD(MRK) = MRKPRD(MRK) + CEMTARGS(L,M)
	            END DO
	        ELSE IF (CarbConstraintType .le. 4) THEN ! IF aggreate climate target
				MRKPRD(MRK) = CEMTARGS(L,M)
				SELECT CASE(CarbConstraintType)
				CASE(2)	! CO2 Concentration
	              	MRKDEM(MRK) = MAGICCCResults(2,M-1)
				CASE(3)	! Total Forcing
	              	MRKDEM(MRK) = MAGICCCResults(13,M-1)
				CASE(4)	! GHG Forcing
	              	MRKDEM(MRK) = MAGICCCResults(5,M-1) + MAGICCCResults(6,M-1) &
	              	            + MAGICCCResults(7,M-1) + MAGICCCResults(8,M-1)	   
	            END SELECT           	
	        ELSE IF (CarbConstraintType .le. 8) THEN ! IF carbon or GHG intensity target (or rate target)
				MRKPRD(MRK) = CEMTARGS(L,M)
				Temp_GDP(:) = 0
				Temp_Em(:) = 0
				DO LL=1,NR
				  L=MRKDEF(MRK,3+LL)
				  Temp_Em(1) = EMISSTCE(L,M-1) + Temp_Em(1)
				  Temp_Em(2) = EMISSTCE(L,M) + Temp_Em(2)
				  IF (IPPPInten .eq. 1) THEN	! PPP-based GDP's
				  	Temp_GDP(1) = GNPPPP(L,M-1) + Temp_GDP(1)
				  	Temp_GDP(2) = GNPPPP(L,M) + Temp_GDP(2)
				  ELSE	! Default
				  	Temp_GDP(1) = GNPMRKT(L,M-1) + Temp_GDP(1)
				  	Temp_GDP(2) = GNPMRKT(L,M) + Temp_GDP(2)
				  END IF
				END DO
				CI1 = Temp_Em(1)/Temp_GDP(1)*1000.	! Past period Intensity in Tonnes of Carbon per million $1990
				CI2 = Temp_Em(2)/Temp_GDP(2)*1000.	! Current period Intensity
				
			 	SELECT CASE(CarbConstraintType)
				  CASE(5)						! Carbon Intensity Target
					MRKDEM(MRK) = CI2
!					IF (M.gt.3) Write(*,'(I2,": ",a,5(f9.0,","))') M,"Prev, MRKDEM, MRKPRD: ",CI1,MRKDEM(MRK),MRKPRD(MRK),P(INCARB,L,M)
				  CASE(6)						! Carbon Intensity Rate Target
					MRKDEM(MRK) = CI2
					IF (CEMTARGS(L,M) .gt. 0) THEN
					   MRKPRD(MRK) = CI1*(1d0-CEMTARGS(L,M))**(15d0)
!					   Write(*,'(I2,": ",a,5(f9.0,","))') M,"Prev,  MRKDEM, MRKPRD: ",CI1,MRKDEM(MRK),MRKPRD(MRK),P(INCARB,L,M)
!					   Write(*,'(a,3(f13.3,","))') "---",CEMTARGS(L,M),(1d0-CEMTARGS(L,M)),(1d0-CEMTARGS(L,M))**(15d0)
					ELSE
					   MRKPRD(MRK) = C12		! No target if negative
					END IF
				  CASE(7)						! GHG Intensity Target
					MRKDEM(MRK) = CI2
				  CASE(8)						! GHG Intensity Rate Target
					MRKDEM(MRK) = CI2
					IF (CEMTARGS(L,M) .gt. 0) THEN
					   MRKPRD(MRK) = CI1*(1d0-CEMTARGS(L,M))**(15d0)
					ELSE
					   MRKPRD(MRK) = C12
					END IF
!					MRKDEM(MRK) = (CEMTARGS(L,M))*100	! need to invert
!					MRKPRD(MRK) = ((CI2/CI1)**(1d0/15d0)-1d0)*100
	            END SELECT    
	          !  write(*,'("Dem, prod: ",2(f7.1,", "))') MRKDEM(MRK), MRKPRD(MRK)
	        ELSE
				MsgStr = "Illegal Carbon Target Option"
				Call MCLog(1,MsgStr,0,0,0,1d0*CarbConstraintType)
				MRKDEM(MRK) = 0
	        END IF

	    ELSE   !Pure ag sectors
              NR = MRKDEF(MRK,3)
              DO LL=1,NR
	            L=MRKDEF(MRK,3+LL)
	            MRKDEM(MRK) = MRKDEM(MRK) + AGDEM(IN,L)
	            MRKPRD(MRK) = MRKPRD(MRK) + AGSUP(IN,L)
	        END DO

	    END IF
	    ExcessD(MRK) = MRKDEM(MRK)-MRKPRD(MRK)
IF (M .eq. 666) Write(*,'(3(a,I4),(a,f9.1),(a,f7.0,a))') &
  "Modl", MODL, " IN: ",IN, " Mrk:", MRK," EXD: ", ExcessD,&
  " (", ExcessD/MRKPRD(MRK)*100,"%)"

	END DO

 IF (M .eq. 666) THEN
      N_mrk_prnt = NMRK
      IF (NMRK .gt. 11) N_mrk_prnt = 11
      ExcessD(:) = ExcessD(:)/MRKPRD(:)*100
     Write(*,'((a4,I4),a5,11i7)') "Modl", MODL, " IN: ",MRKDEF(1:N_mrk_prnt,2)
     Write(*,'(a2,I4,a7,11f7.0)') " ",MODL, " ExD%: ",ExcessD(1:N_mrk_prnt)
 END IF
 
      MRK=MRKSAVE
	RETURN
	END


!**********************************************************************

      SUBROUTINE UNPERTURB(MODE)
!**********************************************************************
!
!     Saves or restores unperturbed demands and supplies so that all
!     regions do not have to be re-run for each marker evalutation
!     unless they are part of that market

!     MODE=0  saves values
!     MODE=1 restores values
!
!     Created by Marshall Wise for the MiniCAM 5/97

!**********************************************************************

!  .  Include Common Blocks

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	REAL*8 EDRILS(NIP,NLPMax),ESRILS(NIP,NLPMax),AGDEMS(NINP,NLPMax), &
      AGSUPS(NINP,NLPMax),EMISSTCESV(NLPMax),CEMTARSV(NLPMax)

      SELECT CASE(MODE)

	CASE(0)

	   DO LL=1,NL
	      L=MRKDEF(0,3+LL)
	      DO I=1,NNI
	         EDRILS(I,L) = EDRIL(I,L)
	         ESRILS(I,L) = ESRILM(I,L,M)
	      END DO
	      DO IN=1,NIN
	         AGDEMS(IN,L) = AGDEM(IN,L)
	         AGSUPS(IN,L) = AGSUP(IN,L)
	      END DO
            EMISSTCESV(L) = EMISSTCE(L,M) 
	      CEMTARSV(L) = CEMTARGS(L,M)
	   END DO

	CASE(1)

	   DO LL=1,NL
	      L=MRKDEF(0,3+LL)
	      DO I=1,NNI
	         EDRIL(I,L) = EDRILS(I,L)
	         ESRILM(I,L,M) = ESRILS(I,L)
	      END DO
	      DO IN=1,NIN
	         AGDEM(IN,L) = AGDEMS(IN,L)
	         AGSUP(IN,L) = AGSUPS(IN,L)
	      END DO
            EMISSTCE(L,M) = EMISSTCESV(L) 
	      CEMTARGS(L,M) = CEMTARSV(L)
	   END DO

	END SELECT

	RETURN
	END


FUNCTION IOTHERGHG_Const()
! ******************************************************************
! This function returns 1 if the carbon constraint requires the 
! calcuation of non-CO2 GHG emissions
! ******************************************************************

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	  INTEGER Local_OTHERGHG
	  
	  Local_OTHERGHG = 0
      IF ((CarbConstraintType .ge. 2) .and. (CarbConstraintType .le. 4)) Local_OTHERGHG = 1
      IF ((CarbConstraintType .eq. 7) .and. (CarbConstraintType .eq. 8)) Local_OTHERGHG = 1
      
      IOTHERGHG_Const = Local_OTHERGHG
      
END FUNCTION
