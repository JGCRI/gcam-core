!***********************************************************************
!
      SUBROUTINE CARBTAX

!     Subroutine to translate carbon taxes or permit prices to fuel
!     taxes
!     Marshall Wise 3/97
!***********************************************************************
!
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!      Check to see if region has a fixed tax, if so - fix and return
         IF(NTAXMODE(L).LE.3) THEN
	      P(INCARB,L,M)=TAXRLM(L,M)
	      IPFIX(INCARB,L,M) = 1
	      CALL CTAXADJ

!     If carbon market, assign tax from market price
         ELSE IF(NTAXMODE(L).EQ.4) THEN
	      
            IF(CEMTARGS(L,M).GT.0.0) THEN
	         TAXRLM(L,M)=P(INCARB,L,M)
!	         IPFIX(INCARB,L,M)=0    ???
	      ELSE
	         TAXRLM(L,M)=0.0
	         P(INCARB,L,M)=0.0  !!?
	         IPFIX(INCARB,L,M) = 1
	      END IF
	      
            CALL CTAXADJ

         END IF

	RETURN
      END
!***********************************************************************
!***********************************************************************

      SUBROUTINE CTAXADJ
      
!***********************************************************************     

!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!     Don't let taxes be higher than backstop (if there is one set)

      IF(CFBSTOP(M).NE.0.D0) THEN
            TAXRLM(L,M) = MIN(TAXRLM(L,M),CFBSTOP(M))
	      P(INCARB,L,M)=TAXRLM(L,M)  !!?
      END IF



!*******************************PERFORM CARBON TAX ADJUSTMENTS*********
!   (NOTE: INDEX AT DO 1210, 1260, AND 1285 NEEDS TO BE SEPARATED IF
!   TXUILM HAS TAX APPLIED TO RENEWABLES.)
!
!  NTAXMODE =1; CONSUMPTION TAX ONLY
      IF (NTAXMODE(L).EQ.1) THEN
         DO 1220 I=1,4
         TXUILM(I,L,M)=TXUILM0(I,L,M)*TAXRLM(L,M)/100.0
         TXISLM(I,L,M)=0.0
 1220    CONTINUE
         DO 1230 K=1,3
            DO 1240 J=1,3
               TXJKLM(J,K,L,M)=TXJKLM0(J,K,L,M) &
                              *TAXRLM(L,M)/100.0
 1240       CONTINUE
 1230    CONTINUE
!
!  NTAXMODE = 2; 1/2 CONSUMPTION TAX; 1/2 SEVERANCE TAX
!
      ELSE IF (NTAXMODE(L).EQ.2) THEN
             DO 1260 I=1,4
                TXUILM(I,L,M)=TXUILM0(I,L,M)*TAXRLM(L,M)/200.0
                TXISLM(I,L,M)=TXISLM0(I,L,M)*TAXRLM(L,M)/200.0
 1260        CONTINUE
             DO 1265 K=1,3
                DO 1270 J=1,3
                   TXJKLM(J,K,L,M)=TXJKLM0(J,K,L,M) &
                                  *TAXRLM(L,M)/200.0
 1270           CONTINUE
 1265        CONTINUE
!
!   NTAXMODE = 3; SEVERANCE TAX ONLY
!
      ELSE IF (NTAXMODE(L).EQ.3) THEN
            DO 1285 I=1,4
               TXUILM(I,L,M)=0.0
               TXISLM(I,L,M)=TXISLM0(I,L,M)*TAXRLM(L,M)/100.0
 1285       CONTINUE
            DO 1290 K=1,3
               DO 1295 J=1,3
                  TXJKLM(J,K,L,M)=0.0
 1295          CONTINUE
 1290       CONTINUE
 
!   NTAXMODE = 4;  CARBON EMISSIONS TARGET
!     Based on consumption tax
      ELSE IF (NTAXMODE(L).EQ.4) THEN
            DO I=1,4
               TXUILM(I,L,M)=TXUILM0(I,L,M)*TAXRLM(L,M)/100.0
               TXISLM(I,L,M)=0.0
            END DO
            DO K=1,3
               DO J=1,3
                  TXJKLM(J,K,L,M)=TXJKLM0(J,K,L,M) &
                                 *TAXRLM(L,M)/100.0
               END DO
            END DO  
      END IF


!
!***************************END TAX ADJUSTMENTS************************
      RETURN
      END
      
!***********************************************************************

               