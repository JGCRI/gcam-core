!***********************************************************************

      SUBROUTINE OPTINFS(INF)
            
!***********************************************************************
!
!	Same as other case/variable format input reading files created
!	by Marshall Wise.
!
!	Reads optimization inputs
!	Mark Jacobsen 8/98
!
! NOTE: hard coded region number in optional tax readin
!
!***********************************************************************
    
      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      CHARACTER*11 TABLEMRK
      DATA TABLEMRK/'INPUT_TABLE'/
      
!   put a control file structure in later to read from a variable file
!  name(s)   (do it marshall)
      
      
!     find the location of the first input data.

      CALL NEXTTABLE(TABLEMRK,INDIC)

!     If no data is found (or file is empty), notify and stop

      IF(INDIC.EQ.0) THEN
	   WRITE(*,*)'The ghosts in the machine have determined that'
	   MsgStr = "The file <"//OptFILES(INF)//"> was not found or has no data:"
       Call MCLog(1,MsgStr,0,0,0,0)
	   STOP
	END IF
      

!     The nexttable subroutine routines INDIC=0 when it reaches the end
!     of a file, else returns 1.  Loop while INDIC=1, else finished.

      DO WHILE(INDIC.EQ.1)

      READ(1,*)
!     Read the variable indicator label and select the right code.
      READ(1,'(I3)')IVARNUM 
      
      READ(1,*)
      READ(1,*)
      
      SELECT CASE(IVARNUM)
      
	CASE(1)
		READ(1,*)INDOPT

	CASE(2)
	    READ(1,*)TXSTART
	    txstart2(:) = TxStart	! Default -- same starting point for each regional group

	CASE(3)
	    READ(1,*)NTAXES
	    If (NTAXES .gt. 7) NTAXES = 7	! Make sure not > last period. Is problem since opt routines have different numbering for years

	CASE(4)
	    READ(1,*)IRATE

	CASE(5)
	    READ(1,*)NLG
        txstart2(:) = TxStart	! Default -- same starting point for each regional group

	CASE(6)
	    DO II=1,NLG
	        READ(1,*)NLINGC(II),(LGMEMBC(II,IJ),IJ=1,NLINGC(II))
	    END DO
	    
	CASE(7)
	    DO II=1,NLG
	        READ(1,*) WHICHTARG(II)
	    END DO


	CASE(8)
		DO II = 1, NLG
		   READ(1,*)CUMTARG(II)
		END DO

	CASE(10)
	    READ(1,*)COSTCALC

	CASE(11)
	    READ(1,*)SUMYEAR

	CASE(12)
		DO II = 1, NLG
		   READ(1,*) txstart2(II)
		END DO

	CASE(74) 
          DO L=1,14
            READ (1,*) IDUM,(DefaultTax(L,MM), MM=1,NTaxes)	! Translate to Opt routine counting system
          END DO

         CASE DEFAULT
     	MsgStr = "No readin instructions exist for variable #:"
        Call MCLog(2,MsgStr,0,0,0,1.0*ivarnum)
      END SELECT
         
      CALL NEXTTABLE(TABLEMRK,INDIC)
      END DO

 1000 FORMAT(I3,20F10.0)
 1010 FORMAT(20F10.0)
 1020 FORMAT(2I3,20F10.0)

      RETURN
      END      


