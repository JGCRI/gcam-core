!***********************************************************************

      SUBROUTINE ERBCTRL
            
!***********************************************************************
!
!   This subroutine reads the input files control file and assigns
!     these input files to the FILES array used by the rest of the
!     program.
!     
!     It calls the PARSECOM subroutine to strip commas and/or spaces
!     at the end of input file names.
!
!
!   Created by  Marshall Wise
!     March, 1995
!
!***********************************************************************

! sjs - Changed 9/00 so that number of regions is input parameter, not hard coded

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*11 TABLEIN,TABLEMRK,RGNMARK
      DATA TABLEMRK/'INPUT_TABLE'/
      DATA RGNMARK/'INPUT_REGNS'/

 1000 FORMAT(A72)
      
      OPEN(1,FILE='ERBCTRL.CSV')
      
!     Read case name and case notes, and output file name.

      READ(1,*)
      READ(1,1000)CASENAME
      READ(1,1000)CASENOTE
      READ(1,1000)DBNAME
      READ(1,*)

      Write(97,*)
      WRITE(97,*)"Scenario Name: "//CASENAME	! Write to log file. sjs
      WRITE(97,*)"Scenario Note: "//CASENOTE
      Write(97,*)

! Section to read in number of regions - SJS
! Is written so that will be compatable with old erbctrl file, defaulting to 14 regions
! New flag, = -1 means that region variable not found but regular input file was found

      CALL NEXTTABLE2(RGNMARK,TABLEMRK,INDIC)
      
      IF (INDIC .EQ. 1) Then
      	Read(1,'(I4)') NLP
      ELSE
      	NLP = 14
      	INDIC = -1
      END IF
      NNLP = NLP + 1
      
      IF (INDIC .NE. -1) CALL NEXTTABLE(TABLEMRK,INDIC) 
      IF (INDIC .EQ. -1) INDIC = 1 

!     The nexttable subroutine routines INDIC=0 when it reaches the end
!     of a file, else returns 1.  Loop while INDIC=1, else finished.
      
      INFILES=0
      
      DO WHILE(INDIC.EQ.1)
         INFILES = INFILES+1
         READ(1,1000)FILES(INFILES)
         CALL PARSECOM(FILES(INFILES))         
         CALL NEXTTABLE(TABLEMRK,INDIC)
      END DO
         
      CLOSE (1)

      RETURN
      END


!*********************************************************************

!  .  Alternative subroutine to advance to next input table -- sjs
!     modified so that new version of code is compatable with old input files without number of regions variable
!  .  Gets file to one line past the marker.  Returns 1 if successful,
!  .  returns a 0 on end-of-file, returns -1 if found regular input marker.

      SUBROUTINE NEXTTABLE2(TABLEMRK,AltMrk,INDIC)

!*********************************************************************
      CHARACTER*11 TABLEIN,TABLEMRK, AltMrk

 3000 FORMAT(A11)
      READ(1,3000,END=10) TABLEIN
      ICNT=0
      DO WHILE ((TABLEIN.NE.TABLEMRK .and. TABLEIN.NE.AltMrk)  &
      			 .AND. ICNT.LT.1000)
         ICNT = ICNT+1
         READ(1,3000,END=10) TABLEIN
      END DO
      INDIC=1
      IF (TABLEIN.EQ.AltMrk) INDIC = -1
      RETURN

! . . On end of file
   10 CONTINUE
      INDIC=0
      RETURN

      END


     
