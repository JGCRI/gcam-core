!***********************************************************************

      SUBROUTINE OPTIN
            
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
!   Adapted for use with the optimizer   Mark Jacobsen 7/98
!
!***********************************************************************

      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
      CHARACTER*11 TABLEMRK
      DATA TABLEMRK/'INPUT_TABLE'/

 1000 FORMAT(A72)
      
      OPEN(1,FILE='OPTCTRL.CSV')
      

      READ(1,*)
      
      CALL NEXTTABLE(TABLEMRK,INDIC)

!     The nexttable subroutine routines INDIC=0 when it reaches the end
!     of a file, else returns 1.  Loop while INDIC=1, else finished.
      
      INFILES_opt=0
      
	  Write(97,*) "****Optimation Inputs"		! Write to log file. sjs
      DO WHILE(INDIC.EQ.1)
         INFILES_opt = INFILES_opt+1
         READ(1,1000)OptFILES(INFILES_opt)
         Write(*,*) 'Opening: ',OptFILES(INFILES_opt)
 		 WRITE(97,*) 'Opening: ',OptFILES(INFILES_opt)
         CALL PARSECOM(OptFILES(INFILES_opt))         
         CALL NEXTTABLE(TABLEMRK,INDIC)
      END DO
	  Write(97,*) 
         
      CLOSE (1)


      RETURN
      END


