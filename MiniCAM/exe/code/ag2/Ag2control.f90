SUBROUTINE Ag2control

! This subroutine reads in the control input file which designates
! which input files to use

! written: 7/01 ktg

USE Ag2Global8

IMPLICIT NONE

CHARACTER*72 AGFILE(MAXFILES)
CHARACTER*11 TABLEMRK

! local integers: total number of files, indicies for nexttable, file number
INTEGER INFILES, INDIC, INF

DATA TABLEMRK/'INPUT_TABLE'/


INFILES=0
INDIC = 0

OPEN(1,FILE='Ag2control.csv')

CALL NEXTTABLE(TABLEMRK,INDIC)

! Loops until nexttable reaches the end of the file and is finished reading in       
DO WHILE(INDIC.EQ.1)
  INFILES = INFILES+1
  READ(1,*)AGFILE(INFILES)        
  CALL NEXTTABLE(TABLEMRK,INDIC)
END DO


! Open each file and read in the data into global arrays
Write(97,*) "****AgLU Model Inputs"		! Write to log file. sjs
DO INF=1,INFILES
  Write(*,*) 'Opening: ',AGFILE(INF)
  WRITE(97,*) 'Opening: ',AGFILE(INF)
  OPEN(1,FILE=AGFILE(INF))
  CALL Ag2GetData(AGFILE,INF)
  CLOSE (1)
END DO

BREAK = 1.0d0

END SUBROUTINE Ag2control