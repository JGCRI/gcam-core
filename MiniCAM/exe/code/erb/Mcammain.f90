!***********************************************************************

      SUBROUTINE MCAMMAIN(MODE)

!***********************************************************************
!
!     MiniCAM main program converted to subroutine form so that it can
!     be called by the optimizing program.
!
!     Converted by Marshall Wise  10-2-95
!
!     MODE=0, model only does read in and initialization
!     MODE=1, model skips read in and runs NM periods
!     MODE=2, model reads, runs and creates output

!***********************************************************************

!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/BCOC/FBC1990, FOC1990, FSO2_dir1990,FSO2_ind1990
	  
	  INTEGER RunID
      INTEGER(2) tmpday, tmpmonth, tmpyear
      INTEGER(2) tmphour, tmpminute, tmpsecond, tmphund
!	vars above provide a timestamp for database output


!     Set switch to run ag module (0=no, 1=yes)

      IF(MODE.EQ.0) THEN
         CALL DATAIN
         
         !Call ag module in data input and initialization mode
	     
		 IF (AGMODEL.EQ.0) THEN ! If using old Ag model           
           MODEAG = 0
		   CALL AGMAIN(MODEAG)
		 ELSE					! If using new Ag model
		   MODEAG = 1
		   CALL AG2LINK(MODEAG) 
		 END IF

      END IF

!
!
!             ***********************************
!             ***   ITERATE OVER NM PERIODS   ***
!             ***********************************
      
      IF(MODE.EQ.1) THEN 
            
      DO 30 M=1,NM
       T_Start = SecondsN()
       
	   CALL ANTEPER
      
       CALL SOLUTN
! 	   CALL POSTPER		moved to Model.f90. sjs - 02/02
       CALL CheckESerDemand (M)
       
       IF ((M .eq. 2) .and. (DoCalib .eq. 1)) CALL CALIB90	! Calibrate starting 1990 values
       IF (M .eq. 2) Call ScaleCoefs	! Scale B coefficients between 1990 to 2050
       IF (M .gt. 2) Call CALIB_Other	! Calibrate values for other years, if necessary
       
	IF (IOTHGASTOG.NE.1 .and.  IOTHERGHG_Const() .eq. 0) THEN !if other gases not in carblimit do emiss once
	   IF (AGMODEL.EQ.1) THEN	! Get emissions from new AgLU model
	  	   MODEAG = 3
		   CALL AG2LINK(MODEAG)
	   END IF

	   CALL ALLOTHERGASES
	   IF (M .gt. 2) CALL SULFUREM
	END IF

! SulfurEM call moved here. Any other routine that needs calibrated GDP values
! should be here, after calib for first period (for other periods, needs to be in MODEL)
       IF (M .eq. 2) CALL SULFUREM

       CALL UPDATE_SJKLP	! This results in coefficients that are lagged one period       
       Call ReportErrors	! Report any errors from non-CO2 GHG routines
       
       WriteTime = (SecondsN()-T_Start)
! 		Write(*,*) "Timestep: ",M," Runtime = ",WriteTime," seconds"
		Write(97,*) "Timestep: ",M," Runtime = ",WriteTime," seconds"

!  		CALL MAGICCLINK(M)				! transfers emissions for MAGICC 
! 		CALL CLIMAT(0,MAGICCCResults,MagEM)	! Run MAIGICC without writting out values
!     	write(*,'(a,I6,": ",16f6.1)') "Emissions 1: ", nint(MagEM(1,1)), MagEM(M,:)
!  		Write(*,'(a,12f9.1)') "results(2): ",MAGICCCResults(1:11,M-1)
 		
 		ISEC = 4 !so2 producing "sector" 4 is elec
		J = 3
	    L = 1
!	    Write(*,'(a,12f9.1)') "Sulfur: ",SO2EMSJLM(ISEC,J,L,M),EDRIKLM(J,1,L,M), &
!	    SO2GDP0(L),SO2TAU(L),SO2CTRL(ISEC,J,L,M),1000*GNPPPP(L,M)/ZLM(L,M), &
!		SO2MAXCTRL(ISEC,J,L)
!		
!		Write(*,*)
		
   30  CONTINUE

       IF (DoCalib .eq. 1) CALL WriteCalFile
       
      Write(97,*)
      Write(97,*) "****Model Run Ended (Call to CLIMAT begin)****"		! Write to log file. sjs
	  
!	Get the MAGICC results
	CALL MAGICCLINK(NM+1)	!writes out the "gas.em" file (now 'gas.csv')
	CALL CLIMAT(1,MAGICCCResults,MagEM)

      Write(97,*) "****CLIMAT Called****"		! Write to log file. sjs
	
!	Write(*,'(12f9.1)') MAGICCCResults(0,1:9)
!	Write(*,'(12f9.1)') MAGICCCResults(1,1:9)
!	Write(*,'(12f9.1)') MAGICCCResults(2,1:9)
!	Write(*,'(12f9.1)') MAGICCCResults(3,1:9)
!	CALL ISAM_M

      END IF	! Mode = 1
      
!     end of time loop****************************************
  

      IF(MODE.EQ.2) THEN
        Write(97,*)
        Write(97,*) "****Data Output Begin****"		! Write to log file. sjs

!		get a runid for database output calls
	    CALL GETDAT(tmpyear, tmpmonth, tmpday)
	    CALL GETTIM(tmphour, tmpminute, tmpsecond, tmphund)
	    RunID = tmpsecond + 100 * tmpminute + 10000 * tmphour +  &
                  1000000 * tmpday + 100000000 * tmpmonth

!          CALL CASEOUT
           IF (GenRelease .ne. 1) &	! don't output general output files if general release
           CALL FULLOUT(1,0,1)	! Changed -- sjs. Can specify which detailed output files want.
           						! Useful for calibration, and also non-database operation
!           CALL Elec_Gen ! Generate some elec generation statistics

! Must call ERBOUTPUT -- some CO2 statistics are done here
	    CALL ERBOUTPUT

  		CALL DBOUTPUT(RunID)
      
!     Call ag module in output mode (AGMODEL=0 for old model,=1 for new model)
        IF (AGMODEL.EQ.0) THEN
		  MODEAG = 2
          CALL AGMAIN(MODEAG)		  
 		  CALL AGDBOUT(RunID) ! Call ag database output
		ELSE
		  MODEAG = 4
		  CALL AG2LINK(MODEAG)
		  CALL AG2DBOUT(RunID,NL) ! Call AgLU database output
		END IF

!	call MAGICC database parser (parse the magicc outfile to the db)
 		CALL MAGDBOUT(RunID)
          
	    CALL OPTPASSRUNID(RunID, DBNAME) !pass the RunID to the "opt" routines

      END IF	! Mode = 2
!
       close(100)
	RETURN
      END
 

     SUBROUTINE CheckESerDemand(Mxx)

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      Integer Mxx

! Warning to note when elasticity is such that income scalar pushes energy service
! elasticity down
! Exception for case if income decreases -- is to be expected then
      
      IF (ISRESeserDmd .eq. 0) then
      IF (Mxx .gt. 1) then
       Do L = 1, NLP
        X  = YLM(L,Mxx)/(ZLM(L,Mxx)/ZLM(L,1))
        XX = YLM(L,Mxx-1)/(ZLM(L,Mxx-1)/ZLM(L,1))
  
        IHasDone = 0	! Only bother printing warning once, since likely to be same for all sectors
        Do K = 1, 3      
         AIncEserScale1 = X**RYKLM(K,L,Mxx)
         AIncEserScale0 = XX**RYKLM(K,L,Mxx-1)

         IF ((AIncEserScale1 .LT. AIncEserScale0) .and. (IHasDone .EQ. 0)  &
              .and. (YLM(L,Mxx) > YLM(L,Mxx-1))) Then
            MsLvl = 3
            IF ((AIncEserScale1/AIncEserScale0) .lt. 0.75) MsLvl = 2	! Write to screen if difference is large
            MsgStr = " E Serv Income Scalar at (M,L,K) decreased by: "
            Call MCLog(MsLvl,MsgStr,Mxx,L,K,AIncEserScale1/AIncEserScale0)
            IHasDone = 1
         END IF
        End Do     
       End Do     

      End If
      End IF
      
      END


     SUBROUTINE UPDATE_SJKLP()
! -- Code to update SJKLP's. SJS 10/00. The result is that these are generally lagged one period. 
! If don't do this, H2 and biomass never come into energy serv calc
! This also can make the model much harder to solve, so leave out for now

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

         DO L=1,NLP
         DO K=1,3
 			SJKLP(1:NNJ,K,L) = SJKL(1:NNJ,K,L)	! Comment this out to not update SJKLP's
 			TempSum = SUM(SJKL(1:NNJ,K,L))
		   IF (abs((TempSum - 1)) .gt. 0.01) THEN
		        MsgStr = "SJKL sum is off by:"
                Call MCLog(2,MsgStr,M,L,K,Tempsum-1)
		   END IF		   
         END DO 
       END DO  ! L loop

      END


!**********************************************************************
!**********************************************************************

      SUBROUTINE ReportErrors
!**********************************************************************
!     If errors were reported in emissions routines, report them
!      mj  7/01
!	Moved. sjs 02/02

!**********************************************************************

!  .  Include Common Blocks

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	DO L = 1,NL
	  DO igas = 1, NOGMax
		
!		IF (igas.GE.10.AND.igas.LE.24) CYCLE  ! hard coded to skip all the 0 errors for the high GWP's

	    DO isrc = 1, NOGSrcMax

		  IF (OGERROR(igas,isrc,L).EQ.1) THEN
	      	MsgStr = "Other Gas ERROR: 0 base year activity level for source #: "
			Call MCLog(2,MsgStr,M,L,igas,1d0*isrc)
		  END IF



		END DO
	  END DO
	END DO


    RETURN
	END
