!***********************************************************************
!
      PROGRAM OPTMCAM
!
!***********************************************************************
!
!     Main MCAm caller.
!
!     Created by Marshall Wise,  9-27-95
!
!***********************************************************************
!
!***********************************************************************
!
!     For this version, the constraint is a cum emit target at time N, the 
!     decision variables are the carbon taxes in times 1 through N, and
!     the objective function is to minimize present value discounted 
!     costs.
!
!***********************************************************************

      USE OPTICOM

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)   
   
      Open(97,FILE="MCLog.txt")		!sjs -- MC log file
      Call InitLog
      
      SecondsNum = secondsN()	!SJS -- Code for timing   
	CALL OPTIN	!read optctrl.csv

	DO INF=1,INFILES_opt    
         OPEN(1,FILE=OptFILES(INF))      
         CALL OPTINFS(INF)  ! read optimization data
         CLOSE(1)
      END DO

!*************************************************      

!                           
!     Call model to read input only.

      MODE = 0
      CALL MCAMMAIN(MODE)
      
!     If not in optimizing mode:

      IF(INDOPT.EQ.0) THEN
         IF (COSTCALC.EQ.0) THEN
          Write(97,*)
          Write(97,*) "****Non-Optimizing Run Begin****"		! Write to log file. sjs

	      CALL MCAMLINK(INDOPT,TAX,GNP,CARB,0)	! Default -- use energy emissions only
	      CALL MCAMMAIN(2)
	   ELSE IF (COSTCALC.EQ.1) THEN
          Write(97,*)
          Write(97,*) "****Non-Optimizing Run with Cost Calc Begin****"		! Write to log file. sjs

	      CALL MITICOST1
	      CALL MITICOST
	   END IF

!     Else call the hotelling routine
      
      ELSE IF (INDOPT.EQ.1) THEN
         Write(97,*)
         Write(97,*) "****Optimizing Run Begin****"		! Write to log file. sjs

         DO L=1,NLP
            TAX(L,1) = 0.d0
         END DO
      
         CALL HOTELCUM

	   CALL MCAMMAIN(2)

	   IF (COSTCALC.EQ.1) THEN
	      TAXSTEPS(ISTEPS,:,3:NMP) = TAX(:,1:NMP-2)
	      EMISSSTEPS(ISTEPS,:,2:NMP) = CARB(:,1:NMP-1)
		  CALL MITICOST
	   END IF

      END IF


      Write(97,*)
      Write(97,*) "****Datamover Call Begin****"		! Write to log file. sjs

! **************************************************************************************
!	 call the c++ program to move the data to the database...
!	 the program takes the following parameters from the command line:
!
!	datamover.exe [database] [datatable] [source csv file] ["replace"]
!
!	the last argument is optional, if you specify "replace" the table is overwritten
!	the source csv file will be deleted by the c program

!	make sure ALL THE CSV's are CLOSED (access requires exclusive read)
	CLOSE(110) !main dbout.csv file
	CLOSE(108) !dbvarinfo.csv file
!	* * * * * * * 

  	idum = SYSTEMQQ("datamover.exe """//OPTDBNAME//""" dbmain dbout.csv")
  	idum = SYSTEMQQ("datamover.exe """//OPTDBNAME//""" dbrunlabels dbruninfo.csv")
   	idum = SYSTEMQQ("datamover.exe """//OPTDBNAME//""" dbvarlabels dbvarinfo.csv replace")
! **************************************************************************************


!     Write the "I'm done" file to let stupid Windows programs know
!     you are done executing

! SJS -- Code for timing      
      SecondsNum = secondsN() - SecondsNum
      
      OPEN(45,FILE='DONE.CSV')
      WRITE(45,*)'I am finished, thank you.'
      WRITE(45,"('Model run took: ',f8.1,' seconds')") SecondsNum
      CLOSE(45)

      Call CloseLog(SecondsNum)
                            
      STOP
      endprogram OPTMCAM
    
    
      
       Function secondsn()
       INTEGER(2) tmphour, tmpminute, tmpsecond, tmphund
   
		   call gettim(tmphour, tmpminute, tmpsecond, tmphund)
       secondsn = tmphund/100. + tmpsecond + tmpminute*60 + tmphour*60*60
      
      Return
      END

