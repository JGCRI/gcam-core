!***********************************************************************
!
      SUBROUTINE   MCLog (Mtype,MsgStr,M,L,K,Value)
      Character(80) MsgStr
      Real*8 Value
      Integer Mtype,M,L,K
      
!***********************************************************************
!
!                  -- Routine to log messages to an output file --
!
! Messages may have the following predefined types
!
! 	Type 1: "Error:" -- something has gone wrong, this is written to screen and to file
! 	Type 2: "Warning:" -- something may have gone wrong, this is also written to screen and to file
! 	Type 3: "Note:" -- informative (written to file only)
!					-- This is useful to note exceptions, and other places where the model does
!					-- something that may not be obvious.
!   Other: Any other number will print to file with no prefix
!
! Note: MsgStr must be a character variable (one called "MsgStr" is in the common block for this purpose)
!
! Steve Smith - 02/00
!
!***********************************************************************
!
      Character*20 MsgPrefix
      
      SELECT CASE(Mtype)
      
         Case(1)
           MsgPrefix = "**Error**: "
           Write(*,'(a,"(",2(I2,","),I2,")"," ",a,"  ",f11.4)')  &
                 Trim(MsgPrefix)//" at ",M,L,K,Trim(MsgStr),Value

         Case(2)
           MsgPrefix = "Warning: "
           Write(*,'(a,"(",2(I2,","),I2,")"," ",a,"  ",f11.4)')  &
                 Trim(MsgPrefix)//" at ",M,L,K,Trim(MsgStr),Value
         Case(3)
           MsgPrefix = "  Note: "

         CASE DEFAULT
           MsgPrefix = ""
           
      END SELECT
      
       Write(97,'(a,"(",2(I2,","),I2,")"," ",a,"  ",f11.4)')  &
                 Trim(MsgPrefix)//" at ",M,L,K,Trim(MsgStr),Value

      
      RETURN
      END



      Subroutine  InitLog
      INTEGER(2) tmphour, tmpminute, tmpsecond, tmphund,tmpyear, tmpmonth, tmpday
      
      Write(97,*) "Minicam Output Log "
      
      call gettim(tmphour, tmpminute, tmpsecond, tmphund)
      CALL GETDAT(tmpyear, tmpmonth, tmpday)
      Write(97,'(I2,":",I2,":",I2,"   --  ",I2,"/",I2,"/",I4)')  &
            tmphour, tmpminute, tmpsecond,tmpday,tmpmonth,tmpyear
     
      
      Write(97,*)
      Write(97,*) "M,L,X,Msg"
      
      Return
      END
      
      
      SUBROUTINE  CloseLog(SecondsNum)	
      
	  USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      CHARACTER*72 OptFILES      
      COMMON/CONTR1/INFILES_opt,OptFILES(10) ! From Opticom.f90
     
      WRITE(97,"('Model run took: ',f8.1,' seconds')") SecondsNum
      Write(97,*)

      Close(97)
      Return
      END
      
