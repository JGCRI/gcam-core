!***********************************************************************

      SUBROUTINE MAGDBOUT(RunID)
                
      
	INTEGER RunID, VarID

	PARAMETER (NUMCOLS = 22, NUMPERS = 8)

	CHARACTER*20 labels(NUMCOLS), units(NUMCOLS)
	REAL*8 DBAR(NUMCOLS,NUMPERS)

	units = (/'      ','Temp  ', &
      'PPM   ','PPM   ','PPM   ','w/m2  ','w/m2  ', &
      'w/m2  ','w/m2  ','w/m2  ','w/m2  ','w/m2  ','w/m2  ', &
      'w/m2  ','GtC/yr','GtC/yr','GtC/yr','GtC/yr','GtC/yr','GtC/yr', &
      'GtC/yr','SLR   '/)

  100 FORMAT(1I10,1H,,1I4,1H,,1I6,8(1H,,1F20.5))
!  200 FORMAT(1I5,1H,,1A100,1H,,1A20)
  200 FORMAT(1I6,4(1H,,A))

	OPEN(9,file='magout.csv')
	
	READ(9,*) labels
	DO MM = 1,8
	  READ(9,*) DBAR(:,MM)
	END DO
	
	CLOSE(9)

	DO II = 2,NUMCOLS  !start at 2 bc first col is year
	  WRITE(108,200) 950100+II,'MAGICC','Summary',labels(II),units(II)
	  WRITE(110,100) RunID,0,950100+II,DBAR(II,:)
	END DO
      
      RETURN
      END
      