!***********************************************************************

    SUBROUTINE MAGDBOUT(RunID)
    
	USE COMMON
	INTEGER RunID, VarID
	PARAMETER (NUMVARS = 24, NUMROWS = 8)
	CHARACTER*20 labels(1:NUMVARS), units(1:NUMVARS)
	Real*8 TempOut(1:NLPMax)
	
	labels= (/'Temp    ','CO2Conc ','CH4Conc ','N2OConc ','FcCO2   ', &
	          'FcCH4   ','FcN2O   ','FcHalos ','FcTropO3','FcSO4Dir', &
			  'FcSO4Ind','FcBioAer','FcTOTAL ','FossCO2 ','NetDefor', &
			  'CH4Em   ','N2OEm   ','DelSO2-1','DelSO2-2','DelSO2-3', &
			  'SeaLevel','FcKyoto ','FcHFCs  ','FcCFCSF6'/)

	units = (/'Temp  ','PPM   ','PPM   ','PPM   ','w/m2  ', &
              'w/m2  ','w/m2  ','w/m2  ','w/m2  ','w/m2  ', &
			  'w/m2  ','w/m2  ','w/m2  ','GtC/yr','GtC/yr', &
              'CH4   ','N     ','S     ','S     ','S     ', &
              'SLR   ','w/m2  ','w/m2  ','w/m2  '/)

    100 FORMAT(1I10,1H,,1I4,1H,,1I6,8(1H,,1F20.5))
    200 FORMAT(1I6,4(1H,,A))

	DO II = 1,NUMVARS
	  WRITE(108,200) 950101+II,'MAGICC','Summary',labels(II),units(II)

	  DO IV = 2,NLPMax
	   	MagInt =1+15/INT(MAGICCCResults(0,2)-MAGICCCResults(0,1))*(IV - 2)
	   	TempOut(IV) = MAGICCCResults(II,MagInt)
	  END DO

	  WRITE(110,100) RunID,0,950101+II,TempOut(2:NLPMax)
	END DO
      
    RETURN
    END

