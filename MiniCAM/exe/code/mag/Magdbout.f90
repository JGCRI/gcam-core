!***********************************************************************

    SUBROUTINE MAGDBOUT(RunID)
    
	USE COMMON
	INTEGER RunID, VarID
	PARAMETER (NUMVARS = 21, NUMROWS = 8)
	CHARACTER*20 labels(1:NUMVARS), units(1:NUMVARS)

	labels= (/'Temp    ','CO2Conc ','CH4Conc ','N2OConc ','FcCO2   ', &
	          'FcCH4   ','FcN2O   ','FcHalos ','FcTropO3','FcSO4Dir', &
			  'FcSO4Ind','FcBioAer','FcTOTAL ','FossCO2 ','NetDefor', &
			  'CH4Em   ','N2OEm   ','DelSO2-1','DelSO2-2','DelSO2-3', &
			  'SeaLevel'/)

	units = (/'Temp  ','PPM   ','PPM   ','PPM   ','w/m2  ', &
              'w/m2  ','w/m2  ','w/m2  ','w/m2  ','w/m2  ', &
			  'w/m2  ','w/m2  ','w/m2  ','GtC/yr','GtC/yr', &
              'CH4   ','N     ','S     ','S     ','S     ', &
              'SLR   '/)

    100 FORMAT(1I10,1H,,1I4,1H,,1I6,8(1H,,1F20.5))
    200 FORMAT(1I6,4(1H,,A))

	DO II = 1,NUMVARS
	  WRITE(108,200) 950101+II,'MAGICC','Summary',labels(II),units(II)
	  WRITE(110,100) RunID,0,950101+II,MAGICCCResults(II,1:NM-1)
	END DO
      
    RETURN
    END

