!     MAGCOM.FOR - COMMON BLOCK
!     CREATE NEW VARIABLES SO THAT GHG CONCENTRATIONS 
!     FROM MAGICC ARE GLOBALLY ACCESSABLE.   SHK 3/4/99
!	  rewritten in module form 7/01 ktg

MODULE Magcom

	  IMPLICIT NONE

      INTEGER,PARAMETER :: NPER=9 !Maximum number of periods

      REAL(4) &
	    CO2ppmv(NPER),              & ! CO2 user concentration 
        CO2Hppmv(NPER),             & ! CO2 high concentration 
        CO2Mppmv(NPER),             & ! CO2 mid concentration 
        CO2Lppmv(NPER),             & ! CO2 low concentration 
        CH4ppmv(NPER),              & ! CH4 user concentration 
        CH4Hppmv(NPER),             & ! CH4 high concentration 
        CH4Mppmv(NPER),             & ! CH4 mid concentration 
        CH4Lppmv(NPER),             & ! CH4 low concentration 
        RN2Oppmv(NPER),             & ! N2O concentration 
        FCCO2(NPER),                & ! CO2 change in concentration 
        FCCH4(NPER),                & ! CH4 change in concentration 
        FCN2O(NPER),                & ! N2O change in concentration 
        FCHALOS(NPER),              & ! Halons change in concentration 
        FCTROPO3(NPER),             & ! Tropos concentration 
        FCSO4DIR(NPER),             & ! SO4 concentration 
        FCSO4IND(NPER),             & ! SO4 concentration 
        FCBIO(NPER),                & ! BIO concentration 
        FCTOT(NPER),                & ! TOT concentration 
        FOSSCO2(NPER),              & ! FOSS CO2 concentration 
        RNETDEF(NPER),              & ! CH4 concentration 
        CH4EM(NPER),                & ! N20 concentration 
        RN2OEM(NPER),               & ! CO2 concentration 
        SO2REG1(NPER),              & ! SO2 concentration 
        SO2REG2(NPER),              & ! SO2 concentration 
        SO2REG3(NPER)                 ! SO2 concentration

END MODULE Magcom