

!	**** MAIN SUBROUTINE FOR THE MAGICC LINK***

	SUBROUTINE MAGICCLINK(Nper)
      USE COMMON
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	REAL*8 AGPCO2(NM), HGWPMAG(8,2:NM),WMRSULF(3)
	REAL*8 CO2Adj, DefAdj,AdjFact

	Integer Nper
    Integer, Save :: WriteFlag  = 0	! Flag to toggle writeout to MAGICC

! sjs - 05/01 
! sjs - 02/02 Cement emissions moved to common block

! sjs - 05/01
! write extrapolated emissions out to 2055 so that can get better WRE approx
! mj - 7/02  removed magicclinkII, ag emissions passed to erb arrays earlier
! sjs - 08/03. Code to write QEXTRA added so as to include BC/OC forcing in MAGICC


	! Write QEXTRA
	CALL WriteMagExtra
	
	AGPCO2(2:NM) = SUM(CARBLAND(1:NL,2:NM),DIM=1)/1000  ! sum and convert land use emissions

! Option added to use deforestation emissions as read-in instead of AgLU version (above).  sjs - 02/02
	IF (UserDeforestEm .eq. 1) AGPCO2(2:NM) = DeForest(2:NM)

	
    NPoints = NM-1+3	!	+3 for 2100, 2150, & 2290
    IF (Nper .le. NM) NPoints = NPer-1

IF (WriteFlag .eq. 1) THEN
	OPEN(45,FILE='GAS.EMK')
	WRITE(45,855) NPoints  !tells magicc how many rows to expect
	WRITE(45,*)	CASENAME  !for mag.out
	WRITE(45,'(a,I2)')	"Emissions to period: ",NPer
	WRITE(45,*)	
	WRITE(45,*)	
  855 FORMAT(I2)
END IF
    MagEM(1,1) = Float(NPoints)


!**** SJS 
	CO2Adj = 0	! SJS Added code to adjust to SRES Standardized emissions
	DefAdj = 0	! Default values are to do no adjustments

    NLoop = Min(Nper,NM)
! Write emissions loop	

    if (SUM(HGWPREAD(3,:,:)) .gt. 0) then	! HGWP's read in directly via SRES
	!	pick out the 8 high gwp emissions that magicc requires from the 
	!	ones read into HGWPREAD
		HGWPMAG(1,2:NM) = SUM(HGWPREAD(3,:,2:NM),DIM=1) !CF4
		HGWPMAG(2,2:NM) = SUM(HGWPREAD(1,:,2:NM),DIM=1) !C2F6
		HGWPMAG(3,2:NM) = SUM(HGWPREAD(4,:,2:NM),DIM=1) !HFC125
		HGWPMAG(4,2:NM) = SUM(HGWPREAD(5,:,2:NM),DIM=1) !HFC134a
		HGWPMAG(5,2:NM) = SUM(HGWPREAD(6,:,2:NM),DIM=1) !HFC143a
		HGWPMAG(6,2:NM) = SUM(HGWPREAD(8,:,2:NM),DIM=1) !HFC227ea
		HGWPMAG(7,2:NM) = SUM(HGWPREAD(11,:,2:NM),DIM=1)!HFC245ca
		HGWPMAG(8,2:NM) = SUM(HGWPREAD(14,:,2:NM),DIM=1)!SF6
	else	! Calculated by the model
	
	! These indicies need to be the same as in Allothergases
	
	IH245 = 10	! HFC245fa equiv
	IH134 = 11	! HFC134a equiv
	IH125 = 12	! HFC125 equiv (including HFC227ea)
	IH143 = 13	! HFC143a equiv
	IHSF6 = 14	! SF6 equiv
	IHC2F6 = 15	! C2F6 equiv
	IHCF4 = 16	! CF4 equiv
	! Conver to kt for MAGICC
		do MM = 2, NLoop
			HGWPMAG(1,MM) = SUM(OGEMISS(IHCF4,:,:,MM))*1000. !CF4
			HGWPMAG(2,MM) = SUM(OGEMISS(IHC2F6,:,:,MM))*1000. !C2F6
			HGWPMAG(3,MM) = SUM(OGEMISS(IH125,:,:,MM))*1000. !HFC125
			HGWPMAG(4,MM) = SUM(OGEMISS(IH134,:,:,MM))*1000. !HFC134a
			HGWPMAG(5,MM) = SUM(OGEMISS(IH143,:,:,MM))*1000. !HFC143a
			HGWPMAG(6,MM) = 0.0 !HFC227ea
			! convert to HFC245ca since this is what MAGICC expects
			HGWPMAG(7,MM) = SUM(OGEMISS(IH245,:,:,MM)) * 640/basegwp(IH245)*1000. !HFC245ca
			HGWPMAG(8,MM) = SUM(OGEMISS(IHSF6,:,:,MM))*1000. !SF6
		end do
	end if
	

	DO MM = 2, NLoop
! Put internal cement emissions into array. Convert from MMT CO2 to GtC
	  ICemt = 9 
	  IF ( SUM(OGREPORT(ICemt,:)).GT.0 ) THEN 
	    Cement(MM) = SUM( OGEMISS(ICemt,:,:,MM) ) / 3.664 / 1000
	  END IF
              
! Calcuate adjustments due to standardization, if used.
	AdjFact = 1. - (MM*15.+1960.-2000.)/100.		! sjs. Standardization adjustments
	! Declining offset for deforestation and SO2 (SO2 not implimented yet)
	IF ((MagCalb .eq. 1) .and. (MM .eq. 3)) THEN
	   CO2Adj = CO2DM(2)/1000. + (CO2DM(3)-CO2DM(2))*2./3./1000.   
	   ! Extrapolated yr 2000 value
       CO2Adj = SRESCALB(1,2) - (CO2Adj+Cement(MM))
       DefAdj = AGPCO2(2) + (AGPCO2(3)-AGPCO2(2))*2./3.		
	 ! Extrapolated yr 2000 value
       DefAdj = SRESCALB(2,2) - DefAdj
	END IF

	IF ((MagCalb .eq. 1) .and. (MM .eq. 2)) THEN
       CO2Adj = SRESCALB(1,1) - (CO2DM(2)/1000. + Cement(MM))
       DefAdj = SRESCALB(2,1) - AGPCO2(2)
       AdjFact = 1.0
	END IF

! IF stand. value = 0, then turn off standardization
	IF (SRESCALB(1,1) .eq. 0) CO2Adj = 0
	IF (SRESCALB(2,1) .eq. 0) DefAdj = 0

	IF (MM .le. 3) THEN
		IF (CO2Adj .ne. 0) THEN
	  		MsgStr = "CO2 Emissions Adjusted by: "
      		Call MCLog(4,MsgStr,0,0,0,CO2Adj)
      	END IF
		IF (DefAdj .ne. 0) THEN
	  		MsgStr = "Deforestation Emissions Adjusted by: "
      		Call MCLog(4,MsgStr,0,0,0,DefAdj)
      	END IF
	END IF
	

! Total and split emissions values as appropriate for MAGICC
	  iyrfull = 1975 + (MM-1)*15
	  finalch4 = SUM(OGEMISS(1,:,:,MM))             !sum the ch4's
	  finaln2o = SUM(OGEMISS(2,:,:,MM)) * 1/NtoN2O  !sum the n20's and convert to N
	  finalnox = SUM(OGEMISS(4,:,:,MM)) / 3.2857    !convert NO2 to N for MAGICC
	  finalvoc = SUM(OGEMISS(6,:,:,MM))				!sum voc
	  finalco  = SUM(OGEMISS(5,:,:,MM))				!sum co

!	  convert sulfur emissions to magicc regions
	  WMRSULF(1) = SUM(SO2MAGREG(:,1) * SO2EM(:,MM)) + SO2SHIP(MM)/3.0
	  WMRSULF(2) = SUM(SO2MAGREG(:,2) * SO2EM(:,MM)) + SO2SHIP(MM)/3.0
	  WMRSULF(3) = SUM(SO2MAGREG(:,3) * SO2EM(:,MM)) + SO2SHIP(MM)/3.0

! Write emissions
     Call SetMagEm(WriteFlag,iyrfull,CO2DM(MM)/1000+CO2Adj+Cement(MM),AGPCO2(MM)+DefAdj*AdjFact, &
        finalch4,finaln2o,WMRSULF(:),HGWPMAG(:,MM),finalnox,finalvoc,finalco)

	END DO	! Emissions loop

		
!	************************************************
!	extend emissions to 2100 & 2150 to get future approx -- 06/01 sjs
	CO2_1 = CO2DM(NM-1)/1000+CO2Adj+Cement(NM-1)	! 2080 Emissions
	CO2_2 = CO2DM(NM)/1000+CO2Adj+Cement(NM)	! 2095 Emissions
	CO2_exr = (CO2_2-CO2_1)*5/15
	IF (Nper .gt. NM) &
	    Call SetMagEm(WriteFlag,2100,CO2_2+CO2_exr,0.0d0,finalch4,finaln2o,WMRSULF(:),HGWPMAG(:,NM),finalnox,finalvoc,finalco)
	
	! Arbitrary extrapolation of emissions. Mainly to get reasonable 2100 values
	IF (CO2_2 .lt. CO2_1) THEN
		CO2_exr = Max(CO2_2+(CO2_2-CO2_1)*55/15,1d0)	! 2150 extrapolated Emissions
    ELSE
		CO2_exr = (CO2_2-CO2_1)*0.5		! 2150 extrapolated Emissions
    END IF
    
	IF (Nper .gt. NM) &
	Call SetMagEm(WriteFlag,2150,CO2_exr,0.0d0,finalch4,finaln2o,WMRSULF(:),HGWPMAG(:,NM),finalnox,finalvoc,finalco)
! Also assume land-use has come into balance

!	extend emissions to 2290 for sustain program
!	the line below could be commented for normal runs
!	** be sure to also change the number of expected points above...

! use same emissions for 2290 since don't generally use this
	IF (Nper .gt. NM) &
	Call SetMagEm(WriteFlag,2290,CO2_exr,0.0d0,finalch4,finaln2o,WMRSULF(:),HGWPMAG(:,NM),finalnox,finalvoc,finalco)

!	*************************************************


	CLOSE(45)

	RETURN
	END

Subroutine SetMagEm(WriteFlag, iyr,xIndustCO2,xNetDefCO2, &
        finalch4,finaln2o,SulfurEM,HGWPEm,finalnox,finalvoc,finalco)

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    Integer iyr, WriteFlag
    Real(8) xIndustCO2,xNetDefCO2,finalch4,finaln2o,finalnox,finalvoc,finalco
    Real(8) SulfurEM(3),HGWPEm(8)   

IF (WriteFlag .eq. 1) THEN 
	  WRITE(45,854) iyr,xIndustCO2,xNetDefCO2,finalch4,finaln2o,SulfurEM(:),HGWPEm(:),finalnox,finalvoc,finalco
END IF
    MM = (iyr - 1990)/15 + 2
    IF (iyr .eq. 2100) MM = (2095 - 1990)/15 + 2 +1
    IF (iyr .eq. 2150) MM = (2095 - 1990)/15 + 2 +2
    IF (iyr .eq. 2290) MM = (2095 - 1990)/15 + 2 +3
    
	MagEM(MM,1) = Float(iyr)
	MagEM(MM,2) = xIndustCO2
	MagEM(MM,3) = xNetDefCO2
	MagEM(MM,4) = finalch4
	MagEM(MM,5) = finaln2o
	MagEM(MM,6:8) = SulfurEM(:)
	MagEM(MM,9:16) = HGWPEm(:)
	MagEM(MM,17) = finalnox
	MagEM(MM,18) = finalvoc
	MagEM(MM,19) = finalco
 IF (1.EQ.2) &	
 WRITE(*,855) " MC: ",MM,iyr,xIndustCO2,xNetDefCO2,finalch4,finaln2o,SulfurEM(:),HGWPEm(:),finalnox,finalvoc,finalco
  855 FORMAT(a,2(I4,','),20(F7.2,','))
 

  854 FORMAT(I4,',',20(F7.2,','))

RETURN
END


Subroutine WriteMagExtra()

      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/BCOC/FBC1990, FOC1990, FSO2_dir1990,FSO2_ind1990

      Real*8 SO2Hist(2,200)
      Real*8 FossForcing90, BioBForcing90, BioB_BCfract, BioB_OCfract
      Real*8 BCOCFValue, FossilFScale, BiomassBScale
      Real*8 BC_OCForcing,BC_OCNext, BCEmiss, OCEmiss
	  Real*8 BCELast,OCELast, BCEmiss90, OCEmiss90
      INTEGER NHist, II, StartYr,EndYr, N, Year, NextYear

	IBC = 7
	IOC = 8

      OPEN (45, FILE='..\magTAR\SO2Hist.csv')
      READ (45,*)
	  READ (45,*) NHist
      READ (45,*) ! Read lables

	  DO II = 1,NHist
	    READ (45,*) SO2Hist(1,II), SO2Hist(2,II)
		
      END DO

	  CLOSE (45)
	  
	  StartYr = 1765
	  EndYr = 2100
	  
	  Year = 1990
	  BCEmiss90 = SUM(OGEMISS(IBC,:,:,2))
	  OCEmiss90 = SUM(OGEMISS(IOC,:,:,2))
	  BCELast = BCEmiss90
	  OCELast = OCEmiss90
	  
	  IF (BCEmiss90 .NE. 0 .and. OCEmiss90 .NE. 0) THEN

  	    BioB_BCfract = SUM(OGEMISS(IBC,16:19,:,2))/BCEmiss90
	    BioB_OCfract = SUM(OGEMISS(IOC,16:19,:,2))/OCEmiss90
	  
	    FossForcing90 = (1 - BioB_BCfract) * FBC1990 + (1 - BioB_OCfract) * FOC1990
	    BioBForcing90 = (BioB_BCfract) * FBC1990 + (BioB_OCfract) * FOC1990
	  
	    BC_OCForcing = FBC1990 + FOC1990

        NextYear = 2005
	    BCEmiss = SUM(OGEMISS(IBC,:,:,3))
	    OCEmiss = SUM(OGEMISS(IOC,:,:,3))
        BC_OCNext = FBC1990 * (BCEmiss/BCEmiss90) + FOC1990 * (OCEmiss/OCEmiss90)

	    MsgStr = "Fraction Biomass Burning: BC"
        Call MCLog(4,MsgStr,0,0,1,BioB_BCfract)
	    MsgStr = "Fraction Biomass Burning: OC"
        Call MCLog(4,MsgStr,0,0,1,BioB_OCfract)
      ELSE
	    BioB_BCfract = 0
        BioB_OCfract = 0
		BioBForcing90 = 0
        FossForcing90 = 0
        NextYear = 2005
	  END IF
	  
	  OPEN (45,FILE='..\magTAR\QEXTRA.IN')
	  WRITE(45,'(1X,I5)') 1
	  WRITE(45,'(1X,2I5)') StartYr, 2250
	  
	  DO II = StartYr,EndYr
	     IF ( II .LT. SO2Hist(1,1) ) THEN
	        BiomassBScale = float(II - StartYr)/float(1990 - StartYr)
	        BCOCFValue = BiomassBScale * BioBForcing90

	        WRITE(45,'(1X,I5,F10.4)') II, BCOCFValue
	     ELSE IF (II .LE. SO2Hist(1,NHist) ) THEN
	        N = II - SO2Hist(1,1) + 1
	        FossilFScale = abs(SO2Hist(2,N) / SO2Hist(2,NHist))
	        BiomassBScale = float(II - StartYr)/float(1990 - StartYr)
	        BCOCFValue = FossilFScale * FossForcing90 + BiomassBScale * BioBForcing90
	
	        WRITE(45,'(1X,I5,F10.4)') II, BCOCFValue
	     ELSE
            If (II .EQ. NextYear) THEN
			  Year = II
 			  IF (NextYear .LT. 2095) THEN
                NextYear = NextYear + 15
				BCELast = BCEmiss
				OCELast = OCEmiss
	            BCEmiss = SUM(OGEMISS(IBC,:,:,(NextYear-1975)/15))
	            OCEmiss = SUM(OGEMISS(IOC,:,:,(NextYear-1975)/15))
			  ELSE
			    NextYear = 2100
				tmpBCEmiss = (BCEmiss - BCELast)/3 + BCEmiss
				tmpOCEmiss = (OCEmiss - OCELast)/3 + OCEmiss
				BCELast = BCEmiss
				OCELast = OCEmiss
				BCEmiss = tmpBCEmiss
				OCEmiss = tmpOCEmiss
			  END IF
  			END IF
		    
			BCEm = float(II-Year)/float(NextYear-Year) * (BCEmiss - BCELast) + BCELast
			OCEm = float(II-Year)/float(NextYear-Year) * (OCEmiss - OCELast) + OCELast
            
			IF (II .EQ. 2100) THEN
			   BCEm = BCELast
			   OCEm = OCELast
			END IF

			IF (BCEmiss90 .NE. 0) THEN
			   BCOCFValue = FBC1990 * (BCEm/BCEmiss90) + FOC1990 * (OCEm/OCEmiss90)
			ELSE
			   BCOCFValue = 0
			END IF
		 	WRITE(45,'(1X,I5,F10.4)') II, BCOCFValue

		 END IF
      END DO
 
 	  DO II = EndYr+1,2250
	     WRITE(45,'(1X,I5,F10.4)') II, BCOCFValue
      END DO
 
      CLOSE(45)

RETURN
END
