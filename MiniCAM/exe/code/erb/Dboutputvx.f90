
!	sub called once for each variable to be written, assigns it a varid
!	and an index for the array to hold all output and writes it to the master table

! modified 10/02 to incorporate different release modes
! variables with Irelease = 1 are reported globally only

	SUBROUTINE ivid(irelease,pct,psct,idx,lstr,ustr)
!***********************************************************************
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	INTEGER varid, pct, psct, idx, irelease
	CHARACTER*(*) lstr, ustr
	CHARACTER*20 l2str
  200 FORMAT(I6,4(1h,,A))
!	assign a 6 digit varid to the item
	varid = pct*10000+psct*100+idx
	IF (vl(pct,psct,idx).ne.0) THEN
	   WRITE(6,*) 'PCT ', PCT,'PSCT ', PSCT,'IDX ',IDX
	   PAUSE 'Duplicate VarID:'
	END IF
!	print the varid table entry for the item

! Add "g" suffix if only available as a global variable
    l2str  = lstr
	IF (irelease .eq. 1) l2str  = lstr // " (g)"
	l2str  = Trim(l2str)
	
	WRITE(108,200) varid, ctstr(pct), sctstr(pct,psct), l2str, ustr


!	assign the varid to the next available array index
	dbaridx = dbaridx + 1
	vl(pct,psct,idx) = dbaridx
	rvl(dbaridx) = varid
	gblvar(dbaridx) = varid
	IF (irelease .eq. 1) gblvar(dbaridx) = 0
	RETURN
	END

      SUBROUTINE DBOUTPUT(RunID)     
!***********************************************************************     
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!     More detailed energy accounting 2-d flatfile output
!
	REAL*8 DBAR(0:5000,2:NM), DBARG(0:5000,2:NM)
	INTEGER RunID, vidx, ct, sct
	LOGICAL vp, exists, writenewtran !vp is if to print out a varid file
	CHARACTER*50 dboutfile, dbinfofile, varinfo,GRPINFO
	CHARACTER*2 INTTOCHAR(0:10)
	CHARACTER*5 SECTL(3), FUELL(6), SO2FUELL(4), SO2SECTL(4)
      CHARACTER*6 TSERV(3), TMD(8), TFUEL(7)
	CHARACTER*12 TUNIT(2), TempPriceUnit
	CHARACTER*10 CCURVLABELS(1:5,1:9)
	Real*8 PP_oil(2:NM),PP_gas(2:NM)
	
	writenewtran = .false. !skip the detailed tran output if not present (faster & smaller)
	IF (NewTransp .eq. 1) writenewtran = .true. 
!     Convert c. oil prices in $/GJ to $/bbl
      CVRTO = 1.055*5.8
!     Convert refined oil (gasoline) prices in $/GJ to $/gal
      CVRTAG = 0.1055*1.1540
!     Convert n. gas prices in $/GJ to $/mcf
      CVRTG = 1.055
!     Convert bituminous coal prices in $/GJ to $/tonne
      CVRTC = 29.1
!	INTTOCHAR = (/'0','1','2','3','4','5','6','7','8','9','10'/)
	FUELL = (/'Oil ','Gas ','Coal','Elec','Biom','H2  '/)
	SECTL = (/'Bld ','Ind ','Tran'/)
	SO2FUELL = (/'Oil  ','Gas  ','Coal ','Biom '/)
	SO2SECTL = (/'Bld  ','Ind  ','Tran ','Elec '/)
      TSERV = (/'Pass  ','Frght ','TotSer'/)
	TMD = (/'Auto  ','Truck ','Bus   ','Rail  ','Air   ','Ship  ', &
      'MotCy ','AllMod'/)
	TFUEL = (/'Oil   ','Gas   ','Coal  ','Elect ','Bioms ','H2    ', &
      'Total '/)
      TUNIT = (/'Mil Pass-mi ','Mil Ton-mi  '/)
	dboutfile = 'dbout.csv'
	dbinfofile = 'dbruninfo.csv'
	varinfo = 'dbvarinfo.csv'
!	See if we are appending or starting a new db buffer
	INQUIRE(FILE = dboutfile, EXIST = exists)
	IF (exists) THEN
	  OPEN (110, FILE = dboutfile, Position = 'APPEND')
	ELSE
	  OPEN (110, FILE = dboutfile)
	  WRITE(110,92) (1960 + MM * 15, MM=2,NM)
	END IF
   92 FORMAT('RunID',1H,,'Region',1H,,'VarID',8(1H,,'y',1I4))

!	** Write to the run information database **
	INQUIRE(FILE = dbinfofile, EXIST = exists)
	IF (exists) THEN
	  OPEN (109, FILE = dbinfofile, Position = 'APPEND')
	ELSE 
	  OPEN (109, FILE = dbinfofile)
	  WRITE(109,91) !91 just has the field names for the first line
	END IF
   91 FORMAT('RunID',1H,,'RunLabel',1H,,'RunComments',1H,,'Field4',1H,, &
      'Field5',1H,, &
      'Field6',1H,,'Field7',1H,,'Field8',1H,,'Field9',1H,,'Field10',1H,, &
      'Field11',1H,,'Field12',1H,, &
      'Field13',1H,,'Field14',1H,,'Field15',1H,,'Field16',1H,,'Field17', &
      1H,,'Field18',1H,, &
      'Field19',1H,,'Field20',1H,,'Field21',1H,,'Field22',1H,,'Field23', &
      1H,,'Field24',1H,, &
      'Field25',1H,,'Field26',1H,,'Field27',1H,,'Field28',1H,,'Field29', &
      1H,,'Field30',1H,, &
      'Field31',1H,,'Field32',1H,,'Field33')
   90 FORMAT(1I10,1H,,1A20,1H,,1A100,30(1H,,1A50))
	WRITE(109,90) RunID,CASENAME,CASENOTE,FILES(1:INFILES)
	CLOSE (109)
!	** Close the run information database **
  100 FORMAT(1I10,1H,,1I4,1H,,1I6,8(1H,,1F20.5))
!	INITIALIZATION
	DBARG = 0.0 !initialize global totals
	vl = 0
	rvl = 0
	gblvar = 0
	dbaridx = 0
      DO 850 L=1,NL !REGION DO LOOP 
!	------------------------------------------------------------------------------------
!	  Begin database output format code
!	------------------------------------------------------------------------------------
	DBAR = 0.0 !initialize DBAR
  300 FORMAT('VarID,','Cat,','SubCat,','VarLabel,','VarUnits')
  400 FORMAT(2(I5,1H,),1H,,3(A,1H,))
  410 FORMAT('GrpID,','VarID,',1H,,'VarLabel,','GrpLabel,','GrpUnit,')
	vp = .FALSE.
	IF (L .EQ. 1) vp = .TRUE. !only print varid if region 1
	IF(vp) THEN
	  OPEN (108, FILE = varinfo)
	  WRITE(108,300)
	END IF

!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	ct = 10  ! co2 emiss
	ctstr(ct) = 'CO2 Emiss'
	sct = 01 ! byfuel
	sctstr(ct,sct) = 'by Fuel'
      IIFLR=9 !index for flared emissions
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','MMT')
	IF(vp)CALL ivid(0,ct,sct,2,'Gas','MMT')
	IF(vp)CALL ivid(0,ct,sct,3,'Coal','MMT')
	IF(vp)CALL ivid(0,ct,sct,4,'Biom','MMT')
	IF(vp)CALL ivid(0,ct,sct,5,'Flared','MMT')
	IF(vp)CALL ivid(0,ct,sct,6,'ShaleOil Prod','MMT')
	IF(vp)CALL ivid(0,ct,sct,99,'Total ','MMT')
	DBAR(vl(ct,sct,1),:) = CARBFUEL(INOIL,L,2:NM) - CO2ILM(2,L,2:NM)	! Subtract CO2 from shaleoil production -- account for this below
	DBAR(vl(ct,sct,2),:) = CARBFUEL(INGAS,L,2:NM)
	DBAR(vl(ct,sct,3),:) = CARBFUEL(INCOAL,L,2:NM)
	DBAR(vl(ct,sct,4),:) = CARBFUEL(INBMASS,L,2:NM)
	DBAR(vl(ct,sct,5),:) = CO2ILM(IIFLR,L,2:M)
	DBAR(vl(ct,sct,6),:) = CO2ILM(2,L,2:NM)
	DBAR(vl(ct,sct,99),:) = CO2DLM(L,2:NM)
	sct = 10 ! by sector
	sctstr(ct,sct) = 'by Sector'
	IF(vp)CALL ivid(0,ct,sct,1,'BLD','MMT')
 	IF(vp)CALL ivid(0,ct,sct,2,'IND','MMT')
 	IF(vp)CALL ivid(0,ct,sct,3,'TRN','MMT')
 	IF(vp)CALL ivid(0,ct,sct,4,'ELEC','MMT')
 	IF(vp)CALL ivid(0,ct,sct,5,'Synfuel Prod','MMT')
 	IF(vp)CALL ivid(0,ct,sct,6,'H2 Prod','MMT')
 	IF(vp)CALL ivid(0,ct,sct,99,'Total','MMT')
      DO K=1,6
	   DBAR(vl(ct,sct,K),:) = CO2KLM(K,L,2:NM)
	END DO
	DBAR(vl(ct,sct,99),:) = SUM(CO2KLM(1:6,L,2:NM),DIM=1)
	sct = 11 ! from Buildings
	sctstr(ct,sct) = 'Buildings'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','MMT')
 	IF(vp)CALL ivid(0,ct,sct,2,'Gas','MMT')
 	IF(vp)CALL ivid(0,ct,sct,3,'Coal','MMT')
 	IF(vp)CALL ivid(0,ct,sct,99,'Total','MMT')
      DO J=1,3
	   DBAR(vl(ct,sct,J),:) = CO2JKLM(J,1,L,2:NM)
	END DO
	DBAR(vl(ct,sct,99),:) = SUM(CO2JKLM(1:3,1,L,2:NM),DIM=1)
	sct = 12 ! from Industries
	sctstr(ct,sct) = 'Industries'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','MMT')
 	IF(vp)CALL ivid(0,ct,sct,2,'Gas','MMT')
 	IF(vp)CALL ivid(0,ct,sct,3,'Coal','MMT')
 	IF(vp)CALL ivid(0,ct,sct,4,'ShaleOil','MMT')
 	IF(vp)CALL ivid(0,ct,sct,5,'Flared','MMT')
 	IF(vp)CALL ivid(0,ct,sct,99,'Total','MMT')
      DO J=1,5
	   DBAR(vl(ct,sct,J),:) = CO2JKLM(J,2,L,2:NM)
	END DO
	DBAR(vl(ct,sct,99),:) = SUM(CO2JKLM(1:5,2,L,2:NM),DIM=1)
	sct = 13 ! from Transportation
	sctstr(ct,sct) = 'Transportation'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','MMT')
 	IF(vp)CALL ivid(0,ct,sct,2,'Gas','MMT')
 	IF(vp)CALL ivid(0,ct,sct,3,'Coal','MMT')
 	IF(vp)CALL ivid(0,ct,sct,99,'Total','MMT')
      DO J=1,3
	   DBAR(vl(ct,sct,J),:) = CO2JKLM(J,3,L,2:NM)
	END DO
	DBAR(vl(ct,sct,99),:) = SUM(CO2JKLM(1:3,3,L,2:NM),DIM=1)
	sct = 14 ! from Electric Power
	sctstr(ct,sct) = 'Elect Pwr'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','MMT')
 	IF(vp)CALL ivid(0,ct,sct,2,'Gas','MMT')
 	IF(vp)CALL ivid(0,ct,sct,3,'Coal','MMT')
 	IF(vp)CALL ivid(0,ct,sct,99,'Total','MMT')
      DO J=1,3
	   DBAR(vl(ct,sct,J),:) = CO2JKLM(J,4,L,2:NM)
	END DO
	DBAR(vl(ct,sct,99),:) = SUM(CO2JKLM(1:3,4,L,2:NM),DIM=1)
	sct = 15 ! from Synfuel Production
	sctstr(ct,sct) = 'Synfuel'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','MMT')
 	IF(vp)CALL ivid(0,ct,sct,2,'Gas','MMT')
 	IF(vp)CALL ivid(0,ct,sct,3,'Coal','MMT')
 	IF(vp)CALL ivid(0,ct,sct,99,'Total','MMT')
      DO J=1,3
	   DBAR(vl(ct,sct,J),:) = CO2JKLM(J,5,L,2:NM)
	END DO
	DBAR(vl(ct,sct,99),:) = SUM(CO2JKLM(1:3,5,L,2:NM),DIM=1)
	sct = 16 ! from H2 Production
	sctstr(ct,sct) = 'H2 Prod'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','MMT')
 	IF(vp)CALL ivid(0,ct,sct,2,'Gas','MMT')
 	IF(vp)CALL ivid(0,ct,sct,3,'Coal','MMT')
 	IF(vp)CALL ivid(0,ct,sct,99,'Total','MMT')
      DO J=1,3
	   DBAR(vl(ct,sct,J),:) = CO2JKLM(J,6,L,2:NM)
	END DO
	DBAR(vl(ct,sct,99),:) = SUM(CO2JKLM(1:3,6,L,2:NM),DIM=1)

!	CARBON SEQUESTRATION (already subtracted from above emissions figures)
!	** NOTE ** double check sequestered carbon from H2 production
	sct =20
	sctstr(ct,sct) = 'CO2 Seq'
      IF(vp)CALL ivid(0,ct,sct,1,'Elec','MMtC')
      IF(vp)CALL ivid(0,ct,sct,2,'Synfuel','MMtC')
      IF(vp)CALL ivid(0,ct,sct,3,'H2 Prod','MMtC')
      IF(vp)CALL ivid(0,ct,sct,99,'Total','MMtC')
	DBAR(vl(ct,sct,1),:) = -CARBSEQ(2,L,2:NM)
	DBAR(vl(ct,sct,2),:) = -CARBSEQ(1,L,2:NM)
	DBAR(vl(ct,sct,3),:) = -CARBSEQ(3,L,2:NM)
	DBAR(vl(ct,sct,99),:) = -SUM(CARBSEQ(1:3,L,2:NM),DIM=1)

!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	ct = 11  ! other emiss
	ctstr(ct) = 'AllEmiss'


	sct = 01
	sctstr(ct,sct) = 'All Anthro.Gas '

	IF(vp)CALL ivid(0,ct,sct,0,'CO2','TgGas')
	DBAR(vl(ct,sct,0),:) = CO2DLM(L,2:NM) * 1/CO2toC
	DO I = 1, NOGMax
		IF (SUM(OGREPORT(I,:)).EQ.0) CYCLE  ! if gas has no sources then skip
		IF(vp)CALL ivid(0,ct,sct,I,OGLabel(I),'TgGas')
		DBAR(vl(ct,sct,I),:) = SUM(OGEMISS(I,:,L,2:NM),DIM=1)
	END DO

	sct = 02
	sctstr(ct,sct) = 'All Anthro.CE '

	IF(vp)CALL ivid(0,ct,sct,0,'CO2','MMTC')
	DBAR(vl(ct,sct,0),:) = CO2DLM(L,2:NM)
	DO I = 1, NOGMax
		IF (SUM(OGREPORT(I,:)).EQ.0) CYCLE  ! if gas has no sources then skip
		IF(vp)CALL ivid(0,ct,sct,I,OGLabel(I),'MMTCE')
		IF (OGGWP(I,2).EQ.0) CYCLE  ! if Index is zero, then no equiv defined, so skip
		DBAR(vl(ct,sct,I),:) = SUM(OGEMISS(I,:,L,2:NM),DIM=1)*OGGWP(I,2:NM)*CO2toC
	END DO

	IF(vp)CALL ivid(0,ct,sct,99,'GHG Total','MMTCE')
	DBAR(vl(ct,sct,99),:) = &
	CO2DLM(L,2:NM) + SUM(SUM(OGEMISS(:,:,L,2:NM),DIM=2)*OGGWP(:,2:NM),DIM=1)*CO2toC
	
	


!	SULFUR EMISSIONS, TOTAL BY SECTOR
	sct = 20
	sctstr(ct,sct) = 'SO2 Total'
      IF(vp)CALL ivid(0,ct,sct,1,'Bld','TgS')
      IF(vp)CALL ivid(0,ct,sct,2,'Ind','TgS')
      IF(vp)CALL ivid(0,ct,sct,3,'Trn','TgS')
      IF(vp)CALL ivid(0,ct,sct,4,'Elec','TgS')
      IF(vp)CALL ivid(0,ct,sct,11,'Ind Proc','TgS')
      IF(vp)CALL ivid(0,ct,sct,12,'Trad Bio','TgS')
      IF(vp)CALL ivid(0,ct,sct,13,'Bio Burn','TgS')
!      IF(vp)CALL ivid(0,ct,sct,14,'Elec Bio','TgS')
      IF(vp)CALL ivid(0,ct,sct,99,'Total','TgS')
	DBAR(vl(ct,sct,1),:) = SUM(SO2EMSJLM(1,:,L,2:NM),DIM=1)
	DBAR(vl(ct,sct,2),:) = SUM(SO2EMSJLM(2,:,L,2:NM),DIM=1)
	DBAR(vl(ct,sct,3),:) = SUM(SO2EMSJLM(3,:,L,2:NM),DIM=1)
	DBAR(vl(ct,sct,4),:) = SUM(SO2EMSJLM(4,:,L,2:NM),DIM=1)
	DBAR(vl(ct,sct,11),:) = SO2INDPROC(L,2:NM)
	DBAR(vl(ct,sct,12),:) = SO2TRBIO(L,2:NM)
	DBAR(vl(ct,sct,13),:) = SO2BIOBURN(L,2:NM)
!	DBAR(vl(ct,sct,14),:) = SO2ELECBIO(L,2:NM)	! No longer used
	DBAR(vl(ct,sct,99),:) = SO2EM(L,2:NM)

!	SO2 SEC DETAIL EMS
	sct = 21
	sctstr(ct,sct) = 'SO2 Detail'
	DO ISEC = 1,4
         DO JJ = 1,4	! sjs -- added biomass
	      vidx = ISEC*10+JJ
            IF(vp)CALL ivid(0,ct,sct,vidx, &
              (SO2SECTL(ISEC)//SO2FUELL(JJ)),'TgS')
	      DBAR(vl(ct,sct,vidx),:) = SO2EMSJLM(ISEC,JJ,L,2:NM)
	   END DO
	END DO
!	SO2 SEC DETAIL CONTRLFRAC
	sct = 22
	sctstr(ct,sct) = 'SO2 Cntl Frac'
	DO ISEC = 1,4
         DO JJ = 1,4	! Add biomass -- sjs 10/00
	      vidx = ISEC*10+JJ
            IF(vp)CALL ivid(0,ct,sct,vidx, &
              (SO2SECTL(ISEC)//SO2FUELL(JJ)),'%')
	      DBAR(vl(ct,sct,vidx),:) = SO2CTRL(ISEC,JJ,L,2:NM)
	   END DO
	END DO


!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	ct = 12  ! other gases (common code reporting)
	ctstr(ct) = 'OG Detail'


	DO I = 1, NOGMax
		IF (SUM(OGREPORT(I,:)).EQ.0) CYCLE  ! if gas has no sources then skip
		sct = I
		sctstr(ct,sct) = TRIM(OGLabel(I))//' Emiss '
		DO II = 1, NOGSrcMax
			IF (OGREPORT(I,II).EQ.0) CYCLE  ! skip empty sources
			IF(vp)CALL ivid(0,ct,sct,II,OGSrcLabel(I,II),'TgGas')
			DBAR(vl(ct,sct,II),:) = OGEMISS(I,II,L,2:NM)
		END DO
	END DO

	DO I = 1, NOGMax
		IF (SUM(OGREPORT(I,:)).EQ.0) CYCLE  ! if gas has no sources then skip
		IF (OGGWP(I,2).EQ.0) CYCLE  ! if Index is zero, then no equiv defined, so skip
		sct = 25+I
		sctstr(ct,sct) = TRIM(OGLabel(I))//' CEEmiss '
		DO II = 1, NOGSrcMax
			IF (OGREPORT(I,II).EQ.0) CYCLE  ! skip empty sources
			IF(vp)CALL ivid(0,ct,sct,II,OGSrcLabel(I,II),'MMTCE')
			DBAR(vl(ct,sct,II),:) = OGEMISS(I,II,L,2:NM)*OGGWP(I,2:NM)*CO2toC
		END DO
	END DO


	DO I = 1, NOGMax
		IF (SUM(OGREPORT(I,:)).EQ.0) CYCLE  ! if gas has no sources then skip
		sct = 50+I
		sctstr(ct,sct) = TRIM(OGLabel(I))//' ActLvl '
		DO II = 1, NOGSrcMax
			IF (OGREPORT(I,II).EQ.0) CYCLE  ! skip empty sources
			IF(vp)CALL ivid(0,ct,sct,II,OGSrcLabel(I,II),'-')
			DBAR(vl(ct,sct,II),:) = OGACT(I,II,L,2:NM)
		END DO
	END DO


	DO I = 1, NOGMax
		IF (SUM(OGREPORT(I,:)).EQ.0) CYCLE  ! if gas has no sources then skip
		sct = 75+I
		sctstr(ct,sct) = TRIM(OGLabel(I))//' Ctrl '
		DO II = 1, NOGSrcMax
			IF (OGREPORT(I,II).EQ.0) CYCLE  ! skip empty sources
			IF(vp)CALL ivid(0,ct,sct,   II,OGSrcLabel(I,II),'gdp%')
			IF(vp)CALL ivid(0,ct,sct,30+II,OGSrcLabel(I,II),'abt%')
			IF(vp)CALL ivid(0,ct,sct,60+II,OGSrcLabel(I,II),'usr%')
			DBAR(vl(ct,sct,II),:)    = OGCTRLS(I,II,1,L,2:NM)
			DBAR(vl(ct,sct,30+II),:) = OGCTRLS(I,II,2,L,2:NM)
			DBAR(vl(ct,sct,60+II),:) = OGCTRLS(I,II,3,L,2:NM)
		END DO
	END DO



!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	ct = 15  ! general and misc
	ctstr(ct) = 'General'
!	GDP and POPULATION
	sct = 01
	sctstr(ct,sct) = 'GDP 90$'
	IF(vp)CALL ivid(0,ct,sct,10,'GDP(90ppp)','B90US$')
	IF(vp)CALL ivid(0,ct,sct,11,'GDP(90mer)','B90US$')
	DBAR(vl(ct,sct,10),:) = GNPPPP(L,2:NM)
	DBAR(vl(ct,sct,11),:) = GNPMRKT(L,2:NM)
	sct = 02
	sctstr(ct,sct) = 'GDP 97$'
	IF(vp)CALL ivid(0,ct,sct,10,'GDP(97ppp)','B97US$')
	IF(vp)CALL ivid(0,ct,sct,11,'GDP(97mer)','B97US$')
	DBAR(vl(ct,sct,10),:) = GNPPPP(L,2:NM)*(CVRT97/CVRT90)
	DBAR(vl(ct,sct,11),:) = GNPMRKT(L,2:NM)*(CVRT97/CVRT90)
	sct = 03
	sctstr(ct,sct) = 'Population'
	IF(vp)CALL ivid(0,ct,sct,10,'Total','thous')
	IF(vp)CALL ivid(0,ct,sct,11,'WageM','thous')
	IF(vp)CALL ivid(0,ct,sct,12,'WageF','thous')
	IF(vp)CALL ivid(0,ct,sct,13,'WageTot','thous')
	DBAR(vl(ct,sct,10),:) = ZLM(L,2:NM)
	DBAR(vl(ct,sct,11),:) = WAgeMC(1,L,2:NM)
	DBAR(vl(ct,sct,12),:) = WAgeMC(2,L,2:NM)
	DBAR(vl(ct,sct,13),:) = WAgeMC(1,L,2:NM)+WAgeMC(2,L,2:NM)

!	CARBON TAX
	sct = 05
	sctstr(ct,sct) = 'Carbon Price'
	IF(vp)CALL ivid(0,ct,sct,1,'Fos Fuel','90US$/TC')
	IF(vp)CALL ivid(0,ct,sct,2,'Fos Fuel','97US$/TC')
	IF(vp)CALL ivid(0,ct,sct,3,'TaxableEms','MMTCE')
	DBAR(vl(ct,sct,1),:) = TAXLM(L,2:NM)
	DBAR(vl(ct,sct,2),:) = TAXLM(L,2:NM)*(CVRT97/CVRT90)
	DBAR(vl(ct,sct,3),:) = EMISSTCE(L,2:NM)

!	MISCELLANEOUS
	sct = 10
	sctstr(ct,sct) = 'Misc'
  	IF(vp)CALL ivid(GenRelease,ct,sct,1,'Energy Serv','EJ/yr')
	DBAR(vl(ct,sct,1),:) = ESERV(L,2:NM)
 	IF(vp)CALL ivid(GenRelease,ct,sct,2,'Bld-Energy Serv','EJ/yr')
 	IF(vp)CALL ivid(GenRelease,ct,sct,3,'Ind-Energy Ser','EJ/yr')
 	IF(vp)CALL ivid(GenRelease,ct,sct,4,'Trans-Energy Serv','EJ/yr')
	DBAR(vl(ct,sct,2),:) = ESERVK(1,L,2:NM)
	DBAR(vl(ct,sct,3),:) = ESERVK(2,L,2:NM)
	DBAR(vl(ct,sct,4),:) = ESERVK(3,L,2:NM)

!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	ct = 20  ! primary energy quantities
	ctstr(ct) = 'Pri Energy'
!	CONSUMPTION
	sct = 01
	sctstr(ct,sct) = 'Consumption'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,4,'Biomass','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,5,'Hydro','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,6,'Nuclear','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,7,'Solar','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,8,'Fusion','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,9,'Wind','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,10,'SWStor','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,11,'SatSolar','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,99,'Total','EJ/yr')
	DBAR(vl(ct,sct,1),:) = EDILM(INOIL,L,2:NM)
	DBAR(vl(ct,sct,2),:) = EDILM(INGAS,L,2:NM)
	DBAR(vl(ct,sct,3),:) = (ESILM(INCOAL,L,2:NM)+EXILM(INCOAL,L,2:NM))
	DBAR(vl(ct,sct,4),:) = EDILM(IBMASS,L,2:NM)
	DBAR(vl(ct,sct,5),:) = ESILM(6,L,2:NM)
	DBAR(vl(ct,sct,6),:) = ESILM(4,L,2:NM)
	DBAR(vl(ct,sct,7),:) = ESILM(5,L,2:NM)
	DBAR(vl(ct,sct,8),:) = ESILM(JUFUSION,L,2:NM)
	DBAR(vl(ct,sct,9),:) = ESILM(JUWIND,L,2:NM)
	DBAR(vl(ct,sct,10),:) = ESILM(JUWIND+1,L,2:NM)
	DBAR(vl(ct,sct,10),:) = ESILM(JUWIND+2,L,2:NM)
	itmp = vl(ct,sct,99)
	DBAR(itmp,:) = SUM(DBAR(itmp-11:itmp-1,:),DIM=1)
!	PRODUCTION
	sct = 05
	sctstr(ct,sct) = 'Production'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,4,'Biomass','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,5,'Dir Electric','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,99,'SubTotal','EJ/yr')
	DBAR(vl(ct,sct,1),:) = ESILM(INOIL,L,2:NM)
	DBAR(vl(ct,sct,2),:) = ESILM(INGAS,L,2:NM)
	DBAR(vl(ct,sct,3),:) = ESILM(INCOAL,L,2:NM)
	DBAR(vl(ct,sct,4),:) = ESILM(IBMASS,L,2:NM)
	DBAR(vl(ct,sct,5),:) = SUM(ESILM(4:6,L,2:NM),DIM=1)+ESILM(JUFUSION,L,2:NM) &
             +ESILM(JUWIND,L,2:NM)+ESILM(JUWIND+1,L,2:NM)+ESILM(JUWIND+2,L,2:NM)
	itmp = vl(ct,sct,99)
	DBAR(itmp,:) = SUM(DBAR(itmp-5:itmp-1,:),DIM=1)

IF (GenRelease .ne. 1) THEN
!	TRADE
	sct = 10
	sctstr(ct,sct) = 'Trade'
	IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,4,'Biomass','EJ/yr')
	IF(vp)CALL ivid(0,ct,sct,99,'Total','EJ/yr')
	DBAR(vl(ct,sct,1),:) = EXILM(INOIL,L,2:NM)
	DBAR(vl(ct,sct,2),:) = EXILM(INGAS,L,2:NM)
	DBAR(vl(ct,sct,3),:) = EXILM(INCOAL,L,2:NM)
	DBAR(vl(ct,sct,4),:) = EXILM(IBMASS,L,2:NM)
	itmp = vl(ct,sct,99)
	DBAR(itmp,:) = SUM(DBAR(itmp-4:itmp-1,:),DIM=1)
END IF

!     COMPONENTS of Crude Oil Production
	sct = 19
	sctstr(ct,sct) = 'C.Oil Source'
      IF(vp)CALL ivid(GenRelease,ct,sct,1,'Conventional','EJ/yr')
      IF(vp)CALL ivid(GenRelease,ct,sct,2,'Shale','EJ/yr')
      IF(vp)CALL ivid(GenRelease,ct,sct,99,'Total','EJ/yr')
	DBAR(vl(ct,sct,1),:) = ESIL1M(1,L,2:NM)
	DBAR(vl(ct,sct,2),:) = ESIL2M(1,L,2:NM)
	DBAR(vl(ct,sct,99),:) = ESIL1M(1,L,2:NM)+ESIL2M(1,L,2:NM)

!     COMPONENTS of Biomass
	sct = 20
	sctstr(ct,sct) = 'Biomass Sources'
      IF(vp)CALL ivid(0,ct,sct,1,'Modern','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Waste & Trad','EJ/yr')
	DBAR(vl(ct,sct,1),:) = ESIL1M(IBMASS,L,2:NM)
	DBAR(vl(ct,sct,2),:) = ESIL2M(IBMASS,L,2:NM)

!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	ct = 21  ! final energy
	ctstr(ct) = 'Final Energy'

!	TOTALS
	sct = 01
	sctstr(ct,sct) = 'by Sector'
      IF(vp)CALL ivid(0,ct,sct,1,'Bld','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Ind','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Tran','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,99,'Total','EJ/yr')
	DBAR(vl(ct,sct,1),:) = EFKLM(2,L,2:NM)
	DBAR(vl(ct,sct,2),:) = EFKLM(3,L,2:NM)
	DBAR(vl(ct,sct,3),:) = EFKLM(4,L,2:NM)
	DBAR(vl(ct,sct,99),:) = EFLM(L,2:NM)

!	SECTOR DETAILS  uses scat 11,12,13
	sctstr(ct,11) = 'Buildings'
	sctstr(ct,12) = 'Industries'
	sctstr(ct,13) = 'Transport'
	DO ISEC = 1, 3
	   sct = 10+ISEC
	   DO JFUEL = 1, NNJ
            IF(vp)CALL ivid(0,ct,sct,JFUEL,FUELL(JFUEL),'EJ/yr')
	      DBAR(vl(ct,sct,JFUEL),:) = FJKLM(JFUEL,ISEC,L,2:NM)
	   END DO
	   IF(vp)CALL ivid(0,ct,sct,99,'Total','EJ/yr')
	   DBAR(vl(ct,sct,99),:) = SUM(FJKLM(1:NNJ,ISEC,L,2:NM),DIM=1)
	END DO
	ISEC = 4	! Totals - sjs
	sct = 10+ISEC
	sctstr(ct,sct) = 'Total-By Fuel'
	   DO JFUEL = 1, NNJ
            IF(vp)CALL ivid(0,ct,sct,JFUEL,FUELL(JFUEL),'EJ/yr')
	      DBAR(vl(ct,sct,JFUEL),:) = SUM(FJKLM(JFUEL,1:3,L,2:NM),DIM=1)
	   END DO
	   IF(vp)CALL ivid(0,ct,sct,99,'Total','EJ/yr')
	   DBAR(vl(ct,sct,99),:) = SUM(SUM(FJKLM(1:NNJ,:,L,2:NM),DIM=1),DIM=1)

IF (GenRelease .ne. 1) THEN
!	SECTORAL ENERGY OUT/IN  uses scat 21,22,23
	sctstr(ct,21) = 'Bld Ser Eff'
	sctstr(ct,22) = 'Ind Ser Eff'
	sctstr(ct,23) = 'Trn Ser Eff'
	DO ISEC = 1, 3
	   sct = 20+ISEC
	   DO JFUEL = 1, NNJ
            IF(vp)CALL ivid(0,ct,sct,JFUEL,FUELL(JFUEL),'Out/In')
	      DBAR(vl(ct,sct,JFUEL),:) = 1/GJKLM(JFUEL,ISEC,L,2:NM)
	   END DO
	END DO
END IF

!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	ct = 25  ! electric power
	ctstr(ct) = 'Electric Pwr'
!	GENERATION BY FUEL
	sct = 01
	sctstr(ct,sct) = 'Generation'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Nuclear','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,5,'Solar','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,6,'Hydro','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,7,'Biomass','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,8,'CoalCap','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,9,'OilCap','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,10,'GasCap','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,11,'H2Fcell','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,12,'Fusion','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,13,'Wind','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,14,'SWStor','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,15,'SatSolar','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,99,'Total','EJ/yr')
	DO I=1,NNU+NStype
	   DBAR(vl(ct,sct,I),:) = ESUILM(I,L,2:NM)
	END DO
	DBAR(vl(ct,sct,99),:) = SUM(ESUILM(1:NNU+NStype,L,2:NM),DIM=1)

!	GENERATION EFFICIENCY
	sct = 05
	sctstr(ct,sct) = 'Efficiency'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','Out/In')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','Out/In')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','Out/In')
      IF(vp)CALL ivid(0,ct,sct,4,'Nuclear','Out/In')
      IF(vp)CALL ivid(0,ct,sct,5,'Solar','Out/In')
      IF(vp)CALL ivid(0,ct,sct,6,'Hydro','Out/In')
      IF(vp)CALL ivid(0,ct,sct,7,'Biomass','Out/In')
      IF(vp)CALL ivid(0,ct,sct,8,'CoalCap','Out/In')
      IF(vp)CALL ivid(0,ct,sct,9,'OilCap','Out/In')
      IF(vp)CALL ivid(0,ct,sct,10,'GasCap','Out/In')
      IF(vp)CALL ivid(0,ct,sct,11,'H2Fcell','Out/In')
      IF(vp)CALL ivid(0,ct,sct,12,'Fusion','Out/In')
      IF(vp)CALL ivid(0,ct,sct,13,'Wind','Out/In')
	DO I=1,NNU
	   DBAR(vl(ct,sct,I),:) = 1/GUILM(I,L,2:NM)
	END DO

!	FUEL CONSUMPTION
	sct = 10
	sctstr(ct,sct) = 'Fuel Cons'
!	note - we could change this to use esuilm and guilm instead of edriklm
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,7,'Biomass','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,8,'CoalCap','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,9,'OilCap','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,10,'GasCap','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,11,'H2','EJ/yr')
	DBAR(vl(ct,sct,1),:) = EDRIKLM(INOIL,1,L,2:NM)
	DBAR(vl(ct,sct,2),:) = EDRIKLM(INGAS,1,L,2:NM)
	DBAR(vl(ct,sct,3),:) = EDRIKLM(INCOAL,1,L,2:NM)
	DBAR(vl(ct,sct,7),:) = EDRIKLM(IBMASS,1,L,2:NM)
	DBAR(vl(ct,sct,8),:) = ESUILM(JUCSCRUB,L,2:NM)* &
      GUILM(JUCSCRUB,L,2:NM)
	DBAR(vl(ct,sct,9),:) = ESUILM(JUOSCRUB,L,2:NM)* &
      GUILM(JUOSCRUB,L,2:NM)
	DBAR(vl(ct,sct,10),:) = ESUILM(JUGSCRUB,L,2:NM)* &
      GUILM(JUGSCRUB,L,2:NM)
	DBAR(vl(ct,sct,11),:) = ESUILM(JUH2GEN,L,2:NM)* &
      GUILM(JUH2GEN,L,2:NM)

!     ***************************
!     Placed here to get primary energy and electricity totals
	ct = 15  ! general and misc
	ctstr(ct) = 'General'
!	PERCAP DATA
	sct = 20
	sctstr(ct,sct) = 'Per Cap Summary'
	IF(vp)CALL ivid(0,ct,sct,10,'GDP(mer)/cap','90US$/per')
	DBAR(vl(ct,sct,10),:) = (GNPMRKT(L,2:NM)/ZLM(L,2:NM))*1.0E6
	IF(vp)CALL ivid(0,ct,sct,20,'CO2/cap','Tonnes/per')
	DBAR(vl(ct,sct,20),:) = (CO2DLM(L,2:NM)/ZLM(L,2:NM))*1.0E3
	IF(vp)CALL ivid(0,ct,sct,30,'PEC/cap','GJ/per')
	DBAR(vl(ct,sct,30),:) = DBAR(vl(20,01,99),:)/ZLM(L,2:NM)*1.0E6
	IF(vp)CALL ivid(0,ct,sct,40,'Elec/cap','GJ/per')
	DBAR(vl(ct,sct,40),:) = DBAR(vl(25,01,99),:)/ZLM(L,2:NM)*1.0E6

!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	ct = 30  ! refining
	ctstr(ct) = 'Refining'
!	FUEL CONS H2 PROD
	sct = 01
	sctstr(ct,sct) = 'H2Prod FuelIn'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Biomass','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,5,'Electricity','EJ/yr')
	DBAR(vl(ct,sct,1),:) = EDRIKLM(INOIL,KH2,L,2:NM)
	DBAR(vl(ct,sct,2),:) = EDRIKLM(INGAS,KH2,L,2:NM)
	DBAR(vl(ct,sct,3),:) = EDRIKLM(INCOAL,KH2,L,2:NM)
	DBAR(vl(ct,sct,4),:) = EDRIKLM(IBMASS,KH2,L,2:NM)
	DBAR(vl(ct,sct,5),:) = ESHILM(JHELCTRO,L,2:NM)*GHILM(JHELCTRO,2:NM)

!	H2 PROD BY FUEL
	sct = 02
	sctstr(ct,sct) = 'H2 Prod'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Biomass','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,5,'Electrolysis','EJ/yr')     
      IF(vp)CALL ivid(0,ct,sct,6,'New H2 tech 1','EJ/yr')     
      IF(vp)CALL ivid(0,ct,sct,7,'New H2 tech 2','EJ/yr')     
      IF(vp)CALL ivid(0,ct,sct,9,'Total','EJ/yr')     
	DBAR(vl(ct,sct,1),:) =  &
                       ESHILM(INOIL,L,2:NM)+ESHILM(INOIL+NH2,L,2:NM)	! Scrubbed + non scrubbed
	DBAR(vl(ct,sct,2),:) =  &
                       ESHILM(INGAS,L,2:NM)+ESHILM(INGAS+NH2,L,2:NM)
	DBAR(vl(ct,sct,3),:) =  &
                       ESHILM(INCOAL,L,2:NM)+ESHILM(INCOAL+NH2,L,2:NM)
	DBAR(vl(ct,sct,4),:) = ESHILM(4,L,2:NM)
	DBAR(vl(ct,sct,5),:) = ESHILM(5,L,2:NM)
	DBAR(vl(ct,sct,6),:) = ESHILM(NNH2+1,L,2:NM)
	DBAR(vl(ct,sct,7),:) = ESHILM(NNH2+2,L,2:NM)
	DBAR(vl(ct,sct,9),:) =  &
                       ESHILM(INOIL,L,2:NM)+ESHILM(INOIL+NH2,L,2:NM)+ &
                       ESHILM(INGAS,L,2:NM)+ESHILM(INGAS+NH2,L,2:NM)+ &
                       ESHILM(INCOAL,L,2:NM)+ESHILM(INCOAL+NH2,L,2:NM)+ &
                       ESHILM(4,L,2:NM)+&
                       ESHILM(5,L,2:NM)+ESHILM(NNH2+1,L,2:NM)+ESHILM(NNH2+2,L,2:NM)

!	FUEL INPUT TO SYNFUEL PROD
	sct = 10
	sctstr(ct,sct) = 'SynProd FuelIn'
      IF(vp)CALL ivid(0,ct,sct,1,'Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Coal','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Biomass','EJ/yr')
	DBAR(vl(ct,sct,1),:) = SYNINPUT(2,L,2:NM)
	DBAR(vl(ct,sct,2),:) = SYNINPUT(3,L,2:NM)
	DBAR(vl(ct,sct,3),:) = SYNINPUT(4,L,2:NM)

!	SYNFUEL PROD
	sct = 11
	sctstr(ct,sct) = 'SynfuelProd'
      IF(vp)CALL ivid(0,ct,sct,1,'Gas_Liq','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Coal_Liq','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Bmas_Liq','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Coal_Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,5,'Bmas_Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,10,'SynGas Tot','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,11,'SynLiq Tot','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,12,'SynGasLoss Tot','EJ/yr')	! E loss from synfuel production
      IF(vp)CALL ivid(0,ct,sct,13,'SynLiqLoss Tot','EJ/yr')
	DBAR(vl(ct,sct,1),:) = SYNFUEL(2,1,L,2:NM)
	DBAR(vl(ct,sct,2),:) = SYNFUEL(3,1,L,2:NM)
	DBAR(vl(ct,sct,3),:) = SYNFUEL(4,1,L,2:NM)
	DBAR(vl(ct,sct,4),:) = SYNFUEL(3,2,L,2:NM)
	DBAR(vl(ct,sct,5),:) = SYNFUEL(4,2,L,2:NM)
	DBAR(vl(ct,sct,10),:) = SUM(SYNFUEL(3:4,2,L,2:NM),DIM=1)
	DBAR(vl(ct,sct,11),:) = SUM(SYNFUEL(2:4,1,L,2:NM),DIM=1)
	DBAR(vl(ct,sct,12),:) = SUM(SYNLoss(:,2,L,2:NM),DIM=1)
	DBAR(vl(ct,sct,13),:) = SUM(SYNLoss(:,1,L,2:NM),DIM=1)

!	SYNFUEL PROD EFF (%)
	sct = 12
	sctstr(ct,sct) = 'SynProd Eff'
      IF(vp)CALL ivid(0,ct,sct,1,'Gas_Liq','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Coal_Liq','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Bmas_Liq','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Coal_Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,5,'Bmas_Gas','EJ/yr')
	DBAR(vl(ct,sct,1),:) = 1/GCI(2,1)
	DBAR(vl(ct,sct,2),:) = 1/GCI(3,1)
	DBAR(vl(ct,sct,3),:) = 1/GCI(4,1)
	DBAR(vl(ct,sct,4),:) = 1/GCI(3,2)
	DBAR(vl(ct,sct,5),:) = 1/GCI(4,2)
      
!	Carb Seq by fuel and processes
	sct = 13
	sctstr(ct,sct) = 'CarbSeqDetail'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil_elec','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas_elec','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal_elec','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Coal_Synf','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,5,'Oil_H2','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,6,'Gas_H2','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,7,'Coal_H2','EJ/yr')
 	DBAR(vl(ct,sct,1),:) = CSEQbyFuel(1,1,L,2:NM)
	DBAR(vl(ct,sct,2),:) = CSEQbyFuel(1,2,L,2:NM)
	DBAR(vl(ct,sct,3),:) = CSEQbyFuel(1,3,L,2:NM)
	DBAR(vl(ct,sct,4),:) = CSEQbyFuel(2,3,L,2:NM)
	DBAR(vl(ct,sct,5),:) = CSEQbyFuel(3,1,L,2:NM)
	DBAR(vl(ct,sct,6),:) = CSEQbyFuel(3,2,L,2:NM)
	DBAR(vl(ct,sct,7),:) = CSEQbyFuel(3,3,L,2:NM)

!	Total conversion losses by fuel
	sct = 18
	sctstr(ct,sct) = 'ConvLosses'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Biomass','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,5,'H2->Elec','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,6,'Elec->H2','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,7,'Total','EJ/yr')
 	DBAR(vl(ct,sct,1),:) =	(EDRIKLM(INOIL,1,L,2:NM)-ESUILM(INOIL,L,2:NM)) + &	! elec gen
 							(ESUILM(JUOSCRUB,L,2:NM)*GUILM(JUOSCRUB,L,2:NM)-(ESUILM(JUOSCRUB,L,2:NM))) + &	! elec gen - scrubbed
  							(EDRIKLM(INOIL,KH2,L,2:NM)-(ESHILM(INOIL,L,2:NM)+ESHILM(INOIL+NH2,L,2:NM))) 	! H2 gen
 	DBAR(vl(ct,sct,2),:) =	(EDRIKLM(INGAS,1,L,2:NM)-ESUILM(INGAS,L,2:NM)) + &	! elec gen
 							(ESUILM(JUGSCRUB,L,2:NM)*GUILM(JUGSCRUB,L,2:NM)-(ESUILM(JUGSCRUB,L,2:NM))) + &	! elec gen - scrubbed
 							(SYNINPUT(INGAS,L,2:NM)-SYNFUEL(INGAS,1,L,2:NM)) + &	! syn gas-liq
  							(EDRIKLM(INGAS,KH2,L,2:NM)-(ESHILM(INGAS,L,2:NM)+ESHILM(INGAS+NH2,L,2:NM))) 	! H2 gen
 	DBAR(vl(ct,sct,3),:) =	(EDRIKLM(INCOAL,1,L,2:NM)-ESUILM(INCOAL,L,2:NM)) + &	! elec gen
							(ESUILM(JUCSCRUB,L,2:NM)*GUILM(JUCSCRUB,L,2:NM)-(ESUILM(JUCSCRUB,L,2:NM))) + &	! elec gen - scrubbed
  							(SYNINPUT(INCOAL,L,2:NM)-(SYNFUEL(3,1,L,2:NM)+SYNFUEL(3,2,L,2:NM))) + &	! syn COAL-gas&liq
  							(EDRIKLM(INCOAL,KH2,L,2:NM)-(ESHILM(INCOAL,L,2:NM)+ESHILM(INCOAL+NH2,L,2:NM))) 	! H2 gen
	DBAR(vl(ct,sct,4),:) =	(EDRIKLM(IBMASS,1,L,2:NM)-ESUILM(7,L,2:NM)) + &	! elec gen
 							(SYNINPUT(IBMASS,L,2:NM)-(SYNFUEL(4,1,L,2:NM)+SYNFUEL(4,2,L,2:NM))) + &	! syn Biomass-gas&liq
  							(EDRIKLM(IBMASS,KH2,L,2:NM)-ESHILM(IBMASS,L,2:NM)) 	! H2 gen
	DBAR(vl(ct,sct,5),:) =	(EDRIKLM(JUH2GEN,1,L,2:NM)-ESUILM(11,L,2:NM))	! Elec gen from H2
	DBAR(vl(ct,sct,6),:) =	ESHILM(JHELCTRO,L,2:NM)*(GHILM(JHELCTRO,2:NM)-1.d0)	! H2 gen by Electrolysis
	DBAR(vl(ct,sct,7),:) =	DBAR(vl(ct,sct,1),:) + DBAR(vl(ct,sct,2),:) + DBAR(vl(ct,sct,3),:) + &
							DBAR(vl(ct,sct,4),:) + DBAR(vl(ct,sct,5),:) + DBAR(vl(ct,sct,6),:)
!	REFINERY DEMAND FOR FUELS
	sct = 20
	sctstr(ct,sct) = 'Ref Demand'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Biomass','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,99,'Total','EJ/yr')
	DBAR(vl(ct,sct,1),:) = EDRILM(1,L,2:NM)
	DBAR(vl(ct,sct,2),:) = EDRILM(2,L,2:NM)
	DBAR(vl(ct,sct,3),:) = EDRILM(3,L,2:NM)
	DBAR(vl(ct,sct,4),:) = EDRILM(IBMASS,L,2:NM)
	DBAR(vl(ct,sct,99),:) = EDRLM(L,2:NM)

!	Fossil Production
	sct = 30
	sctstr(ct,sct) = 'Cumulative Production'
      IF(vp)CALL ivid(0,ct,sct,1,'Conv Oil','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,2,'Natural Gas','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','EJ/yr')
      IF(vp)CALL ivid(0,ct,sct,4,'Unconv Oil','EJ/yr')
	DBAR(vl(ct,sct,1),:) = QISLM(1,L,2:NM)
	DBAR(vl(ct,sct,2),:) = QISLM(2,L,2:NM)
	DBAR(vl(ct,sct,3),:) = QISLM(3,L,2:NM)
	DBAR(vl(ct,sct,4),:) = QISLM(4,L,2:NM)

!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	IF (WRITENEWTRAN) THEN
!	****** New Tranportation Module
	ct = 35  !New Transportation Data
	ctstr(ct) = 'Transportation'
!     Transportation Sector Total Fuel Consumption by Mode (Quads)
      DO ISER=1,NTSERV+1
         DO IMODE=1,NTMODE+1
  	      sct = ISER*10+IMODE
	      sctstr(ct,sct) = 'C'//TSERV(ISER)//TMD(IMODE)
            DO J=1,NFUEL+1
               IF(vp)CALL ivid(0,ct,sct,J,TFUEL(J),'Quad')
               DBAR(vl(ct,sct,J),:) = TRANFUEL(ISER,IMODE,J,L,2:NM)
            END DO
         END DO
      END DO	        
!     Transportation Sector Service by Mode and Fuel (Mil Pass-mi/Ton-mi)
      DO ISER=1,NTSERV
         DO IMODE=1,NTMODE+1
  	      sct = 30+ISER*10+IMODE
	      sctstr(ct,sct) = 'S'//TSERV(ISER)//TMD(IMODE)
	      DO J=1,NFUEL+1
               IF(vp)CALL ivid(0,ct,sct,J,TFUEL(J),TUNIT(ISER))
               DBAR(vl(ct,sct,J),:) = TRANSERV(ISER,IMODE,J,L,2:NM)
	      END DO
	   END DO
	END DO
!     Transportation Sector Energy Intensity by Mode (Btu/Veh-mi)
      DO ISER=1,NTSERV
         DO IMODE=1,NTMODE
  	      sct = 60+ISER*10+IMODE
	      sctstr(ct,sct) = 'I'//TSERV(ISER)//TMD(IMODE)
	      DO J=1,NFUEL
               IF(vp)CALL ivid(0,ct,sct,J,TFUEL(J),'Btu/Veh-mi')
               DBAR(vl(ct,sct,J),:) = TRANEFF(ISER,IMODE,J,L,2:NM)
	      END DO
	   END DO
      END DO	        
	END IF !end toggle on new tran output
!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	ct = 40  ! prices
	ctstr(ct) = 'Prices'
!	GLOBAL PRIMARY FUEL PRICES
	sct = 01
	sctstr(ct,sct) = 'Global 90$'
	TempPriceUnit = '90US$/GJ'
	IF (GenRelease .eq. 1) TempPriceUnit = 'index'
	
	IF(vp)CALL ivid(0,ct,sct,1,'C.Oil',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,2,'N.Gas',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,3,'Coal',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,4,'Biomass',TempPriceUnit)
	DBAR(vl(ct,sct,1),:) = (P(INOIL,L,2:NM)*CVRT90)
	DBAR(vl(ct,sct,2),:) = (P(INGAS,L,2:NM)*CVRT90)
	DBAR(vl(ct,sct,3),:) = (P(INCOAL,L,2:NM)*CVRT90)
	DBAR(vl(ct,sct,4),:) = (P(INBMASS,L,2:NM)*CVRT90)

IF (GenRelease .ne. 1) THEN
	sct = 02
	sctstr(ct,sct) = 'Global 97$'
	IF(vp)CALL ivid(0,ct,sct,1,'C.Oil','97US$/bbl')
	IF(vp)CALL ivid(0,ct,sct,2,'N.Gas','97US$/mcf')
	IF(vp)CALL ivid(0,ct,sct,3,'Coal','97US$/tonne')
	IF(vp)CALL ivid(0,ct,sct,4,'Biomass','97US$/GJ')
	DBAR(vl(ct,sct,1),:) = P(INOIL,L,2:NM)*CVRTO*CVRT97
	DBAR(vl(ct,sct,2),:) = P(INGAS,L,2:NM)*CVRTG*CVRT97
	DBAR(vl(ct,sct,3),:) = P(INCOAL,L,2:NM)*CVRTC*CVRT97
	DBAR(vl(ct,sct,4),:) = P(INBMASS,L,2:NM)*CVRT97
END IF

!	REGIONAL PRIMARY FUEL PRICES (transport and tax added)
	sct = 03
	sctstr(ct,sct) = 'Regional 90$'
	TempPriceUnit = '90US$/GJ'
	IF (GenRelease .eq. 1) TempPriceUnit = 'index'

	IF(vp)CALL ivid(0,ct,sct,1,'C.Oil',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,2,'N.Gas',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,3,'Coal',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,4,'Biomass',TempPriceUnit)
      DBAR(vl(ct,sct,1),:) = PILM(INOIL,L,2:NM)*CVRT90
      DBAR(vl(ct,sct,2),:) = PILM(INGAS,L,2:NM)*CVRT90
      DBAR(vl(ct,sct,3),:) = PILM(INCOAL,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,4),:) = PILM(IBMASS,L,2:NM)*CVRT90

IF (GenRelease .ne. 1) THEN
	sct = 04
	sctstr(ct,sct) = 'Regional 97$'
	IF(vp)CALL ivid(0,ct,sct,1,'C.Oil','97US$/bbl')
	IF(vp)CALL ivid(0,ct,sct,2,'N.Gas','97US$/mcf')
	IF(vp)CALL ivid(0,ct,sct,3,'Coal','97US$/tonne')
	IF(vp)CALL ivid(0,ct,sct,4,'Biomass','97US$/GJ')
      DBAR(vl(ct,sct,1),:) = PILM(INOIL,L,2:NM)*CVRTO*CVRT97
      DBAR(vl(ct,sct,2),:) = PILM(INGAS,L,2:NM)*CVRTG*CVRT97
      DBAR(vl(ct,sct,3),:) = PILM(INCOAL,L,2:NM)*CVRTC*CVRT97
	DBAR(vl(ct,sct,4),:) = PILM(IBMASS,L,2:NM)*CVRT97
END IF

!	REGIONAL Prices for Refined/Delivered Fuels
	sct = 05
	sctstr(ct,sct) = 'Reg Rfnd 90$'
	TempPriceUnit = TempPriceUnit
	IF (GenRelease .eq. 1) TempPriceUnit = 'index'

	IF(vp)CALL ivid(0,ct,sct,1,'DlvdFossOil',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,2,'DlvdFossGas',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,3,'DlvdCoal',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,4,'DlvdBiom',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,5,'Gas_Liq',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,6,'Coal_Liq',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,7,'Biom_Liq',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,8,'Coal_Gas',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,9,'Biom_Gas',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,10,'DlvdLiq',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,11,'DlvdGas',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,12,'Elect',TempPriceUnit)
	IF(vp)CALL ivid(0,ct,sct,13,'H2',TempPriceUnit)
	
! Reconstruct aggregate price. Arbitary k
    PP_oil(2:NM) = (PJKLM(INOIL,1,L,2:NM)-HJKLM(INOIL,1,L,2:NM))/GJKLM(INOIL,1,L,2:NM) 
    PP_gas(2:NM) = (PJKLM(INgas,1,L,2:NM)-HJKLM(INgas,1,L,2:NM))/GJKLM(INgas,1,L,2:NM) 
 
      DBAR(vl(ct,sct,1),:) = PJLM(INOIL,L,2:NM)*CVRT90
      DBAR(vl(ct,sct,2),:) = PJLM(INGAS,L,2:NM)*CVRT90
      DBAR(vl(ct,sct,3),:) = PJLM(INCOAL,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,4),:) = PJLM(JSBMASS,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,5),:) = PCJILM(2,1,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,6),:) = PCJILM(3,1,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,7),:) = PCJILM(4,1,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,8),:) = PCJILM(3,2,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,9),:) = PCJILM(4,2,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,10),:) = PP_oil*CVRT90
	DBAR(vl(ct,sct,11),:) = PP_gas*CVRT90
	DBAR(vl(ct,sct,12),:) = (PJLM(4,L,2:NM)+SUM(PUTDKM(1:3,2:NM),DIM=1)/3.)*CVRT90
	DBAR(vl(ct,sct,13),:) = (PJLM(JSH2,L,2:NM)+SUM(PH2TDKM(1:3,2:NM),DIM=1)/3.)*CVRT90

IF (GenRelease .ne. 1) THEN
	sct = 06
	sctstr(ct,sct) = 'Reg Rfnd 97$'
	IF(vp)CALL ivid(0,ct,sct,1,'DlvdFossOil','97US$/gal')
	IF(vp)CALL ivid(0,ct,sct,2,'DlvdFossGas','97US$/mcf')
	IF(vp)CALL ivid(0,ct,sct,3,'DlvdCoal','97US$/tonne')
	IF(vp)CALL ivid(0,ct,sct,4,'DlvdBiom','97US$/GJ')
	IF(vp)CALL ivid(0,ct,sct,5,'Gas_Liq','97US$/gal')
	IF(vp)CALL ivid(0,ct,sct,6,'Coal_Liq','97US$/gal')
	IF(vp)CALL ivid(0,ct,sct,7,'Biom_Liq','97US$/gal')
	IF(vp)CALL ivid(0,ct,sct,8,'Coal_Gas','97US$/mcf')
	IF(vp)CALL ivid(0,ct,sct,9,'Biom_Gas','97US$/mcf')
	IF(vp)CALL ivid(0,ct,sct,10,'DlvdOil','97US$/gal')
	IF(vp)CALL ivid(0,ct,sct,11,'DlvdGas','97US$/mcf')
	IF(vp)CALL ivid(0,ct,sct,12,'Elect','97US$/GJ')
	IF(vp)CALL ivid(0,ct,sct,13,'H2','97US$/GJ')

      DBAR(vl(ct,sct,1),:) = PJLM(INOIL,L,2:NM)*CVRTAG*CVRT97
      DBAR(vl(ct,sct,2),:) = PJLM(INGAS,L,2:NM)*CVRTG*CVRT97
      DBAR(vl(ct,sct,3),:) = PJLM(INCOAL,L,2:NM)*CVRTC*CVRT97
	DBAR(vl(ct,sct,4),:) = PJLM(JSBMASS,L,2:NM)*CVRT97
	DBAR(vl(ct,sct,5),:) = PCJILM(2,1,L,2:NM)*CVRTAG*CVRT97
	DBAR(vl(ct,sct,6),:) = PCJILM(3,1,L,2:NM)*CVRTAG*CVRT97
	DBAR(vl(ct,sct,7),:) = PCJILM(4,1,L,2:NM)*CVRTAG*CVRT97
	DBAR(vl(ct,sct,8),:) = PCJILM(3,2,L,2:NM)*CVRTG*CVRT97
	DBAR(vl(ct,sct,9),:) = PCJILM(4,2,L,2:NM)*CVRTG*CVRT97
	DBAR(vl(ct,sct,10),:) = PP_oil*CVRTAG*CVRT97
	DBAR(vl(ct,sct,11),:) = PP_gas*CVRTG*CVRT97
	DBAR(vl(ct,sct,12),:) = (PJLM(4,L,2:NM)+SUM(PUTDKM(1:3,2:NM),DIM=1)/3.)*CVRT97
	DBAR(vl(ct,sct,13),:) = (PJLM(JSH2,L,2:NM)+SUM(PH2TDKM(1:3,2:NM),DIM=1)/3.)*CVRT97
END IF

IF (GenRelease .eq. 1) THEN	! convert prices to indices
	DO sct = 01,06
	   DO II = 1,14
	       DBAR(vl(ct,sct,II),:) = DBAR(vl(ct,sct,II),:)/DBAR(vl(ct,sct,II),2)
	   END DO
	END DO
END IF

!	Reg Prices for Delivered Fuels for Electric Gen (1990US$/GJ)
	sct = 07
	sctstr(ct,sct) = 'Dlvd Elec'
      DO I=1,NF
 	   IF(vp)CALL ivid(0,ct,sct,I,FUELL(I),'90US$/GJ')
         DBAR(vl(ct,sct,I),:) = PJLM(I,L,2:NM)*PAUIL(I,L)*CVRT90
      END DO	        
!	Reg Prices for Delivered Fuels for Electric Gen (1997US$/bbl)
 	IF(vp)CALL ivid(0,ct,sct,11,'Oil','97US$/gal')
 	IF(vp)CALL ivid(0,ct,sct,12,'Gas','97US$/mcf')
 	IF(vp)CALL ivid(0,ct,sct,13,'Coal','97US$/tonne')
      DBAR(vl(ct,sct,11),:) = PJLM(INOIL,L,2:NM)*PAUIL(INOIL,L)*CVRTAG* &
      CVRT97
      DBAR(vl(ct,sct,12),:) = PJLM(INGAS,L,2:NM)*PAUIL(INGAS,L)*CVRTG* &
      CVRT97
      DBAR(vl(ct,sct,13),:) = PJLM(INCOAL,L,2:NM)*PAUIL(INCOAL,L)*CVRTC* &
      CVRT97

IF (GenRelease .ne. 1) THEN
!	BLD, IND, TRN "costs", end use enrgy service (En) and non-energy (NEn)
	sctstr(ct,11) = 'Bld Ser Cost'
	sctstr(ct,12) = 'Ind Ser Cost'
	sctstr(ct,13) = 'Trn Ser Cost'
	DO ISEC = 1, 3
	  sct = 10+ISEC
	  DO J = 1, NNJ
	    vidx = J
	    IF(vp)CALL ivid(0,ct,sct,vidx,(FUELL(J)//' tot'),'90US$/GJ')
	    DBAR(vl(ct,sct,vidx),:) = PJKLM(J,ISEC,L,2:NM)*CVRT90
	  END DO
	    vidx = NNJ + 1	! Add aggregate service cost to the end- sjs 10/00
	    IF(vp)CALL ivid(0,ct,sct,vidx,'E cost (PKLM)','90US$/GJ')	! call is ok as long as NNJ < 9 (since +10 is used below)
	    DBAR(vl(ct,sct,vidx),:) = PKLM(ISEC,L,2:NM)*CVRT90
	END DO

	sctstr(ct,11) = 'Bld NF Cost'
	sctstr(ct,12) = 'Ind NF Cost'
	sctstr(ct,13) = 'Trn NF Cost'
	DO ISEC = 1, 3
	  sct = 10+ISEC
	  DO J = 1, NNJ
	    vidx = J
	    IF(vp)CALL ivid(0,ct,sct,10+vidx,(FUELL(J)),'90US$/GJ')
	    DBAR(vl(ct,sct,10+vidx),:) = HJKLM(J,ISEC,L,2:NM)*CVRT90
	  END DO
	END DO
END IF

!	REGIONAL carbon tax for Refined/Delivered Fuels
	sct = 08
	sctstr(ct,sct) = 'Reg Carbon Price'
	IF(vp)CALL ivid(0,ct,sct,1,'Conv Oil','90US$/GJ')
	IF(vp)CALL ivid(0,ct,sct,2,'Conv Gas','90US$/GJ')
	IF(vp)CALL ivid(0,ct,sct,3,'Coal','90US$/GJ')
	IF(vp)CALL ivid(0,ct,sct,4,'Biomass','90US$/GJ')
    DBAR(vl(ct,sct,1),:) = TXUILM(INOIL,L,2:NM)*CVRT90
    DBAR(vl(ct,sct,2),:) = TXUILM(INGAS,L,2:NM)*CVRT90
    DBAR(vl(ct,sct,3),:) = TXUILM(INCOAL,L,2:NM)*CVRT90
	DBAR(vl(ct,sct,4),:) = TXUILM(JSBMASS,L,2:NM)*CVRT90


!	HYDROGEN Price by Fuel Source (1990 US $/GJ)
	sct = 20
	sctstr(ct,sct) = 'H2 by Fuel'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,4,'Biomass','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,5,'Elctrlys','90US$/GJ')
      IF (NH2_New .gt. 0 .and. vp)CALL ivid(0,ct,sct,6,'New_H2tech1','90US$/GJ')
      IF (NH2_New .gt. 1 .and. vp)CALL ivid(0,ct,sct,7,'New_H2tech2','90US$/GJ')
      DO I=1,5
         DBAR(vl(ct,sct,I),:) = (PHILM(I,L,2:NM)*CVRT90)
      END DO	        
      IF (NH2_New .gt. 0) DBAR(vl(ct,sct,6),:) = (PHILM(9,L,2:NM)*CVRT90)
      IF (NH2_New .gt. 1) DBAR(vl(ct,sct,7),:) = (PHILM(10,L,2:NM)*CVRT90)

!	HYDROGEN Production Non-Energy Cost by Fuel Source (1990 US $/GJ)
	sct = 21
	sctstr(ct,sct) = 'H2 NF Cost'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,4,'Biomass','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,5,'Elctrlys','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,6,'Oil_Cap','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,7,'Gas_Cap','90US$/GJ')
      IF(vp)CALL ivid(0,ct,sct,8,'Coal_Cap','90US$/GJ')
      IF (NH2_New .gt. 0 .and. vp)CALL ivid(0,ct,sct,9,'New_H2tech1','90US$/GJ')
      IF (NH2_New .gt. 1 .and. vp)CALL ivid(0,ct,sct,10,'New_H2tech2','90US$/GJ')
      DO I=1,8+NH2_New
         DBAR(vl(ct,sct,I),:) = (HHILM(I,2:NM)*CVRT90)
      END DO	   

!	Electricity Prices by Sector  (1990 US Cents/KWh)'     
	sct = 30
	sctstr(ct,sct) = 'Elect Enduse'
      FAC=CVRT90*0.36  !Conversion to Cents/KWh      
      IF(vp)CALL ivid(0,ct,sct,1,'Bld','90USc/KWh')
      IF(vp)CALL ivid(0,ct,sct,2,'Ind','90USc/KWh')
      IF(vp)CALL ivid(0,ct,sct,3,'Trn','90USc/KWh')
	DBAR(vl(ct,sct,1),:) = PUKLM(1,L,2:NM)*FAC
	DBAR(vl(ct,sct,2),:) = PUKLM(2,L,2:NM)*FAC
	DBAR(vl(ct,sct,3),:) = PUKLM(3,L,2:NM)*FAC

!	Electricity T&D costs by sector
	sct = 31
	sctstr(ct,sct) = 'Elec T&D Cost'
      IF(vp)CALL ivid(0,ct,sct,1,'Bld','90USc/KWh')
      IF(vp)CALL ivid(0,ct,sct,2,'Ind','90USc/KWh')
      IF(vp)CALL ivid(0,ct,sct,3,'Trn','90USc/KWh')
	DBAR(vl(ct,sct,1),:) = PUTDKM(1,2:NM)*FAC
	DBAR(vl(ct,sct,2),:) = PUTDKM(2,2:NM)*FAC
	DBAR(vl(ct,sct,3),:) = PUTDKM(3,2:NM)*FAC

!     Electricity Generation Prices (1990 Cents/KWh)'
	sct = 32
	sctstr(ct,sct) = 'Elec Gen'
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,4,'Nuclear','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,5,'Solar','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,6,'Hydro','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,7,'Biomass','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,8,'CoalCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,9,'OilCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,10,'GasCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,11,'H2','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,12,'Fus','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,13,'Wind','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,14,'SWStor','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,15,'SatSolar','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,99,'AVG','90C/KWh')
      DBAR(vl(ct,sct,1),:) = PSSUM(INOIL,1,L,2:NM)*FAC
      DBAR(vl(ct,sct,2),:) = PSSUM(INGAS,1,L,2:NM)*FAC
      DBAR(vl(ct,sct,3),:) = PSSUM(INCOAL,1,L,2:NM)*FAC
      DBAR(vl(ct,sct,4),:) = PUILM(4,L,2:NM)*FAC 
      DBAR(vl(ct,sct,5),:) = PUILM(5,L,2:NM)*FAC 
      DBAR(vl(ct,sct,6),:) = PUILM(6,L,2:NM)*FAC 
      DBAR(vl(ct,sct,7),:) = PSSUM(INCOAL,3,L,2:NM)*FAC
      DBAR(vl(ct,sct,8),:) = PSSUM(INCOAL,2,L,2:NM)*FAC
      DBAR(vl(ct,sct,9),:) = PSSUM(INOIL,2,L,2:NM)*FAC
      DBAR(vl(ct,sct,10),:) = PSSUM(INGAS,2,L,2:NM)*FAC
      DBAR(vl(ct,sct,11),:) = PUILM(7,L,2:NM)*FAC
      DBAR(vl(ct,sct,12),:) = PUILM(8,L,2:NM)*FAC
      DBAR(vl(ct,sct,13),:) = PUILM(9,L,2:NM)*FAC
      DBAR(vl(ct,sct,14),:) = PUILM(10,L,2:NM)*FAC
      DBAR(vl(ct,sct,15),:) = PUILM(11,L,2:NM)*FAC
      DBAR(vl(ct,sct,99),:) = PJLM(4,L,2:NM)*FAC 
!     Electricity Fuel Cost (1990 Cents/KWh)
	sct = 33
	sctstr(ct,sct) = 'Elec Fuel Cost'
      FAC=CVRT90*0.36  !Conversion to Cents/KWh  
	    
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,4,'Nuclear','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,5,'Solar','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,6,'Hydro','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,7,'Biomass','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,8,'CoalCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,9,'OilCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,10,'GasCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,11,'H2','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,12,'Fus','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,13,'Wind','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,14,'SWStor','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,15,'SatSolar','90C/KWh')

      DBAR(vl(ct,sct,1),:) = (PSSUM(INOIL,1,L,2:NM)-HUILM(INOIL,L,2:M)) &
      *FAC
      DBAR(vl(ct,sct,2),:) = (PSSUM(INGAS,1,L,2:NM)-HUILM(INGAS,L,2:M)) &
      *FAC
      DBAR(vl(ct,sct,3),:) = (PSSUM(INCOAL,1,L,2:NM)- &
      HUILM(INCOAL,L,2:M))*FAC
      DBAR(vl(ct,sct,4),:) = (PUILM(4,L,2:NM)-HUILM(4,L,2:M))*FAC 
      DBAR(vl(ct,sct,5),:) = (PUILM(5,L,2:NM)-HUILM(5,L,2:M))*FAC 
      DBAR(vl(ct,sct,6),:) = (PUILM(6,L,2:NM)-HUILM(6,L,2:M))*FAC 
      DBAR(vl(ct,sct,7),:) = (PSSUM(INCOAL,3,L,2:NM)- &
      HUILM(JUBMASS,L,2:M))*FAC
      DBAR(vl(ct,sct,8),:) = (PSSUM(INCOAL,2,L,2:NM)- &
      HUILM(JUCSCRUB,L,2:NM))*FAC
      DBAR(vl(ct,sct,9),:) = (PSSUM(INOIL,2,L,2:NM)- &
      HUILM(JUOSCRUB,L,2:NM))*FAC
      DBAR(vl(ct,sct,10),:) = (PSSUM(INGAS,2,L,2:NM)- &
      HUILM(JUGSCRUB,L,2:NM))*FAC
      DBAR(vl(ct,sct,11),:) = (PUILM(7,L,2:NM)-HUILM(JUH2GEN,L,2:M)) &
      *FAC
      DBAR(vl(ct,sct,12),:) = (PUILM(8,L,2:NM)-HUILM(JUFUSION,L,2:M)) &
      *FAC
      DBAR(vl(ct,sct,13),:) = (PUILM(9,L,2:NM)-HUILM(JUWIND,L,2:M))*FAC
      DBAR(vl(ct,sct,14),:) = (PUILM(10,L,2:NM)-HUILM(JUWIND+1,L,2:M))*FAC
      DBAR(vl(ct,sct,15),:) = (PUILM(11,L,2:NM)-HUILM(JUWIND+2,L,2:M))*FAC
!     Electricity Non-Fuel Cost (1990 Cents/KWh)
	sct = 34
	sctstr(ct,sct) = 'Elec NF Cost'
      FAC=CVRT90*0.36  !Conversion to Cents/KWh      
      IF(vp)CALL ivid(0,ct,sct,1,'Oil','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,2,'Gas','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,3,'Coal','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,4,'Nuclear','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,5,'Solar','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,6,'Hydro','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,7,'Biomass','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,8,'CoalCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,9,'OilCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,10,'GasCap','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,11,'H2','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,12,'Fus','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,13,'Wind','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,14,'SWStor','90C/KWh')
      IF(vp)CALL ivid(0,ct,sct,15,'Sat Solar','90C/KWh')
 
      DO I=1,NNU+NStype
         DBAR(vl(ct,sct,I),:) = (HUILM(I,L,2:NM)*FAC)
      END DO	        

!	Carbon Disposal Cost
	sct = 35
	sctstr(ct,sct) = 'C Disp Cost'
      IF(vp)CALL ivid(0,ct,sct,1,'CDisp','90US$/TonneC')
      DBAR(vl(ct,sct,1),:) = CARBDISP(L,2:NM)*CVRT90

!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	IF(WRITENEWTRAN) THEN
!****** PRICES NEW TRANSPORTATION MODULE *******
	ct = 41  ! Transportation prices
	ctstr(ct) = 'TranPrc'
      DO ISER=1,NTSERV
!	   Transportation sector service cost (1990 US $/mile)
         DO IMODE=1,NTMODE
  	      sct = ISER*10+IMODE
	      sctstr(ct,sct) = 'Sv'//TSERV(ISER)//TMD(IMODE)
	      DO J=1,NFUEL
               IF(vp)CALL ivid(0,ct,sct,J,TFUEL(J),'90US$/mi')
               DBAR(vl(ct,sct,J),:) = TRANCOST(ISER,IMODE,J,L,2:NM)
	      END DO
	   END DO
!	   Transportation sector fuel cost (1990 US $/mi)
         DO IMODE=1,NTMODE
  	      sct = 30+ISER*10+IMODE
	      sctstr(ct,sct) = 'FL'//TSERV(ISER)//TMD(IMODE)
	      DO J=1,NFUEL
               IF(vp)CALL ivid(0,ct,sct,J,TFUEL(J),'90US$/mi')
               DBAR(vl(ct,sct,J),:) = TRANFLCT(ISER,IMODE,J,L,2:NM)
	      END DO
	   END DO
!	  'Transportation sector non-fuel cost (1990 US $/mi)'
         DO IMODE=1,NTMODE
  	      sct = 60+ISER*10+IMODE
	      sctstr(ct,sct) = 'NF'//TSERV(ISER)//TMD(IMODE)
	      DO J=1,NFUEL
               IF(vp)CALL ivid(0,ct,sct,J,TFUEL(J),'90US$/mi')
               DBAR(vl(ct,sct,J),:) = TRANNFCT(ISER,IMODE,J,L,2:NM)
	      END DO
	   END DO
      END DO	        
      sct = 90
      sctstr(ct,sct) = 'Agg Ser Cost'
!     Transportation sector aggregate pass service cost (licensed)
      IF(vp)CALL ivid(0,ct,sct,1,'Lic Pass','90US$/mi')
      DBAR(vl(ct,sct,1),:) = TRANCOST(1,NTMODE+1,NFUEL+1,L,2:M)
!     Transportation sector aggregate pass service cost (unlicensed)
      IF(vp)CALL ivid(0,ct,sct,2,'UL Pass','90US$/mi')
      DBAR(vl(ct,sct,2),:) = TRANCOSTUL(L,2:M)
!     Transportation sector aggregate freight service cost.
      IF(vp)CALL ivid(0,ct,sct,3,'Frght','90US$/mi')
      DBAR(vl(ct,sct,3),:) = TRANCOST(2,NTMODE+1,NFUEL+1,L,2:M)
	
!******* END NEW TRANSPORTATION *****
	END IF !end toggle on new tran output
!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	ct = 45  ! elasticities
	ctstr(ct) = 'Elasticities'
!	PRICE ELAST aggregate end-use sector demand
	sct = 01
	sctstr(ct,sct) = 'E Ser Price e'
      DO K=1,3
         IF(vp)CALL ivid(0,ct,sct,K,SECTL(K),'e')
         DBAR(vl(ct,sct,K),:) = RPKLM(K,L,2:NM)
      END DO	        
!	INCOME ELAST aggregate end-use sector demand
	sct = 02
	sctstr(ct,sct) = 'Income e'
      DO K=1,3
         IF(vp)CALL ivid(0,ct,sct,K,SECTL(K),'e')
         DBAR(vl(ct,sct,K),:) = RYKLM(K,L,2:NM)
      END DO	  

!     INCOME elasticity for end-use sector demand BY FUEL
	sct = 03
	sctstr(ct,sct) = 'Income e Fuel'
	vidx = 1
      DO K=1,3
	   DO J=1,4
            IF(vp)CALL ivid(0,ct,sct,vidx,(SECTL(K)//TFUEL(J)),'e')
            DBAR(vl(ct,sct,vidx),:) = RYJKLM(J,K,L,2:NM)
		  vidx = vidx + 1
	   END DO
      END DO	        
!--------------------------------------------------------------------------------------
	
!	Now write the contents of DBAR to the file
	DO I = 1, UBOUND(DBAR,DIM=1)
	  IF (rvl(I).EQ.0) EXIT
	  IF (gblvar(I).NE.0) THEN ! check to see if it is in genrelease
	    WRITE(110,100) RunID, L, rvl(I), DBAR(I,:)
	  END IF
	END DO
!-----------------------------------------------------------------------------------	
!	sum global totals
	DBARG = DBARG + DBAR !sum the whole array

!	**********
  850 CONTINUE !END REGION DO LOOP

!	GLOBAL OUTPUT --------------------------------------
!	first delete all the things that can't be summed
	DO I = 1, UBOUND(DBAR,DIM=1)
	 IF(rvl(I).ge.400000.and.rvl(I).lt.450000) DBARG(I,:)=-11111.11111  !price
	 IF(rvl(I).ge.112200.and.rvl(I).lt.112300) DBARG(I,:)=-11111.11111  !so2ct
	 IF(rvl(I).eq.150501) DBARG(I,:) = -11111.11111   !carbon tax
	 IF(rvl(I).eq.150502) DBARG(I,:) = -11111.11111   !carbon tax
	 IF(rvl(I).eq.152010) DBARG(I,:) = -11111.11111   !gdp/cap
	 IF(rvl(I).eq.152020) DBARG(I,:) = -11111.11111   !co2/cap
	 IF(rvl(I).eq.152030) DBARG(I,:) = -11111.11111   !pec/cap
	 IF(rvl(I).eq.152040) DBARG(I,:) = -11111.11111   !elec/cap
	 IF(rvl(I).ge.212100.and.rvl(I).lt.212400) DBARG(I,:)=-11111.11111  !eff
	 IF(rvl(I).ge.250500.and.rvl(I).lt.250600) DBARG(I,:)=-11111.11111  !eff
	 IF(rvl(I).ge.301200.and.rvl(I).lt.301300) DBARG(I,:)=-11111.11111  !eff
	 IF(rvl(I).ge.357000.and.rvl(I).lt.360000) DBARG(I,:)=-11111.11111  !tran
	 IF(rvl(I).ge.450000.and.rvl(I).lt.460000) DBARG(I,:)=-11111.11111  !elast
	 IF(rvl(I).ge.127600.and.rvl(I).lt.130000) DBARG(I,:)=-11111.11111  !ccurv redux
	END DO
!	now fix anything that remains to be done for global items
!	---------------------------------------------------------
!	put global prices back in:
	ct = 40
	sct = 01
	L = 1 !use USA global price vector
	DBARG(vl(ct,sct,1),:) = (P(INOIL,L,2:NM)*CVRT90)
	DBARG(vl(ct,sct,2),:) = (P(INGAS,L,2:NM)*CVRT90)
	DBARG(vl(ct,sct,3),:) = (P(INCOAL,L,2:NM)*CVRT90)
	DBARG(vl(ct,sct,4),:) = (P(INBMASS,L,2:NM)*CVRT90)
	ct = 40
	sct = 02
	DBARG(vl(ct,sct,1),:) = P(INOIL,L,2:NM)*CVRTO*CVRT97
	DBARG(vl(ct,sct,2),:) = P(INGAS,L,2:NM)*CVRTG*CVRT97
	DBARG(vl(ct,sct,3),:) = P(INCOAL,L,2:NM)*CVRTC*CVRT97
	DBARG(vl(ct,sct,4),:) = P(INBMASS,L,2:NM)*CVRT97
!	so2 ems global needs international shipping added
	DBARG(vl(11,20,99),:) = SO2EMGLBL(2:NM)

!	----------------------------------------------------
!	write global output (DBARG) to region '0'
	DO I = 1,UBOUND(DBARG,DIM=1)
	  IF (rvl(I).EQ.0) EXIT
	  IF (DBARG(I,2).NE.-11111.11111) THEN
	    WRITE(110,100) RunID, 0, rvl(I), DBARG(I,:)
	  END IF
	END DO
!-----END GLOBAL OUTPUT

      RETURN
      END
