 
SUBROUTINE Ag2dbout(RunID,numregions)
            
!***********************************************************************
!
!   This subroutine sends AgLU data to the output database
!     
!
!   Created by Kenny Gillingham 7/01
!     Based on Marshall Wise's Agdbout
!
!***********************************************************************

! Use both modules to allow for access to the dbout variables as well as the AgLU variables    
USE Ag2Global8
USE COMMON

IMPLICIT NONE

INTEGER,PARAMETER :: TotJ = 8

INTEGER RunID, ct, sct,J,I,numregions,T 
REAL(8) DBAR(500,2:NM),DBARG(500,2:NM),TotCrops(numregions,NM),GlobTotCrops(NM)
REAL(8) GlobConsump(NM),Globpopu(NM)
LOGICAL vp !vp is if to print out a varid file
CHARACTER(9) seclabels(TotJ), landseclabels(5)


seclabels = (/'Forest   ','FwdForest','Beefprds ','Foodgr   ','CoarseGr ',&
&'OilCrops ','MiscCrops','Biomass  '/)

landseclabels = (/'Crops    ','Pasture  ','Forest   ','BioLand  ','Unmgd    '/)


100 FORMAT(1I10,1H,,1I4,1H,,1I6,8(1H,,1F20.5))

!	INITIALIZATION
DBARG = 0.0 !initialize global totals
vl = 0
rvl = 0
gblvar = 0
dbaridx = 0

DO L = 1,numregions !REGION DO LOOP 

!	------------------------------------------------------------------------------------
!	  Begin database output format code
!	------------------------------------------------------------------------------------

  DBAR = 0.0 !initialize DBAR

!	------------------------------------------------------------------------------------

  vp = .FALSE.
  IF (L .EQ. 1) vp = .TRUE. !only print varid if region 1


!	* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  ct = 90  ! ag model
  ctstr(ct) = 'Ag2Model'

!	PROD, PRICES, and LAND USE
  sctstr(ct,01) = 'Prod'
  sctstr(ct,02) = 'Prices'
  sctstr(ct,03) = 'LandUse'

  DO J = 1,TotJ
	sct = 01
	IF(J.EQ.1 .OR. J.EQ.2) THEN
	  IF(vp)CALL ivid(GenRelease,ct,sct,J,seclabels(J),'000c.m.') ! Forest units
	ELSE
	  IF(vp)CALL ivid(GenRelease,ct,sct,J,seclabels(J),'10^13cals')	! Crop units
	END IF
	IF(J.NE.3) DBAR(vl(ct,sct,J),:) = qsup(J,L,2:NM)  !production
	IF(J.EQ.3) DBAR(vl(ct,sct,J),:) = AgFD(J,1,L,2:NM)+AgFD(J,2,L,2:NM) ! beef production
	sct = 02
  IF (GenRelease .eq. 1) THEN
	IF(vp) CALL ivid(0,ct,sct,J,seclabels(J),'index')
  ELSE
	IF(J.EQ.1 .OR. J.EQ.2) THEN
	  IF(vp)CALL ivid(0,ct,sct,J,seclabels(J),'$/c.m.') ! Forest units
	ELSE
	  IF(vp)CALL ivid(0,ct,sct,J,seclabels(J),'$/Gcal')	! Crop units
	END IF
  END IF ! GenRelease 
    
    DBAR(vl(ct,sct,J),:) = sprice(J,L,2:NM)  !prices
	IF (GenRelease .eq. 1) DBAR(vl(ct,sct,J),:) = sprice(J,L,2:NM)/sprice(J,L,2)  !price indices	
  END DO

  sct = 01
  IF(vp)CALL ivid(GenRelease,ct,sct,9,'Pasture','?')
  DBAR(vl(ct,sct,9),:) = qsup(3,L,2:NM)  ! pasture production
  sct = 02
  IF(vp)CALL ivid(GenRelease,ct,sct,9,'Pasture','$/Gcal')
  DBAR(vl(ct,sct,9),:) = sprice(10,L,2:NM)  ! pasture price
  
  DO J = 1, 5
	sct = 03
	IF(vp)CALL ivid(GenRelease,ct,sct,J,landseclabels(J),'000 Ha')
    DBAR(vl(ct,sct,J),:) = SaveLand(J,L,2:NM)  !land use
  END DO
    J = 6
	IF(vp)CALL ivid(GenRelease,ct,sct,J,"Non-prod Land",'000 Ha')
    DBAR(vl(ct,sct,J),:) = HistLand(L,10)  !Add output for the land assumed not to be used at all

! BIOMASS YIELDS
  sct = 04
  sctstr(ct,sct) = 'Yields'

  IF(vp)CALL ivid(GenRelease,ct,sct,1,'Biomass','Gcal/ha')	! Gcal/ha yield
  DBAR(vl(ct,sct,1),:) = (qsup(8,L,2:NM)/SaveLand(4,L,2:NM))*10.0d0
  IF(vp)CALL ivid(GenRelease,ct,sct,2,'Biomass','GJ/ha')	! GJ/ha yield
  DBAR(vl(ct,sct,2),:) = ((qsup(8,L,2:NM)*GJperGCAL/(100000d0))/ &
		SaveLand(4,L,2:NM))*1000000.0d0

! CROP YIELDS
  GlobTotCrops(:) = 0.0d0
  DO T = 2,NM
	TotCrops(L,T) = qsup(4,L,T)+qsup(5,L,T)+qsup(6,L,T)+qsup(7,L,T)
	GlobTotCrops(T) = GlobTotCrops(T) + TotCrops(L,T)
  END DO

  IF(vp)CALL ivid(GenRelease,ct,sct,3,'Crops','Gcal/ha')	! Gcal/ha crop yield
  DBAR(vl(ct,sct,3),:) = (TotCrops(L,2:NM)/SaveLand(1,L,2:NM))*10.0d0
  IF(vp)CALL ivid(GenRelease,ct,sct,4,'Forest','c.m./ha')	! c.m./ha forest yield
  DBAR(vl(ct,sct,4),:) = (qsup(1,L,2:NM)/SaveLand(3,L,2:NM))

! DIETS (in kcals/person)
  sct = 10
  sctstr(ct,sct) = 'Diets'

  DO J = 3, TotJ-1
	IF(vp)CALL ivid(0,ct,sct,J,seclabels(J),'kcal/cap/day')
    DBAR(vl(ct,sct,J),:) = SaveDiet(J,L,2:NM)  ! Food diet
  END DO

  IF(vp) CALL ivid(0,ct,sct,8,'ProcCrops','kcal/person')
  DBAR(vl(ct,sct,8),:) = SaveDiet(8,L,2:NM) ! Processed crops diet
  IF(vp) CALL ivid(0,ct,sct,9,'Pork','kcal/person')
  DBAR(vl(ct,sct,9),:) = SaveDiet(9,L,2:NM) ! Pork diet
  IF(vp) CALL ivid(0,ct,sct,10,'Poultry','kcal/person')
  DBAR(vl(ct,sct,10),:) = SaveDiet(10,L,2:NM) ! Poultry diet
  SaveDiet(11,L,2:NM) = 0.0d0
  DO J = 3,10	! Calculate Total Diet
	SaveDiet(11,L,2:NM) = SaveDiet(11,L,2:NM) + SaveDiet(J,L,2:NM)
  END DO  
  IF(vp) CALL ivid(0,ct,sct,11,'Total Diet','kcal/person')
  DBAR(vl(ct,sct,11),:) = SaveDiet(11,L,2:NM) ! Total Diet

! CONSUMPTION 
  sct = 11
  sctstr(ct,sct) = 'Consump'

  DO J = 3, TotJ-1
	IF(vp)CALL ivid(0,ct,sct,J,seclabels(J),'kcal/day')
    DBAR(vl(ct,sct,J),:) = SaveDiet(J,L,2:NM)*popu(L,2:NM)  ! Food consumption
  END DO

  IF(vp) CALL ivid(0,ct,sct,8,'ProcCrops','kcal')
  DBAR(vl(ct,sct,8),:) = SaveDiet(8,L,2:NM)*popu(L,2:NM) ! Processed crops consumption
  IF(vp) CALL ivid(0,ct,sct,9,'Pork','kcal')
  DBAR(vl(ct,sct,9),:) = SaveDiet(9,L,2:NM)*popu(L,2:NM) ! Pork consumption
  IF(vp) CALL ivid(0,ct,sct,10,'Poultry','kcal')
  DBAR(vl(ct,sct,10),:) = SaveDiet(10,L,2:NM)*popu(L,2:NM) ! Poultry consumption 

! FEED CONSUMPTION

  sct = 12
  sctstr(ct,sct) = 'BeefFeed'
  DO J = 4, TotJ-1
	IF(vp)CALL ivid(GenRelease,ct,sct,J,seclabels(J),'10^13cals')
    DBAR(vl(ct,sct,J),:) = AgZ(J,3,L,2:NM)  ! Beef Feed Consumption
  END DO

  sct = 13
  sctstr(ct,sct) = 'PorkFeed'
  DO J = 4, TotJ-1
	IF(vp)CALL ivid(GenRelease,ct,sct,J,seclabels(J),'10^13cals')
    DBAR(vl(ct,sct,J),:) = AgZ(J,10,L,2:NM)  ! Pork Feed Consumption
  END DO

  sct = 14
  sctstr(ct,sct) = 'PoultryFeed'
  DO J = 4, TotJ-1
	IF(vp)CALL ivid(GenRelease,ct,sct,J,seclabels(J),'10^13cals')
    DBAR(vl(ct,sct,J),:) = AgZ(J,11,L,2:NM)  ! Poultry Feed Consumption
  END DO

! CARB EMISSIONS FROM LAND USE CHANGE
  sct = 15
  sctstr(ct,sct) = 'LUC Emiss'

  IF(vp)CALL ivid(GenRelease,ct,sct,1,'CarbEmiss','TgC')
  DBAR(vl(ct,sct,1),:) = CarbEmiss(L,2:NM)


!  AG CH4 EMISSIONS ("base" data, not actual emissions)
  sct = 16
  sctstr(ct,sct) = 'AgCh4Act'

  IF(vp)CALL ivid(GenRelease,ct,sct,1,'enteric','Activity')
  IF(vp)CALL ivid(GenRelease,ct,sct,2,'mgdmanure','Activity')
  IF(vp)CALL ivid(GenRelease,ct,sct,3,'oth/rice','Activity')
  IF(vp)CALL ivid(GenRelease,ct,sct,99,'tot','NONE')
  DBAR(vl(ct,sct,1),:) = AG2CH4(1,L,2:NM)
  DBAR(vl(ct,sct,2),:) = AG2CH4(2,L,2:NM)
  DBAR(vl(ct,sct,3),:) = AG2CH4(3,L,2:NM)
  DBAR(vl(ct,sct,99),:) = AG2CH4(4,L,2:NM)


! AG N2O EMISSIONS ("base" data, not actual emissions)
  sct = 17
  sctstr(ct,sct) = 'AgN2O'

  IF(vp)CALL ivid(GenRelease,ct,sct,1,'soils','Activity')
  IF(vp)CALL ivid(GenRelease,ct,sct,2,'mgdmanure','Activity')
  IF(vp)CALL ivid(GenRelease,ct,sct,3,'empty','Activity')
  IF(vp)CALL ivid(GenRelease,ct,sct,99,'tot','NONE')
  DBAR(vl(ct,sct,1),:) = AG2N2O(1,L,2:NM)
  DBAR(vl(ct,sct,2),:) = AG2N2O(2,L,2:NM)
  DBAR(vl(ct,sct,3),:) = AG2N2O(3,L,2:NM)
  DBAR(vl(ct,sct,99),:) = AG2N2O(4,L,2:NM)
	
!	PROD, PRICES, and LAND USE
  sct = 18
  sctstr(ct,sct) = 'Exports'

  DO J = 1,TotJ
	IF(J.EQ.1 .OR. J.EQ.2) THEN
	  IF(vp)CALL ivid(GenRelease,ct,sct,J,seclabels(J),'000c.m.') ! Forest units
	ELSE
	  IF(vp)CALL ivid(GenRelease,ct,sct,J,seclabels(J),'10^13cals')	! Crop units
	END IF
	DBAR(vl(ct,sct,J),:) = qsup(J,L,2:NM) - qdem(J,L,2:NM)  ! Supply - Demand
  END DO

! Write the contents of DBAR to the file

  DO I = 1, UBOUND(DBAR,DIM=1)
	IF (rvl(I).EQ.0) EXIT
	IF (gblvar(I).NE.0) THEN
	  WRITE(110,100) RunID, L, rvl(I), DBAR(I,:)
	END IF
  END DO

! Sum global totals

  DBARG = DBARG + DBAR !sum the whole array


END DO !END REGION DO LOOP


!	GLOBAL OUTPUT --------------------------------------

!	fix anything that remains to be done for global items
!	---------------------------------------------------------

! Overwrite global prices
sct = 02
L = 1 !use USA global price vector
DO J = 1, TotJ
	DBARG(vl(ct,sct,J),:) = sprice(J,L,2:NM)  ! Global prices
END DO
DBARG(vl(ct,sct,9),:) = sprice(10,L,2:NM)	! Pasture price set to USA's pasture price
DBARG(vl(ct,sct,3),:) = 0.0d0 ! Set global anim prds prices to a baseline
DBARG(vl(ct,sct,8),:) = 0.0d0 ! Same for Biomass prices (no global price for either)

! Calculate global average diet
sct = 10
GlobConsump(2:NM) = 0.0d0
Globpopu(2:NM) = 0.0d0
DO L = 1,numregions
  GlobConsump(2:NM) = GlobConsump(2:NM) + SaveDiet(11,L,2:NM)*popu(L,2:NM)
  Globpopu(2:NM) = Globpopu(2:NM) + popu(L,2:NM)
END DO
DBARG(vl(ct,sct,11),:) = GlobConsump(2:NM) / Globpopu(2:NM)
DO J = 3,10
	DBARG(vl(ct,sct,J),:) = 0.0d0	! zero out the rest of the global diet totals
END DO

! Calculate average global yields
sct = 04
DBARG(vl(ct,sct,1),:) = (DBARG(vl(ct,01,8),:)/DBARG(vl(ct,03,4),:))*10d0 ! biomass Gcal/ha
DBARG(vl(ct,sct,2),:) = (DBARG(vl(ct,01,8),:)*GJperGCAL/(100000d0)/ &
		DBARG(vl(ct,03,4),:))*1000000d0   ! biomass GJ/ha
DBARG(vl(ct,sct,3),:) = (GlobTotCrops(2:NM)/DBARG(vl(ct,03,1),:))*10d0 ! crop Gcal/ha
DBARG(vl(ct,sct,4),:) = (DBARG(vl(ct,01,1),:)/DBARG(vl(ct,03,3),:)) ! forest c.m./ha

!	----------------------------------------------------
!	write global output (DBARG) to region '0'
DO I = 1,UBOUND(DBARG,DIM=1)
  IF (rvl(I).EQ.0) EXIT
  ! Check to make sure it is not a blank column
  IF (DBARG(I,2).NE.0.0d0 .OR. DBARG(I,5).NE.0.0d0 .OR. DBARG(I,9).NE.0.0d0) &
    WRITE(110,100) RunID, 0, rvl(I), DBARG(I,:)
END DO

!-----END GLOBAL OUTPUT


END SUBROUTINE Ag2dbout