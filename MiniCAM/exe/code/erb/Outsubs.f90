!***********************************************************************
!
!     Collection of output subroutines taken from code in ERB's MA.for
!
!***********************************************************************

      SUBROUTINE FULLOUT(DoCase,DoPrice,DoSO2)
      
!***********************************************************************     

!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!***********************************************************************
!
!     More detailed energy accounting 2-d flatfile output
!
! SJS -- also output prices in format useful for calibration

	INTEGER DoCase,DoPrice,DoSO2
	REAL*8 H2WORLD(NHP,NMP),ELECH2G(NMP)
!	CHARACTER*8 REGNAMES(NNLPMax)
!     Converter from 1975$ to 1990$ CVRT
      CVRT = 2.212
	  NLL=NL+1
	  
      IF (DoCase .eq. 1) THEN
        OPEN (1,FILE="case.csv")

         WRITE (1,852)

       DO 850 L=1,NL
          DO 853 M=2,NM
          
!   Temporary reporting variables for IPCC output.
!        Primary demands
          EDOIL=EDILM(INOIL,L,M)
          EDGAS=EDILM(INGAS,L,M)
          EDCOAL=ESILM(INCOAL,L,M)+EXILM(INCOAL,L,M)	! Why is this here for coal and not for others?
          EDBIO=EDILM(IBMASS,L,M)
          EDHYDR=ESILM(6,L,M)
          EDSOLR=ESILM(5,L,M)
          EDNUKE=ESILM(4,L,M)
	    EDFUSION=ESILM(12,L,M)
	    EDWIND=ESILM(13,L,M)
	      ESWStor=ESILM(14,L,M)
          EDSOLR=EDSOLR+EDWIND+ESWStor	! Add wind here so includes all options. Print out separately at end.
	      IF (NStype .ge. 2) EDSOLR = EDSOLR + ESILM(15,L,M)


	      
          EDTOT=EDOIL+EDGAS+EDCOAL+EDBIO+EDHYDR+EDSOLR+EDNUKE+EDFUSION
	IYR = 1975 + (M-1)*NJUMP
	PBIO = PJLM(JSBMASS,L,M)*CVRT
	PH2 = PJLM(JSH2,L,M)*CVRT
	CARBSTOP = CO2DLMG(L,M) - CO2DLM(L,M)
	ELECH2 = ESUILM(JUH2GEN,L,M)*GUILM(JUH2GEN,L,M)
	ELECH2G(M) = ELECH2G(M)+ELECH2   !Electric consumption of H2
	H2ELEC = ESHILM(JHELCTRO,L,M)*GHILM(JHELCTRO,M)
	H2QOIL = ESHILM(INOIL,L,M) + ESHILM(INOIL+NH2,L,M)
	H2QGAS = ESHILM(INGAS,L,M) + ESHILM(INGAS+NH2,L,M)
	H2QCOAL = ESHILM(INCOAL,L,M) + ESHILM(INCOAL+NH2,L,M)

!     Demands for fuel input to Hydrogen

      DO I=1,NF
	   H2WORLD(I,M) = H2WORLD(I,M) + EDRIKLM(I,KH2,L,M)
	END DO
	   H2WORLD(JHBMASS,M) = H2WORLD(JHBMASS,M)  &
                         + EDRIKLM(IBMASS,KH2,L,M)
	   H2WORLD(JHELCTRO,M) = H2WORLD(JHELCTRO,M) + H2ELEC


  852 FORMAT ('Region,','Region#,','Year,','CarbTax,','CarbEmit,', &
      'GDP(ppp),','GDP(mer),','Populat,','PrOilCons,','PrGasCons,', &
      'PrCoalCons,','PrBioCons,','Hydro,','SolWnd,','Nuclear,','Fusion,', &
      'PrTotal,','OilProd,','GasProd,','CoalProd,','BioProd,', &
      'OilPrice,','GasPrice,','CoalPrice,','BioPrice,', &
      'RefOilDem,','RefGasDem,','RefCoalDem,','RefBioDem,','RefTotal,',      &
      'SynLiq,','SynGas,','CoalInSyn,','BioInSyn,','Industry,', &
      'Transport,','Building,','SecETotal,','OilImp,','GasImp,', &
      'CoalImp,','BioImp,','ShaleOil,', &
      'ErbCH4,','ErbN2O_TgN,','SeqCrbEl,','SeqCrbSyn,','SeqCrbH2,', &
      'CrBstop,','CarbOil,','CarbGas,','CarbCoal,','CarbBio,', &
      'ElecOil,','ElecGas,','ElecCoal,','ElecBio,', &
      'IndOil,','IndGas,','IndCoal,','IndElec,','TranOil,','TranGas,',      &
      'TranCoal,','TranElec,','BldgOil,','BldgGas,','BldgCoal,', &
      'BldgElec,','ElOilQ,','ElGasQ,','ElCoalQ,','ElNucQ,','ElSolQ,', &
      'ElHydQ,','ElBioQ,','ElFusQ,','ElCoalScr,','ElOilScr,', &
      'ElGasScr,','ModBmass,','OldBmass,','NucPrice,','SolPrice,', &
      'HydPrice,','ElecPrice,','H2Price,','ElecH2,','IndH2,','TranH2,', &
      'BldgH2,', &
      'H2QOil,','H2QGas,','H2QCoal,','H2QBio,','H2QElec,', &
      'H2Oil,','H2Gas,','H2Coal,','H2Bio,','H2Elec,','WrkAgePop,', &
      'EnergServ,hGWPInd,hGWPTrn,hGWPBld,ElPrcRC,ElPrcInd,ElPrcTrn', &
      ',IndBio,TranBio,BldgBio,PE-Wind,PE-SWStor,GasinSyn' &
      )

      WRITE (1,854)REGNAMES(L),L,IYR,TAXLM(L,M),CO2DLM(L,M),GNPPPP(L,M), &
      GNPMRKT(L,M),ZLM(L,M),EDOIL,EDGAS,EDCOAL,EDBIO,EDHYDR,EDSOLR, &
      EDNUKE,EDFUSION,EDTOT,(ESILM(I,L,M),I=1,NF),ESILM(IBMASS,L,M), &
      ((PJLM(J,L,M)*CVRT),J=1,NF),PBIO,(EDRILM(I,L,M),I=1,NF), &
      EDRILM(IBMASS,L,M),EDRLM(L,M),(SYNILM(I,L,M),I=1,NF), &
      SYNILM(IBMASS,L,M),EFKLM(3,L,M),EFKLM(4,L,M),EFKLM(2,L,M), &
      EFLM(L,M),(EXILM(I,L,M),I=1,NF),EXILM(IBMASS,L,M), &
      ESIL2M(1,L,M),0.0,0.0,-CARBSEQ(2,L,M), &
      -CARBSEQ(1,L,M),-CARBSEQ(3,L,M),-CARBSTOP, &
      (CARBFUEL(IN,L,M),IN=1,INBMASS), &
      (EDRIKLM(I,1,L,M), I=1,INCOAL),EDRIKLM(IBMASS,1,L,M), &
      (FJKLM(J,2,L,M), J=1,NJ),(FJKLM(J,3,L,M), J=1,NJ), &
      (FJKLM(J,1,L,M), J=1,NJ),(ESUILM(I,L,M),I=1,JUBMASS), &
      ESUILM(JUFUSION,L,M),(ESUILM(I,L,M),I=JUCSCRUB,JUGSCRUB), &
      ESIL1M(IBMASS,L,M),ESIL2M(IBMASS,L,M),((PILM(I,L,M)*CVRT),I=4,6), &
      PJLM(4,L,M)*CVRT*0.36,PH2,ELECH2,FJKLM(JSH2,2,L,M), &
      FJKLM(JSH2,3,L,M),FJKLM(JSH2,1,L,M), &
      H2QOIL,H2QGAS,H2QCOAL,(ESHILM(I,L,M),I=4,5), &
      (EDRIKLM(I,KH2,L,M),I=1,NF),EDRIKLM(IBMASS,KH2,L,M),H2ELEC, &
      (WAgeMC(1,L,M)+WAgeMC(2,L,M)),ESERV(L,M), &
      HGWPHFC(L,M),HGWPPFC(L,M),HGWPSF6(L,M), &
      ((PJKLM(4,K,L,M)*CVRT*0.36),K=1,3) &
      ,FJKLM(5,2,L,M),FJKLM(5,3,L,M),FJKLM(5,1,L,M) &
      ,EDWIND,ESWStor,SYNINPUT(2,L,M)
 
      
  854 FORMAT(A8,',',2(I8,','),F12.4,',',500(F14.4,','))

  853 CONTINUE
  850 CONTINUE

      DO 863 M=2,NM
         IYR = 1975 + (M-1)*NJUMP
	   CARBSTOP = CO2MG(M) - CO2DM(M)
	   H2QOILM = ESHIM(INOIL,M) + ESHIM(INOIL+NH2,M)
	   H2QGASM = ESHIM(INGAS,M) + ESHIM(INGAS+NH2,M)
	   H2QCOALM = ESHIM(INCOAL,M) + ESHIM(INCOAL+NH2,M)

       EDSWStor=Sum(ESILM(5,1:NL,M))+Sum(ESILM(13,1:NL,M)) &
               +Sum(ESILM(14,1:NL,M))     

      WRITE (1,854) REGNAMES(NNLP),NNLP,IYR,0.0,CO2DM(M),GNPPPP(NLL,M), &
      GNPMRKT(NLL,M),ZLM(NLL,M)*1000.0,(EDIM(I,M),I=1,NF), &
      EDIM(IBMASS,M),EDIM(6,M),EDSWStor,EDIM(4,M),EDIM(12,M),EDM(M), &
      (ESIM(I,M),I=1,NF),ESIM(IBMASS,M),((P(I,1,M)*CVRT),I=1,INBMASS), &
      (EDRIM(I,M),I=1,NF),EDRIM(IBMASS,M),EDRM(M),(SYNIM(I,M),I=1,NF), &
      SYNIM(IBMASS,M),EFKM(3,M),EFKM(4,M),EFKM(2,M),EFM(M), &
      (EXIM(I,M),I=1,NF),EXIM(IBMASS,M),ESI2M(1,M),0.0,0.0, &
      -CARBSEQM(2,M),-CARBSEQM(1,M),-CARBSEQM(3,M),-CARBSTOP, &
      (CARBFUEL(IN,NLL,M),IN=1,INBMASS), &
      (EDRIKM(I,M), I=1,INCOAL),EDRIKM(IBMASS,M), &
      (FJKM(J,2,M), J=1,NJ),(FJKM(J,3,M), J=1,NJ),(FJKM(J,1,M), J=1,NJ), &
      (ESUIM(I,M),I=1,JUBMASS),ESUIM(JUFUSION,M), &
      (ESUIM(I,M),I=JUCSCRUB,JUGSCRUB),ESI1M(IBMASS,M),ESI2M(IBMASS,M), &
      0.0,0.0,0.0,0.0,0.0,ELECH2G(M),FJKM(JSH2,2,M),FJKM(JSH2,3,M), &
      FJKM(JSH2,1,M),H2QOILM,H2QGASM,H2QCOALM,(ESHIM(I,M),I=4,5), &
      (H2WORLD(I,M), I=1,NH2),(WAgeMC(1,12,M)+WAgeMC(2,12,M)), &
      ESERV(NLL,M),SUM(HGWPHFC(1:NL,M)),SUM(HGWPPFC(1:NL,M)), &
      SUM(HGWPSF6(1:NL,M)) 

!     &ESERVWLD(M),SUM(HGWPHFC(1:NL,M)),SUM(HGWPPFC(1:NL,M)),
!     &(EXIM(I,M),I=1,NF),EXIM(IBMASS,M),ESI2M(1,M),CH4M(M),CNOM(M),	! SJS Delete unused vars
 
  863 CONTINUE

 1000 FORMAT(A80)
	WRITE(1,*)
      WRITE(1,1000)CASENAME
	WRITE(1,1000)CASENOTE
	DO IFILE=1,INFILES
		WRITE(1,1000)FILES(IFILE)
	END DO
    
      END IF ! DoCase



!     DUMP FILE FOR UTILITY PRICES
      IPUDDUMP=0
  999 FORMAT ('Region,','Region#,','Year,','Oil,','Gas,', &
      'Coal,','Nuclear,','Solar,','Hydro,','Biomass,', &
      'CoalScr,','OilScr,','GasScr,','H2Fcell,')
	IF(IPUDDUMP.EQ.1) THEN
	   OPEN(33,FILE='PUDDUMP.CSV')
	   WRITE(33,*)'Electric Gen prices (1990 Cents/KWh)'
	   WRITE(33,999)
	   FAC=CVRT*0.36
	   DO L=1,NL
	      DO M=2,NM
	         IYR = 1975 + (M-1)*NJUMP
	
      WRITE(33,854)REGNAMES(L),L,IYR,(PUILM(IU,L,M)*FAC, IU=1,NNU)
	      END DO
	   END DO
	   CLOSE (33)
	END IF

! - SJS Energy prices in separate file
	IF (DoPrice .eq. 1) then
      OPEN (99,FILE='Prices.csv')	! SJS - Price data to use for calibration
      Write (99,*) 'Casename, Region, Variable, Sector, Oil, Gas, Coal, Electric, Unkown, Unkown, Unkown'
 
      DO L=1,NLL 
155      FORMAT(4(A,','),8(F11.3,','),2(A,',')) 
156      FORMAT(4(A,','),(F11.3,','),3(A,',',F11.3,',')) 
       IF(L.NE.NLL) THEN
         WRITE(99,155)CASENAMEDB,REGNAMES(L),"PJKLM", &
         'ResCom',((PJKLM(J,1,L,2)),J=1,NJP),'75US$/EJ'
         WRITE(99,155)CASENAMEDB,REGNAMES(L),"PJKLM", &
         'Industry',((PJKLM(J,2,L,2)),J=1,NJP),'75US$/EJ'
         WRITE(99,155)CASENAMEDB,REGNAMES(L),"PJKLM", &
         'Transport',((PJKLM(J,3,L,2)),J=1,NJP),'75US$/EJ'

         WRITE(99,156)CASENAMEDB,REGNAMES(L),"BPKL", &
         'ResCom',((BPKL(1,L))),"PKLM (M=1)",PKLM(1,L,1),"PKLM (M=2)",PKLM(1,L,2)
         WRITE(99,156)CASENAMEDB,REGNAMES(L),"BPKL", &
         'Industry',((BPKL(2,L))),"PKLM (M=1)",PKLM(2,L,1),"PKLM (M=2)",PKLM(2,L,2)
         WRITE(99,156)CASENAMEDB,REGNAMES(L),"BPKL", &
         'Transport',((BPKL(3,L))),"PKLM (M=1)",PKLM(3,L,1),"PKLM (M=2)",PKLM(3,L,2)
     
         WRITE(99,156)CASENAMEDB,REGNAMES(L),"PUILM", &
         'Oil',((PUILM(1,L,2))),"P",P(1,L,2),"PILM",PILM(1,L,2),"PJLM",PJLM(1,L,2)
         WRITE(99,156)CASENAMEDB,REGNAMES(L),"PUILM", &
         'Gas',((PUILM(2,L,2))),"P",P(2,L,2),"PILM",PILM(2,L,2),"PJLM",PJLM(2,L,2)
         WRITE(99,156)CASENAMEDB,REGNAMES(L),"PUILM", &
         'Coal',((PUILM(3,L,2))),"P",P(3,L,2),"PILM",PILM(3,L,2),"PJLM",PJLM(3,L,2)
         WRITE(99,156)CASENAMEDB,REGNAMES(L),"PUILM", &
         'Nuclear',((PUILM(4,L,2))),"P",0.0,"PILM",PILM(JUNUC,L,2),"PJLM",PJLM(4,L,2)
         WRITE(99,155)CASENAMEDB,REGNAMES(L),"PUILM", &
         'Solar',((PUILM(5,L,2)))
         WRITE(99,155)CASENAMEDB,REGNAMES(L),"PUILM", &
         'Hydro',((PUILM(6,L,2)))
         WRITE(99,156)CASENAMEDB,REGNAMES(L),"PUILM", &
         'Biomass',((PUILM(7,L,2))),"P",P(INBMASS,L,2),"PILM",PILM(IBMASS,L,2),"PJLM",PJLM(JSBMASS,L,2)
	   END IF
       END DO
       close(99)
	END IF	! DoPrice

! - SJS 1990 SO2 Emissions in separate file
	IF (DoSO2 .eq. 1) then
      OPEN (98,FILE='SO2.csv')	! SJS - Complete SO2 details
       Write (98,*) 'SO2 Emissions (TgS)'
       Write (98,*) 'Region, Year, Var, Total, Bld,Ind,Trn,Elec,Ind Proc, Trad Bio, Bio Burn,'// &
      				'Bld-Oil,Bld-Gas,Bld-Coal,Bld-Bio,Ind-Oil,Ind-Gas,Ind-Coal,Ind-Bio,'// &
      				'Trn-Oil,Trn-Gas,Trn-Coal,Trn-Bio,Ele-Oil,Ele-Gas,Ele-Coal,Ele-Bio,'//    &
      				'el-oilcntl,el-gascntl,el-coalcntl,el-Biocntl'   
 
      DO M = 2,NM
        DO L=1,NLL-1 
157      FORMAT((A,','),(I4,','),(A,','),40(F11.3,',')) 
         WRITE(98,157)REGNAMES(L),1975 + (M-1)*NJUMP,"SO2 Em",SO2EM(L,M), &
      		SUM(SO2EMSJLM(1,:,L,M),DIM=1),SUM(SO2EMSJLM(2,:,L,M),DIM=1), &
      		SUM(SO2EMSJLM(3,:,L,M),DIM=1),SUM(SO2EMSJLM(4,:,L,M),DIM=1), &
      		SO2INDPROC(L,M),SO2TRBIO(L,M),SO2BIOBURN(L,M), &
      		SO2EMSJLM(1,1,L,M),SO2EMSJLM(1,2,L,M),SO2EMSJLM(1,3,L,M),SO2EMSJLM(1,4,L,M), &
      		SO2EMSJLM(2,1,L,M),SO2EMSJLM(2,2,L,M),SO2EMSJLM(2,3,L,M),SO2EMSJLM(2,4,L,M), &
      		SO2EMSJLM(3,1,L,M),SO2EMSJLM(3,2,L,M),SO2EMSJLM(3,3,L,M),SO2EMSJLM(3,4,L,M), &
      		SO2EMSJLM(4,1,L,M),SO2EMSJLM(4,2,L,M),SO2EMSJLM(4,3,L,M),SO2EMSJLM(4,4,L,M), &
      		SO2CTRL(4,1,L,M),SO2CTRL(4,2,L,M),SO2CTRL(4,3,L,M),SO2CTRL(4,4,L,M) &
      		,SO2CTRL(3,1,L,M),SO2CTRL(3,2,L,M),SO2CTRL(3,3,L,M),SO2CTRL(3,4,L,M)
        END DO
         WRITE(98,157)"Global",1975 + (M-1)*NJUMP,"SO2 Em",SUM(SO2EM(:,M)), &
      		SUM(SO2EMSJLM(1,:,:,M)),SUM(SO2EMSJLM(2,:,:,M)), &
      		SUM(SO2EMSJLM(3,:,:,M)),SUM(SO2EMSJLM(4,:,:,M)), &
      		SUM(SO2INDPROC(:,M)),SUM(SO2TRBIO(:,M)),SUM(SO2BIOBURN(:,M)), &
      		SUM(SO2EMSJLM(1,1,:,M)),SUM(SO2EMSJLM(1,2,:,M)),SUM(SO2EMSJLM(1,3,:,M)),SUM(SO2EMSJLM(1,4,:,M)), &
      		SUM(SO2EMSJLM(2,1,:,M)),SUM(SO2EMSJLM(2,2,:,M)),SUM(SO2EMSJLM(2,3,:,M)),SUM(SO2EMSJLM(2,4,:,M)), &
      		SUM(SO2EMSJLM(3,1,:,M)),SUM(SO2EMSJLM(3,2,:,M)),SUM(SO2EMSJLM(3,3,:,M)),SUM(SO2EMSJLM(3,4,:,M)), &
      		SUM(SO2EMSJLM(4,1,:,M)),SUM(SO2EMSJLM(4,2,:,M)),SUM(SO2EMSJLM(4,3,:,M)),SUM(SO2EMSJLM(4,4,:,M))
      END DO
       close(98)
	END IF	! DoSO2

      RETURN

      END
!***********************************************************************      
!
!
!***********************************************************************

      SUBROUTINE CASEOUT
      
!***********************************************************************     

!
! COMMON BLOCKS
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!***********************WRITE SPECIAL OUTPUT FILE*********************
!
!     Add prices to output (1990$) maw 3/29/94
!     Change consumption variables to primary energy demand variables.
!      Use new biomass consumption variable, subtract from solids to get
!      regional coal consumption.  maw 5/31/94

!     Converter from 1975$ to 1990$ CVRT
      CVRT = 2.212
      OPEN (1,FILE=OUTFILE)

      WRITE (1,852)

      DO 850 L=1,NL
!          WRITE (1,851)
  851 FORMAT ('|PRIMARY ENERGY, ','| OIL  |,', &
      'SECONDARY ENERGY,')
  852 FORMAT (' L,','M,','TAX,','CO2,','% OF 90,','OIL,', &
      'GAS,','COAL,','BIO,','HYDRO,','SOLAR,','NUCLR,', &
      'TOTAL,','PRODN,','LIQD,','GAS,','SOLID,','ELEC,', &
      'IND,','TRANS,','R/C,','GNP,','OIL Price.,','GAS,','SOLIDS')
          DO 853 M=1,NM
          
!   Temporary reporting variables for IPCC output.
          EDOIL=EDILM(1,L,M)
          EDGAS=EDILM(2,L,M)
          EDCOAL=ESILM(3,L,M)+EXILM(3,L,M)! -EDBIOLM(L,M)
!          EDBIO=EDBIOLM(L,M)
		  EDBIO = 0.0d0 ! Added 7/01 ktg so EDBIO will have a value defined before it is used
          EDHYDR=ESILM(6,L,M)
          EDSOLR=ESILM(5,L,M)
          EDNUKE=ESILM(4,L,M)
          EDTOT=EDOIL+EDGAS+EDCOAL+EDBIO+EDHYDR+EDSOLR+EDNUKE          
          
          
               WRITE (1,854) L,M,TAXLM(L,M),CO2DLM(L,M),(CO2DLM(L,M)* &
      100.0/CO2DLM(L,2)),EDOIL,EDGAS,EDCOAL,EDBIO,EDHYDR, &
      EDSOLR,EDNUKE,EDTOT, &
      ESIL1M(1,L,M),(EFJLM(J,L,M),J=1,4),EFKLM(3,L,M), &
      EFKLM(4,L,M),EFKLM(2,L,M),GNPFLM(L,M), &
      ((PILM(I,L,M)*CVRT),I=1,6),ZLM(L,M)
!    &((PILM(I,L,M)*CVRT),I=1,6),ZLM(L,M),CH4LM(L,M),CNOLM(L,M)
!		Don't know what some of these are, instead of figuring this out delete.

!        Old write statements used before 5/31/94
!               WRITE (1,854) L,M,TAXLM(L,M),CO2DLM(L,M),(CO2DLM(L,M)*
!     &100.0/CO2DLM(L,2)),(ESILM(1,L,M)+EXILM(1,L,M)),(ESILM(2,L,M)+EXILM   
!     &(2,L,M)),(ESIL1M(3,L,M)+EXILM(3,L,M)),ESIL2M(3,L,M),ESILM(6,L,M),
!     &ESILM(5,L,M),ESILM(4,L,M),(ESLM(L,M)+EXILM(1,L,M)+EXILM(2,L,M)+
!     &EXILM(3,L,M)),ESIL1M(1,L,M),(EFJLM(J,L,M),J=1,4),EFKLM(3,L,M),
!     &EFKLM(4,L,M),EFKLM(2,L,M),GNPFLM(L,M),
!     &((PILM(I,L,M)*CVRT),I=1,6)

  854 FORMAT(I2,',',I2,',',F7.0,',',F6.0,',',F7.1,',',F7.2,',',F7.2,',', &
      F7.2,',',F6.2,',',F6.2,',',F6.2,',',F7.2,',',F7.2,',',F6.2,',', &
      F7.2,',',F6.2,',',F7.2,',',F7.2,',',F7.2,',',F6.2,',',F6.2,',', &
      F7.0,6(1H,,F7.2),',',F10.0,',',F7.2,',',F7.2)
  853 CONTINUE
  850 CONTINUE
!          WRITE (1,861)
  861 FORMAT (' WORLD TOTALS,','|PRIMARY ENERGY, ', &
      '| OIL  |,','SECONDARY ENERGY')
!          WRITE (1,862)
  862 FORMAT ('M,','CO2,','% OF 90,','OIL,', &
      'GAS,','COAL,','BIO,','HYDRO,','SOLAR,','NUCLR,', &
      'TOTAL,','PRODN,','LIQD,','GAS,','SOLID,','ELEC,' &
      ,'IND,','TRANS,','R/C,','GNP,')
          DO 863 M=1,NM
               WRITE (1,864) M,CO2DM(M),(CO2DM(M)* &
      100.0/CO2DM(2)),EDIM(1,M),EDIM(2,M),(EDIM(3,M)- &
      ESI2M(3,M)),ESI2M(3,M),EDIM(6,M),EDIM(5,M),EDIM(4,M), &
      EDM(M),ESI1M(1,M),(EFJM(J,M),J=1,4),EFKM(3,M), &
      EFKM(4,M),EFKM(2,M),GNPFM(M),((P(I,1,M)*CVRT),I=1,3), &
      ZLM(10,M)*1000.0
!     &ZLXM(10,M)*1000.0,CH4M(M),CNOM(M) ! SJS. delete vars not now used
  864 FORMAT(1H,,I2,',',1H,,F7.0,',',F7.1,',',F6.2,',',F6.2,',', &
      F7.2,',',F7.2,',',F6.2,',',F7.2,',',F7.2,',',F8.2,',',F7.2,',', &
      F7.2,',',F6.2,',',F7.2,',',F7.2,',',F7.2,',',F6.2,',',F6.2,',', &
      F8.0,3(1H,,F7.2),',',F10.0,',',F7.2,',',F7.2)
  863 CONTINUE
!*********************************END SPECIAL OUTPUT********************

      RETURN
      END

!***********************************************************************
