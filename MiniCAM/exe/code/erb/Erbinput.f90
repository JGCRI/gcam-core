!**********************************************************************
!
      SUBROUTINE ERBINPUT(INF)
!
!***********************************************************************
!
!                    -- DATA INPUT MODULE --
!
! THIS SUBROUTINE READS AND PRINTS EXOGENOUS INPUT DATA AND PARAMETER
! VALUES IN THE FOLLOWING ORDER:
!
!   PIM,TRI                STISL                  BSJKLM
!   NXIL,TXILM,TXJLM       BESIL                  RPKK,RPJK
!   ZL,ZLM                 ESFILM                 RPK,RPJ
!   PROLM,GNPBL            FLRL                   RYKK,RYJK
!   GIJ,HIJ                HYDRO                  RYJ,RYKLT
!   GUI,HUIL,RUI           CILT (SOLAR)           RY
!   PAUIL                  HCILT                  COI
!   BSUILM                 GCI,RCI                SBURNL,SHALE
!   TJKLM                  GJK,GJ                 SFEDIL
!   RIGISL                 HJK,HJ                 BIOPSM
!   CIGIS                  SJKLP,BSKL             BIOLM
!   VISL                                          RYSHT
!   DILSET
!   RIL
!
! MODEL VERSION:  A.07.31.84
!
! SUBROUTINES CALLED: NONE
!
! CODED BY: JAE EDMONDS             LATEST REVISION BY: DAN WAHL
! COMPLETED: 1 JANUARY 1982         ON 3 NOVEMBER 1982
!                                 12 January 1995, C. MacCracken
!                                       combined OREAD and OREAD2;
!                                       added Marshall's variable/case
!                                       data input method
!***********************************************************************
!
!
! COMMON BLOCKS
!
      USE COMMON
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                           
      COMMON/CO2Cal/CO2Calb(3)	! Used only in calib.for
      COMMON/BCOC/FBC1990, FOC1990, FSO2_dir1990,FSO2_ind1990	! Used only in maglink

!
!  -- DECLARE CHARACTER VARIABLES --
      REAL*8 TMPREAD(NLPMax,NMP),TMM,TMI  ! temporary array for reading in.
      CHARACTER*11 TABLEMRK, tempStr
      INTEGER ISERV,MI,MF,MM,MMM
      DATA TABLEMRK/'INPUT_TABLE'/
      Parameter (verbflag = 0)	! For debugging input problems
	  REAL CIGISTemp(NLPMax,NMP)
	  Integer TempRegNumber(NLPMax), NRegionstemp
	  
 3000 FORMAT(I8,15F16.0)
 3010 FORMAT(2I8,15F16.0)
 
!
!     find the location of the first input data.
	  IF (verbflag .eq. 1) write (*,*) "Begin ERB Input"
      CALL NEXTTABLE(TABLEMRK,INDIC)
	  IF (verbflag .eq. 1) write (*,*) "First Table Found ", INDIC
!     If no data is found (or file is empty), notify and stop
      IF(INDIC.EQ.0) THEN
	   WRITE(*,*)'The ghosts in the machine have determined that'
	   MsgStr = "The file <"//FILES(INF)//"> was not found or has no data:"
       Call MCLog(1,MsgStr,0,0,0,0)
	   STOP
	END IF
        
!     The nexttable subroutine returns INDIC=0 when it reaches the end
!     of a file, else returns 1.  Loop while INDIC=1, else finished.
      DO WHILE(INDIC.EQ.1)
      READ(IUNIT,*)
!     Read the variable indicator label and select the right code.
      READ(IUNIT,'(I4)')IVARNUM 	! Changed to I4 to allow more codes. sjs 12/00
      
      READ(IUNIT,*)                                            
      READ(IUNIT,*)
      IF (verbflag .eq. 1) write (*,*) IVARNUM
      
      SELECT CASE(IVARNUM)
            
!     *************************************************************
         CASE(1)
! -----------------------------------------------
! -- READ IN ENERGY PRICE PARAMETERS           --
! -----------------------------------------------
!     Transportation costs.
          READ (IUNIT,*)   (TRI(I),I=1,NF),TRI(INBMASS) 

         CASE(2)
         
!     Period 1 solution starting prices.
          DO L=1,NL
             READ (IUNIT,*) IDUM, (P(IN,L,1),IN=1,NIN) 
          END DO
         CASE(3)
         
!     Trade barrier multipliers
          DO I=1,NF+1
             DO L=1,NL
                READ(IUNIT,*) IDUM,(TXILM(I,L,M),M=1,NM) 
             END DO       
          END DO
             
         CASE(4)
!           ---------------------------------------
!           --  READ IN FINAL CONSUMPTION TAXES  --
!           ---------------------------------------
          DO  J=1,NJ
                 READ (IUNIT,*) IDUM,(TXJKLM(J,1,1,M),M=1,NMREAD)  
          END DO
!     Fill out to all regions and sectors
          DO  J=1,NJ
              DO L=1,NL
              NK=NKL(L)
                 DO K=1,NK
	             DO M=1,NMREAD
                     TXJKLM(J,K,L,M) = TXJKLM(J,1,1,M)   
	             END DO
                 END DO
              END DO
          END DO
          
          CASE(5) 
!     Read in exogenous flaring carbon emissions
           DO L=1,NL
              READ(IUNIT,*) IDUM, (EXOFLARE(L,M), M=1,NM)
           END DO         
         CASE(6)
!     Population (thousands), ZLM(L,0) is population for 1960
	       ifPopRate = .false.                                       !      hmp
             DO L=1,NL
                READ (IUNIT,*) IDUM, (ZLM(L,M),M=0,NM) 
            END DO
         CASE(7)
!      ---------------------------------------------
!      --  READ IN BASE GNP                       --
!      ---------------------------------------------
         DO L=1,NL
            READ (IUNIT,*)   IDUM,GNPBL(L) 
         END DO
         CASE(8)
!      ---------------------------------------------
!      --  READ IN LABOR PRODUCTIVITY GROWTH      --
!      ---------------------------------------------
         DO L=1,NL
            READ (IUNIT,*)  IDUM,(PROLM(L,M),M=1,NM)
         END DO
                       
                 
         CASE(9)
! ----------------------------------------------------------------------
! --  READ IN REFINERY COEFFICIENTS AND BASE SECONDARY ENERGY PRICES  --
! ----------------------------------------------------------------------
          READ (IUNIT,*)   (GIJ(I),I=1,NF),GIJ(JSBMASS) 
         
         CASE(10)
          READ (IUNIT,*)   (HIJ(I),I=1,NF),HIJ(JSBMASS) 
         
         CASE(11)
!        Read in number of markets
         READ(IUNIT,*) NMRK
         CASE(12)
!     Read in market definition table
          DO MRK=0,NMRK
	       READ(IUNIT,*)(MRKDEF(MRK,I),I=1,3),(MRKDEF(MRK,I), &
                   I=4,3+MRKDEF(MRK,3))
	    END DO
!     Logit elasticity parameters for utilities
         CASE(13)
          READ (IUNIT,*)   (RUI(I),I=1,NU) 
         
         CASE(14)
! ------------------------------------------------
! --  READ IN ELECTRIC UTILITY BASE PRICE DATA  --
! ------------------------------------------------
!
          DO L=1,NL
             READ (IUNIT,*)  IDUM,(PAUIL(I,L),I=1,NF+1) 
          END DO
          
         CASE(15)
!      -------------------------------------------------------------
!      --  READ IN FUEL SHARE WEIGHTS BY PERIOD, FUEL AND REGION  --
!          NU first then NL, order is different than before  shk 4/26/00
!      -------------------------------------------------------------
!
          DO I=1,NU
             DO L=1,NL
                READ (IUNIT,*)  IDUM,(BSUILM(I,L,M),M=1,NM) 
             END DO
          END DO
          
         CASE(16)
! ---------------------------------------------------------------
! --  READ TKLM--RATE OF END-USE ENERGY EFFICIENCY IMPROVEMENT  --
! --            BY SECTOR AND REGION                           --
! ---------------------------------------------------------------
!     read in only to 1990, use 216 to read in from 2005 on.
          DO K=1,3
             DO L=1,NL
               READ (IUNIT,'(I8,18F10.0)') IDUM,(TKLM(K,L,M), M=1,2)
             END DO
          END DO

         CASE(17)
!
!              READ RIGISL--RESOURCE BASE AMOUNTS
!
          DO L=1,NL
             DO IG=1,NIG
                READ (IUNIT,*) IDUM,IDUM, &
                     (RIGISL(IG,IS,L), IS=1,5)
             END DO
          END DO

         CASE(18)
!
!              READ CIGIS--EXTRACTION COST AMOUNTS
!
          NNIG=NIG+1
          DO IG=1,NNIG
             READ (IUNIT,*)   IDUM,(CIGIS(IG,IS),IS=1,NNIS) 
          END DO                     
         
         CASE(19)	! Added for compatability with older 11 region version runs. sjs - 11/00
         			! DO NOT USE otherwise
         			! Read in as $1990 $/gj
!
!     ---Solar costs (used to be in CIGIS -- now in HUILM)
!
         MsgStr = "Archaic read-in for solar costs used. Use readin #:"
         Call MCLog(2,MsgStr,0,0,0,471)
          DO L=1,NL
              READ (IUNIT,*) IDUM,(CIGISTemp(L,M),M=1,NM)
          END DO                                     
 
          DO L=1,NL
            DO M=1,NM
               HUILM(5,L,M) = 1.7 + CIGISTemp(L,M)/2.219
            END DO
          END DO         
        
         
         CASE(20)
!
!              READ RIL--PRICE ELASTICITY OF SUPPLY FOR
!                        FOSSIL FUELS, EVALUATED AT DILSET
!
          DO  L=1,NL
             READ (IUNIT,*)   IDUM,(RIL(IS,L),IS=1,NIS) 
          END DO
                  
         CASE(21)
!
!              READ STISM--RATE OF TECHNOLOGICAL CHANGE
!                          BY SUPPLY MODE AND TIME
!           (Replaces STISL which was by region maw 12-8-97)
!
          DO M=1,NM
             READ (IUNIT,*) IDUM,(STISM(IS,M),IS=1,NNIS) 
          END DO
         CASE(22)
!
!                  READ BESIL--MINIMUM LEVEL OF SHORT TERM
          DO  L=1,NL
             READ (IUNIT,*)   IDUM,(BESIL(IS,L),IS=1,NNIS) 
          END DO
         
         CASE(23)
!
!              READ ESFIL--INITIAL ENERGY SUPPLY ESTIMATES
!
          DO  L=1,NL
             READ (IUNIT,*)   IDUM,(ESFIL(IS,L),IS=1,NNIS) 
          END DO
   
         CASE(24)
! ------------------------------------------------
! --  READ NATURAL GAS FLARING RATE PARAMETERS  --
! ------------------------------------------------
!
          DO  L=1,NL
             READ (IUNIT,*)   IDUM,(FLRL(II,L),II=1,3)  
          END DO
          
         CASE(25)
! --------------------------------------------------
! --  READ IN HYDROELECTRIC PARAMETERS BY REGION  --
! --------------------------------------------------
!
          DO  L=1,NL
             READ (IUNIT,*) IDUM,(HYDRO(I,L),I=1,5)  
          END DO
          
         CASE(26)
!  -----------------------------------------------------------
!  --  READ SYNFUEL ADD ON COSTS FOR N.GAS TO LIQUIDS ONLY  --
!  -----------------------------------------------------------
!
          DO L=1,NL
             READ (IUNIT,*) IDUM,IDUM,(HCILT(2,1,L,IT),IT=1,3) 	! Type error mismatch fixed. sjs & sk. 1/02
          END DO
                
         CASE(27)
!  --------------------------------
!  --  READ SYNFUEL ADD ON COSTS --
!  --------------------------------
!
          DO L=1,NL
             READ (IUNIT,*) IDUM,IDUM,(HCILT(3,1,L,IT),IT=1,3), &
             (HCILT(3,2,L,IT),IT=1,3),(HCILT(4,1,L,IT),IT=1,3),  &
             (HCILT(4,2,L,IT),IT=1,3)								! Type error mismatch fixed. sjs & sk. 1/02
          END DO
                  
         CASE(28)
!  ------------------------------------------
!  --  READ SYNFUEL CONVERSION EFFICIENCY  --
!  ------------------------------------------
!
         DO I=1,2
            READ (IUNIT,*)  IDUM,(GCI(J,I),J=2,INBMASS)
         END DO   
         CASE(29)
!  -------------------------------------------------
!  --  READ SYNFUEL ELASTICITY CONTROL PARAMETER  --
!  -------------------------------------------------
!
         DO I=1,2
            READ (IUNIT,*)  IDUM,(RCI(J,I),J=2,INBMASS)
         END DO   
         CASE(30)
!  -----------------------------------------------------
!  --  READ ENERGY SERVICE INPUT-OUTPUT COEFFICIENTS  --
!  -----------------------------------------------------
!
          DO K=1,NKMAX
             READ (IUNIT,*)  IDUM,(GJK(J,K),J=1,NNJ-1)  
          END DO
    
    
         CASE(32)
!  -------------------------------------------------
!  --  READ NON-ENERGY INPUT-OUTPUT COEFFICIENTS  --
!  -------------------------------------------------
!
          DO K=1,NKMAX
             READ (IUNIT,*)  IDUM,(HJK(J,K),J=1,NNJ-1)  
          END DO

         CASE(34)
!  ---------------------------------------------------
!  --  READ BASE ENERGY SERVICE CONSUMPTION SHARES  --
!  ---------------------------------------------------
!
          DO L=1,NL
             NK=NKL(L)
             DO  K=1,NK
               READ (IUNIT,*) IDUM,(SJKLP(J,K,L),J=1,NJ)  
               SJKLP(NJ+1,K,L) = 0	! Initialize biomass to zero -- sjs 10/00
               SJKLP(NJ+2,K,L) = 0  ! Initialize H2 to zero
             END DO
          END DO

         CASE(35)
!  ---------------------------------------------------
!  --  READ BASE ENERGY SERVICE CONSUMPTION SHARES  --
!  ---------------------------------------------------
!
          DO L=1,NL
             NK=NKL(L)
             DO K=1,NK
               READ (IUNIT,*) IDUM,BSKL(K,L)  
             END DO
          END DO
          
         CASE(36)
! ---------------------------------------------------------------------
! -- READ LOGIT FUNCTION WEIGHTS AND SECTORAL ENERGY SERVICE DEMANDS --
! ---------------------------------------------------------------------
!
          NK=NKL(1)
          DO  L=1,NL
              DO J=1,NJ
                 DO K=1,NK
                 READ (IUNIT,*) IDUM, (BSJKLM(J,K,L,M),M=1,NMREAD)  
                 END DO
              END DO
          END DO
         CASE(37)
!  --------------------------------------
!  --  READ OECD END-USE ELASTICITIES  --
!  --------------------------------------
!
          DO K=1,NKMAX
             READ (IUNIT,'(F10.0)') RPKK(K)
          END DO
!
         
         CASE(38)
!  --------------------------------------
!  --  READ Global END-USE ELASTICITIES  --
!      different from before, since same elasticity is used
!      take out regional index.  shk 4/26/00
!  --------------------------------------
!
          DO K=1,NKMAX
             READ (IUNIT,'(4F10.0)') (RPJK(J,K),J=1,NJ)  
          END DO
!
         
         CASE(39)
          READ (IUNIT,'(F10.0)') RPK

         CASE(40)
          READ (IUNIT,'(4F10.0)') (RPJ(J),J=1,NJ)  

         CASE(41)               
         
            DO K=1,3
                DO L=1,NL
                  READ (IUNIT,'(I8,18F10.0)') IDUM, &
                          (TALPHA(K,L,M), M=1,NM)
                END DO
            END DO
         
         CASE(42)
         
          DO  L=1,NL
             DO  J=1,NJ
                DO  K=1,NKMAX
                  READ (IUNIT,*) IDUM,(RYJKLM(J,K,L,M),M=1,NM)  
                END DO
             END DO
          END DO    

         CASE(43)
          
         DO L=1,NL
            READ (IUNIT,'(F10.0)') RYL(L) 
         END DO
         
         CASE(44)
! -----------------------------------------
! --  READ IN CARBON RELEASE PARAMETERS  --
! -----------------------------------------
!
          KK=7
          READ (IUNIT,*)  (COI(I),I=1,KK) 

         CASE(45)
!  -----------------------------------
!  --  READ GAS FLARING PARAMETERS  --
!  -----------------------------------
!
          DO L=1,NL
             READ (IUNIT,*)  (SBURNL(L,IT),IT=1,3) 
          END DO

         CASE(46)
!  -----------------------------------
!  --  READ GAS FLARING PARAMETERS  --
!  -----------------------------------
!
          DO L=1,NL
             READ (IUNIT,*)  (SHALE(L,IT),IT=1,3) 
          END DO

         CASE(47)
!  ---------------------------------
!  --  READ FEEDSTOCK PARAMETERS  --
!  ---------------------------------
!
          DO L=1,NL
             READ (IUNIT,*)  (SFEDIL(I,L),I=1,NF) 
          END DO

         CASE(48)
!  ---------------------------------
!  --  READ BIOMASS COEFFICIENTS  --
!  ---------------------------------
!
          DO IP=1,NBIP
             READ (IUNIT,*)  (BIOPSM(IP,1,IM),BIOPSM(IP,2,IM), &
                                IM=1,NIM) 
          END DO

         CASE(49)  
         
          DO  L=1,NL
             READ (IUNIT,*)  (BIOLM(L,IM),IM=1,NIM) 
          END DO
             
         CASE(50)
         
          READ (IUNIT,'(F10.0)') RYSHT 
          

! *****************************************************************************
! ******  CH4 and N2O EMISSIONS PARAMTERS *************************************
 
          CASE(59)
!--------READ PRICE ELASTICITY OF TRADITIONAL BIOMASS (for ch4 biom burning)
!
          DO  L=1,NL
               READ (IUNIT,'(F10.0)') RPBIOL(L) 
          END DO
 
     
	 CASE(61)
!--------READ INCOME ELASTICITY OF TRADITIONAL BIOMASS (for ch4 biom burning)
!
          DO L=1,NL
               READ (IUNIT,'(F10.0)') RYBIOL(L) 
          END DO


     CASE(62)
!--------READ INITIAL STOCK OF TRADITIONAL BIOMASS (for ch4 biom burning)
!
          DO L=1,NL
               READ (IUNIT,'(F10.0)') EBIOL(L) 
          END DO
 

!**************** END CH4 and N2O PARAMS ******************************


         CASE(67)
!           ------------------------------------------
!           --  READ IN SEVERENCE TAX             ----
!           ------------------------------------------
!
          DO IS=1,5 
             READ (IUNIT,*)IDUM, (TXISLM(IS,1,M),M=1,NMREAD) 
          END DO
!        Expand to all regions
          DO L=1,NL
	      DO IS=1,5
	         DO M=1,NMREAD
	            TXISLM(IS,L,M) = TXISLM(IS,1,M)
	         END DO
	      END DO
	    END DO

         CASE(68)
!           ------------------------------------------
!           --  READ IN CARBON TAX CONVERSION     --
!           ------------------------------------------
! -- This var is the conversion from 1990 Carbon tax ($/ton) to 1975 fuel prices ($/GJ)
! Later stored in TXUILM0

          DO I=1,6 
             READ (IUNIT,*)IDUM, (TXUILM(I,1,M),M=1,NMREAD) 
          END DO   
!         Expand to all regions
          DO L=1,NL
	      DO I=1,6
	         DO M=1,NMREAD
	            TXUILM(I,L,M) = TXUILM(I,1,M)
	         END DO
	      END DO
	    END DO

         CASE(69)
!      -------------------------------------------------------------
!      --  READ IN ENVIRONMENTAL COST BY PERIOD, FUEL AND REGION  --
!      -------------------------------------------------------------
!
          DO IS=1,NNIS
              DO L=1,NL    
                READ (IUNIT,*)IDUM,(VISLM(IS,L,M),M=1,NM) 
              END DO
          END DO

         CASE(70)
!      -------------------------------------------------------------
!      --  READ IN NET Delivery EFFICIENCY BY PERIOD, FUEL AND REGION  --
!          Read in for all technology types and all periods.
!          Different from before. shk 4/26/00
!      -------------------------------------------------------------
!
          DO I=1,NNU
             DO L=1,NL
                READ (IUNIT,*)IDUM,(GUILM(I,L,M),M=1,NM) 
             END DO
          END DO

         CASE(71)
!      -------------------------------------------------------------
!      --  READ IN NON-ENERGY COSTS BY PERIOD, FUEL AND REGION  --
!          Read in for all technology types and all periods.
!          Different from before. shk 4/26/00
!      -------------------------------------------------------------
!
          DO I=1,NNU+NStype	!Include advanced "Solar-type" tech's
             DO L=1,NL
                READ (IUNIT,*)IDUM,(HUILM(I,L,M),M=1,NM) 
             END DO
          END DO

!     Read in more detailed energy IO for transportation
         CASE(72)    
            K=3
            DO J=1,NNJ-1
               DO L=1,NL
                  READ(IUNIT,*) IDUM,(GJKLM(J,K,L,M), M=1,NM)
               END DO
            END DO
         
         CASE(73)      
         
            K=3
            DO J=1,NNJ-1
               DO L=1,NL
                  READ(IUNIT,*) IDUM,(HJKLM(J,K,L,M), M=1,NM)
               END DO
            END DO
!     Taxes in 1990 $/mtc
         CASE(74) 
         
          DO L=1,NL
            READ (IUNIT,*) IDUM,(TAXRLM(L,M), M=3,NM)
          END DO
          
!     tax mode
         CASE(75) 
          DO L=1,NL
             READ (IUNIT,'(2I8)')IDUM,NTAXMODE(L)
          END DO
         
!     Carbon-free backstop cost  (1990 $/mtC removed)
                   
         CASE(76) 
          DO M=1,NM
	        READ(IUNIT,*) IDUM,CFBSTOP(M)
	    END DO
!     Coal (solids) electric subsector competition logit exponent
!     Read in subsector competition logit exponent for renewables,
         CASE(77)   
         DO J=INOIL,INGAS
	      READ(IUNIT,*)(RUISS(J,ISS), ISS=1,2)
	   END DO
         J=INCOAL
	   READ(IUNIT,*)(RUISS(J,ISS), ISS=1,3)              
                       
         CASE(78)
!        Block of data for doing carbon emissions trading groups
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)            
         END DO
           
           
!     Income elasticities of demand
         CASE(79)
         
          DO L=1,NL
             DO K=1,NKMAX
                READ (IUNIT,*) IDUM,(RYKLM(K,L,M), M=1,NM)
             END DO
          END DO
!        Electric subsector logit coefficients
!        Include share weights for solar renewables,
         CASE(80)
      
	   DO I=INOIL,INGAS
	      DO ISS=1,2
               READ(IUNIT,*)IDUM,(BSSUILM(I,ISS,1,M), M=1,NM)
!              Propagate to all regions
	         DO L=2,NL
	            DO M=1,NM
	               BSSUILM(I,ISS,L,M) = BSSUILM(I,ISS,1,M)
	            END DO
               END DO
	      END DO
	   END DO
         I=INCOAL
         DO ISS=1,3
            READ(IUNIT,*)IDUM,(BSSUILM(I,ISS,1,M), M=1,NM)
	      DO L=2,NL
	         DO M=1,NM
	            BSSUILM(I,ISS,L,M) = BSSUILM(I,ISS,1,M)
	         END DO
            END DO
	   END DO
!        Solids final demand subsector logit exponents
!        (Dimensioned now just for coal, will have to add fuel
!           index if want other fuels to have subsectors)
         CASE(81)
         J=INCOAL
         DO K=1,NKMAX
	      READ(IUNIT,*)(RSSPJK(K,ISS), ISS=1,2)
	   END DO

!        Solids final demand subsector logit coefficients
!        (Dimensioned now just for coal, will have to add fuel
!           index if want other fuels to have subsectors)
         CASE(82)
         J=INCOAL
	   DO ISS=1,2
	      DO L=1,NL
               DO K=1,NKMAX
	            READ(IUNIT,*)IDUM,(BSSJKLM(K,ISS,L,M), M=1,NM)
	         END DO
	      END DO
         END DO
!     Carbon disposal or sequestration costs, entered in 1990 $/mtC
!     If zero or not read in, model assumes disposal is not an option
         CASE(83)
 
         DO L=1,NL
            READ(IUNIT,*)IDUM,(CARBDISP(L,M), M=3,NM)
         END DO

!     Carbon sequestration removal fraction, by mode (1=synfuel, 2=elec)
         CASE(84)
         DO L=1,NL
            READ(IUNIT,*)IDUM,(REMFRAC(IM,L), IM=1,2)
         END DO
!     Entry period for coal phase-out project
         CASE(85)
         DO L=1,NL
            READ(IUNIT,*)IDUM,(ICARBOUT(L,IM),IM=1,2)
         END DO
!     Conversion penalty from fossil fuel to H2
         CASE(86)
         READ(IUNIT,*)(GIJH2(I),I=1,NF),GIJH2(JSBMASS)
!     Conversion form market GDPs to PPP GDPs (PPP/Market)
         CASE(87)
	   DO L=1,NL
	      READ(IUNIT,*)IDUM,PPPCONV(L)
	   END DO
!        Electric subsector logit coefficients for biomass by region
         CASE(88)
      
         I=INCOAL
         ISS = 3
	     DO L=1,NL
            READ(IUNIT,*)IDUM,(BSSUILM(I,ISS,L,M), M=1,NM)
	     END DO

!     Rate at which supply limit on fossil production can expand with GDP.
         CASE(90)
          DO I=1,NF
             DO L=1,NL
                READ(IUNIT,*) IDUM,(RFosExpan(I,L,M),M=1,NM) 
             END DO       
          END DO


!     Control for electrolytic H2 input electric price
         CASE(97)
          READ (IUNIT,*) NH2electPtype

!     Read in max elec util share by technology
        CASE(98)
	   DO I=1,NNU+NStype	!Include advanced "Solar-type" tech's
	      READ(IUNIT,*)IDUM,ECapLim(I)
	   END DO
!     Read in strings for region names
        CASE(99)
	   DO L=1,NNLP
	      READ(IUNIT,*)IDUM,REGNAMES(L)
	      REGNAMES(L) = trim(REGNAMES(L))
	   END DO
!     read in hydrogen fuel cell electric generation tech parameters
         CASE(101)
	   READ(IUNIT,*)IDUM,(GUILM(JUH2GEN,1,M), M=1,NM)
         READ(IUNIT,*)IDUM,(HUILM(JUH2GEN,1,M), M=1,NM)
!        Copy same values to all regions
!        Previous version had error; did not copy HUILM values
!        to all regions.  shk 4/26/00
         DO L=1,NL
            DO M=1,NM
	         GUILM(JUH2GEN,L,M) = GUILM(JUH2GEN,1,M)
               HUILM(JUH2GEN,L,M) = HUILM(JUH2GEN,1,M)
            END DO
	   END DO
!     read in hydrogen fuel cell electric generation share coeff.
         CASE(102)
	   DO L=1,NL
            READ(IUNIT,*)IDUM,(BSUILM(JUH2GEN,L,M), M=1,NM)
	   END DO
         
!     read in hydrogen production i/o numbers, by fuel.
!     1) energy input (GJ/GJ) 
         CASE(103)
	   DO I=1,NNH2
            READ(IUNIT,*)IDUM,(GHILM(I,M), M=1,NM)
	   END DO
 
!     read in hydrogen production i/o numbers, by fuel.
!     2) NON-energy input ($/GJ) 
         CASE(104)
	   DO I=1,NNH2
            READ(IUNIT,*)IDUM,(HHILM(I,M), M=1,NM)
	   END DO
!     read in hydrogen production share coeff.
         CASE(105)
	   DO I=1,NH2
            READ(IUNIT,*)IDUM,(BSHILM(I,M), M=1,NM)
	   END DO
!     read in hydrogen production share exponent.
         CASE(106)
	   DO I=1,NH2
            READ(IUNIT,*)IDUM,RHI(I)
	   END DO
!     read in hydrogen production subsector share coeff.
         CASE(107)
         DO I=1,NF
            DO ISS=1,2
               READ(IUNIT,*)IDUM,(BSSHILM(I,ISS,1,M), M=1,NM)
!              Propagate to all regions
               DO L=2,NL
                  DO M=1,NM
                     BSSHILM(I,ISS,L,M) = BSSHILM(I,ISS,1,M)
	            END DO
               END DO
            END DO
         END DO
!     read in end-use hydrogen consumption share coeff.
         CASE(108)
         NK=NKL(1)
         J=JSH2
         DO L=1,NL
            DO K=1,NK
               READ (IUNIT,*) IDUM, (BSJKLM(J,K,L,M),M=1,NM)  
            END DO
         END DO
  
!     read in end-use hydrogen consumption energy I/O coeff.
         CASE(109)
         NK=NKL(1)
         J=JSH2
         L=1
         DO K=1,NK
            READ (IUNIT,*) IDUM, (GJKLM(J,K,L,M),M=1,NM)  
         END DO
!        Copy to all regions
         DO M=1,NM
            DO K=1,NK
	         DO L=1,NL
	            GJKLM(J,K,L,M) = GJKLM(J,K,1,M)
	         END DO
	      END DO
	   END DO
!     read in end-use hydrogen consumption NON-energy I/O coeff.
         CASE(110)
         NK=NKL(1)
         J=JSH2
         L=1
         DO K=1,NK
            READ (IUNIT,*) IDUM, (HJKLM(J,K,L,M),M=1,NM)  
         END DO
!        Copy to all regions
         DO M=1,NM
            DO K=1,NK
	         DO L=1,NL
	            HJKLM(J,K,L,M) = HJKLM(J,K,1,M)
	         END DO
	      END DO
	   END DO
                 
         CASE(111)
!
!              READ CIGIS--EXTRACTION COST FOR A SINGLE FUEL -- sjs 05/01
!
          READ (IUNIT,*) NumFuel
          NNIG=NIG+1
          DO IG=1,NNIG
             READ (IUNIT,*)   IDUM,(CIGIS(IG,NumFuel)) 
          END DO                     

         CASE(112)
! ----------------------------------------------------------
! -- READ IN NUMBER OF "new" H2 elec gen technologies (sjs - 08/02)
! ----------------------------------------------------------
          READ (IUNIT,*) NH2_New

!     read in hydrogen production costs for "new" H2 technologies
!     2) NON-energy input ($/GJ) 
      CASE(113)
	   DO I=NNH2+1,NNH2 +NH2_New	! sjs -- 08/02; add inputs for new H2 technologies
	          READ(IUNIT,*)IDUM,(HHILM(I,M), M=1,NM)
	   END DO


	   CASE(170)
!        Read-in deforestation emissions
        UserDeforestEm = 1
        READ(IUNIT,*) IDUM, (DeForest(M), M = 2,NM)
         
	   CASE(171)
!        Reset deforestation emissions to internal values
	     UserDeforestEm = 0
        
	   CASE(172)
!        Read-in cement emissions
	     READ(IUNIT,*) IDUM, (Cement(M), M = 2,NM)
        
         CASE(175)
!		Readin for carbon concentration constraint
         CarbConstraintType = 2.0
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)            
         END DO
         
         CASE(176)
!		Readin for total forcing constraint
         CarbConstraintType = 3.0
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)            
         END DO
         
         CASE(177)
!		Readin for GHG forcing constraint
         CarbConstraintType = 4.0
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)            
         END DO
         
         CASE(178)
!        Block of data for doing carbon emissions trading groups
!	If this readin is used the emissions constraint will apply to fossil + land-use emissions.
! 		(but not industrial -- i.e., cement, etc., emissions). sjs - 01
         CarbConstraintType = 1.0
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)            
         END DO
                      
!     Income elasticities of demand 
! This triggers alternative SRES version of energy demand calc so don't have to re-calibrate. sjs -- 11/00
         CASE(179)
         
          DO L=1,NL
             DO K=1,NKMAX
                READ (IUNIT,*) IDUM,(RYKLM(K,L,M), M=1,NM)
             END DO
          END DO
          ISRESeserDmd = 1
          MsgStr = "Using variant energy demand formulation: "
      	Call MCLog(3,MsgStr,0,0,0,0)

         CASE(180)
!		Readin for Carbon Intensity constraint
         CarbConstraintType = 5.0
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)            
         END DO
         
         CASE(181)
!		Readin for Carbon Intensity Rate constraint
         CarbConstraintType = 6.0
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)  
         END DO
         
         CASE(182)
!		Readin for GHG Intensity constraint
         CarbConstraintType = 7.0
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)            
         END DO
         
         CASE(183)
!		Readin for GHG Intensity Rate constraint
         CarbConstraintType = 8.0
         DO L=1,NL
             READ(IUNIT,*) IDUM,(CEMTARGS(L,M), M=3,NM)            
         END DO
         
         CASE(184)
!		Toggle to set carbon intensity constraint to market-based GDP (default)
         IPPPInten = 0

         CASE(185)
!		Toggle to set carbon intensity constraint to PPP-based GDPs
         IPPPInten = 1

!     Alternate read in for consumption taxes by region      
         CASE(204)
          DO  J=1,NJ-1
	      DO  L=1,NL
                 READ (IUNIT,*) IDUM,(TXJKLM(J,1,L,M),M=1,NM)  
            END DO
	    END DO
!     Fill out to all regions and sectors
          DO  J=1,NJ
              DO L=1,NL
              NK=NKL(L)
                 DO K=1,NK
	             DO M=1,NM
                     TXJKLM(J,K,L,M) = TXJKLM(J,1,L,M)   
	             END DO
                 END DO
              END DO
          END DO
!     Population read in as growth rates (for interface)
!     birth, death and migration rates to drive demog subroutine
         CASE(206)
         ifpoprate = .true.
         Call Popdata(iunit)
 
        
         CASE(208)
!      ---------------------------------------------
!      --  READ IN LABOR PRODUCTIVITY GROWTH (for interface)     --
!      ---------------------------------------------
         DO L=1,NL
            READ (IUNIT,*)  IDUM,(PROLM(L,M),M=3,NM)
         END DO

         CASE(216)
! ---------------------------------------------------------------
! --  READ TKLM--RATE OF END-USE ENERGY EFFICIENCY IMPROVEMENT  --
! --            for interface, read one for all sectors       --
! ---------------------------------------------------------------
!
         DO L=1,NL
          READ(IUNIT,*) IDUM, (TMPREAD(L,M), M=3,NM)
         
!        Propagate to all sectors
          DO K=1,NKMAX
            DO M=3,NM
               TKLM(K,L,M) = TMPREAD(L,M)
            END DO
          END DO
         
         END DO
         CASE(220)         
! ---------------------------------------------------------------
! --  READ TKLM--RATE OF END-USE ENERGY EFFICIENCY IMPROVEMENT  --
! --  New version, read in for all time periods at once - sjs 10/2/00
! --            for each sector and region  --
! ---------------------------------------------------------------
!
          DO K=1,3
             DO L=1,NL
               READ (IUNIT,*) IDUM,(TKLM(K,L,M), M=1,NM)
             END DO
          END DO

	  CASE(236)
!   Alternate way to read in end-use logit shares
         NK=NKL(1)
          DO  J=1,NJ
              DO L=1,NL
                 DO K=1,NK
                 READ (IUNIT,*) IDUM, (BSJKLM(J,K,L,M),M=1,NMREAD)  
                 END DO
              END DO
          END DO
	  CASE(240)
!   Readin for all time periods for finer control
         NK=NKL(1)
          DO  J=1,NJ
              DO L=1,NL
                 DO K=1,NK
                 READ (IUNIT,*) IDUM, (BSJKLM(J,K,L,M),M=1,NM)  
                 END DO
              END DO
          END DO

        CASE(270)
!      -------------------------------------------------------------
!      --  READ IN NET Delivery EFFICIENCY BY PERIOD for coal, interface
!      -------------------------------------------------------------
!      NOT USED ANY MORE
          I=INCOAL
          DO L=1,NL
            READ (IUNIT,*)IDUM,(GUILM(I,L,M),M=3,NM)
            DO M=3,NM
                GUILM(I,L,M) = 1.D0/GUILM(I,L,M)		
            END DO
          END DO

        CASE(299)
!      -------------------------------------------------------------
!      --  READ IN NET Delivery EFFICIENCY BY PERIOD for any one technology -- more convienent for scenarios sjs 01/00
!      -------------------------------------------------------------
!
             READ (IUNIT,*)IFuel,(GUILM(IFuel,1,M),M=1,NM) 	! Index follows that for first period 
             DO L=2,NL
                READ (IUNIT,*)IDUM,(GUILM(IFuel,L,M),M=1,NM) 
             END DO
          
        CASE(300)
!      -------------------------------------------------------------
!      --  READ IN Generation EFFICIENCY BY PERIOD for any one technology -- more convienent for scenarios sjs 01/00
!      -------------------------------------------------------------
!
             READ (IUNIT,*)IFuel,(GenEff(IFuel,1,M),M=1,NM) 	! Index follows that for first period 
             DO L=2,NL
                READ (IUNIT,*)IDUM,(GenEff(IFuel,L,M),M=1,NM) 
             END DO
          
        CASE(301)
!      -------------------------------------------------------------
!      --  READ IN Distribution Losses BY PERIOD 
!      -------------------------------------------------------------
!
             DO L=1,NL
                READ (IUNIT,*)IDUM,(DistLoss(L,M),M=1,NM) 
             END DO

        CASE(269)
!	Read in by fuel         
!     Trade barrier multipliers
             DO L=1,NL
                READ(IUNIT,*) IFuel,(TXILM(IFuel,L,M),M=1,NM) 
             END DO       
          
         CASE(370)
!      -------------------------------------------------------------
!      --  READ IN NET Delivery EFFICIENCY BY PERIOD, FUEL AND REGION  --
!          Read in to last period and for all generation technologies.
!          Different from before; before NNU-1  shk 4/26/00
!      -------------------------------------------------------------
          DO I=1,NNU
             DO L=1,NL
                READ (IUNIT,*)IDUM,(GUILM(I,L,M),M=1,NM) 
             END DO
          END DO
        CASE(371)
!      -------------------------------------------------------------
!      --  READ IN NON-ENERGY COSTS BY PERIOD, FUEL AND REGION  --
!          Read in to last period and for all generation technologies.
!          Different from before; before NNU-1  shk 4/26/00
!      -------------------------------------------------------------
          DO I=1,NNU+NStype	!Include advanced "Solar-type" tech's
             DO L=1,NL
                READ (IUNIT,*)IDUM,(HUILM(I,L,M),M=1,NM) 
             END DO
          END DO
          
        CASE(372)  
!      -------------------------------------------------------------
!      --  READ IN ratio of 1990 to 1975 GNP for working age method
!      -------------------------------------------------------------
!         read in one value for each region as ratio of constant 1987 US$
!         from 1997 World bank World Development Indicators CD
           do L = 1,NL
	        read(IUNIT,*)IDUM,GNPIND(L,1)
	     end do

        Case(373)
!      -------------------------------------------------------------
!      --  READ IN labor force participation rates for working age method
!      -------------------------------------------------------------
!         Read in time series for each region--ad hoced by hmp february 1998
!         from 1997 World bank World Development Indicators CD
           do L = 1,NL
	        read(IUNIT,*)IDUM,(RLFP(L,M),M=1,NM)
	     end do

        Case(374)
!      -------------------------------------------------------------
!      --  READ IN Sulfates
!      -------------------------------------------------------------
!         Read in sulfates (for MAGICC)   mrj 7/98
	        READ(IUNIT,*)(SULFIN(M),M=2,NM)
        Case(375)
!      -------------------------------------------------------------
!      --  READ IN labor force percentage
!      ---------------------------------------participation----------------------
!         This variable enables exact duplication of demographic modeule results while using 
!         		read-in of population values. 
!         Useful becaue this variable does not change much -- co can use new population figures
!             without re-doing the complex population input files.
!         SJS -- 12/00
         LFPercMethod = 1	! Turn on new method of calcuating GDP
           do L = 1,NL
	        read(IUNIT,*)IDUM,(PLFPerc(L,M),M=1,NM)
!	        write(*,*) IDUM, PLFPerc(L,2),PLFPerc(L,3),PLFPerc(L,4),PLFPerc(L,5)
	     end do

	  Case(400)
!      -------------------------------------------------------------
!      --  READ IN High GWP gases - SRES ONLY !
!      -------------------------------------------------------------
!           mrj 8/98
			DO IITER = 1, 56 !test reg inputs SRES ONLY!
				READ(IUNIT,*)LL,IISC,(HGWPREAD(IISC,LL,M),M=2,NM)
			END DO
	  Case(401)
			DO IITER = 1, 14 !test reg inputs SRES ONLY!
				READ(IUNIT,*)IDUM,HGWPGWP(IITER)
			END DO
	  Case(402)
!      -------------------------------------------------------------
!      --  READ IN SRES Standardization values - Generally would be used for SRES only
!      -------------------------------------------------------------
!           SJS 7/00
			MagCalb = 1
			DO IITER = 1, 8
				READ(IUNIT,*)LL,(SRESCALB(IITER,M),M=1,2)
			END DO

        CASE(470)
!      -------------------------------------------------------------
!      --  READ IN NET Delivery EFFICIENCY BY PERIOD, FUEL AND REGION  --
!      --  One Plant at a Time                                       --
!      -------------------------------------------------------------
!         read-in to last period
          DO L=1,NL
             READ (IUNIT,*)I,(GUILM(I,L,M),M=1,NM) 
          END DO
         
        CASE(471)
!      -------------------------------------------------------------
!      --  READ IN NON-ENERGY COSTS BY PERIOD, FUEL AND REGION    --
!      --  One Plant at a Time, Does not include T&D costs        --
!      -------------------------------------------------------------
!         read-in to last period
!
          DO L=1,NL
             READ (IUNIT,*)I,(HUILM(I,L,M),M=1,NM) 
          END DO
        CASE(472)
!      -------------------------------------------------------------
!      --  READ IN electric generation shares for one technology only -- sjs 03/01
!      --  Allows switching down/off one tech at a time        --
!      -------------------------------------------------------------
!         read-in to last period
!
          DO L=1,NL
             READ (IUNIT,*)I,(BSUILM(I,L,M),M=1,NM) 
          END DO
!      -------------------------------------------------------------
!      --  READ IN SO2 MODULE VARIABLES
!      -------------------------------------------------------------
  	   CASE(501)
		DO IITER = 1, NLP*4*3
	      READ(IUNIT,*) L,IISC,JJ,SO2SCT(IISC,JJ,L)
          END DO
	   CASE(502)
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2IPBASE(L)
          END DO
	   CASE(503)
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2TRBIOB(L)
          END DO
     	   CASE(504)
		DO IITER = 1, NLP*4
	      READ(IUNIT,*) L,IISC,SO2ASHR(IISC,L)
          END DO
	   CASE(505)
		DO IITER = 1, NLP*4*3
	      READ(IUNIT,*) L,IISC,JJ,SO2ENDN(IISC,JJ,L)
          END DO
	   CASE(506)
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2TAU(L)
          END DO
	   CASE(507)
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2GDP0(L)
          END DO
	   CASE(508)
		DO IITER = 1, NLP*4*3
	    READ(IUNIT,*) L,IISC,JJ,SO2MAXCTRL(IISC,JJ,L)
          END DO
	   CASE(509)
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2IPMR(L)
          END DO
	   CASE(510)
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2PLG(L)
          END DO
	   CASE(511)
		DO L = 1,NLP
	      READ(IUNIT,*) IDUM, (SO2BIOBURN(L,MM),MM=2,9)
          END DO
	   CASE(512)
	    READ(IUNIT,*) IDUM,(SO2SHIP(MM),MM=2,9)
	   CASE(513)
	
		DO L = 1,NLP
	      READ(IUNIT,*) IDUM, SO2BUNKF(L)
          END DO
	   CASE(514) !alternate liquids-must be read after all other sulfur stuff
	
		DO IITER = 1, NLP*4
	      READ(IUNIT,*) L,IISC,SO2PETSCLIM(IISC,L)
          END DO
!	    use the max sulf content redux as the max control percentage
	    SO2MAXCTRL(:,1,:) = (1-(SO2PETSCLIM(:,:)/SO2SCT(:,1,:))) * 100
!	    toggle the altliquids on
	    SO2ALTLIQ = 1.0d0
 	   CASE(515)
	    DO LL = 1, NL
	       READ(IUNIT,*) IDUM,(SO2MAGREG(LL,iHGII), iHGII=1,3)
	    END DO
	   CASE(516)	! Easy way to increase or decrease S controls while retaining 1990 calibration
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2CNTRLADJ(L)
        END DO
	   CASE(517)	! Easy way to increase or decrease S controls while retaining 1990 calibration
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2TauADJ(L)
        END DO
  	   CASE(521)		! SJS -- new version. Sectors all together. Easier to read. 10/00
		DO IITER = 1, NLP*3
	      READ(IUNIT,*) L,JJ,(SO2SCT(IISC,JJ,L), IISC =1,4)
        END DO
  	   CASE(522)		! SJS -- new version. Sectors all together. Easier to read. 10/00
		DO IITER = 1, NLP
	      READ(IUNIT,*) L,(SO2ASHR(IISC,L), IISC =1,4)
        END DO
  	   CASE(523)		! SJS -- new version. Sectors all together. Easier to read. 10/00
		DO IITER = 1, NLP*3
	      READ(IUNIT,*) L,JJ,(SO2ENDN(IISC,JJ,L), IISC =1,4)
        END DO
          
  	   CASE(524)		! SJS -- new version. Sectors all together. Easier to read. 10/00
		DO IITER = 1, NLP*3
	      READ(IUNIT,*) L,JJ,(SO2MAXCTRL(IISC,JJ,L), IISC =1,4)
        END DO
   	   CASE(525)		! SJS -- new version. Sectors all together. Easier to read. 10/00
		DO IITER = 1, NLP
	      READ(IUNIT,*) L,(SO2PETSCLIM(IISC,L), IISC =1,4)
        END DO
!	    use the max sulf content redux as the max control percentage
	    SO2MAXCTRL(:,1,:) = (1-(SO2PETSCLIM(:,:)/SO2SCT(:,1,:))) * 100
!	    toggle the altliquids on
	    SO2ALTLIQ = 1.0d0

	   CASE(526)
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, SO2_Cntrl_TechChange(L)
          END DO
                               
	   CASE(530)	! Per-capita cooling demand
		DO IITER = 1,NLP
	      READ(IUNIT,*) L, CoolDemand(L)
        END DO
                               
                
         CASE(571)
!      -------------------------------------------------------------------
!      --  READ IN TRANSMISSIONS AND DISTRIBUTION COSTS FOR ELECTRICITY --
!      --  BY END-USE SECTOR AND PERIOD                                 --
!      -------------------------------------------------------------------
!         read-in to last period
!
          DO K=1,NKMAX
             READ (IUNIT,*)I,(PUTDKM(K,M),M=1,NM) 
          END DO

         CASE(572)
!      -------------------------------------------------------------------
!      --  READ IN TRANSMISSIONS AND DISTRIBUTION COSTS FOR H2 --
!      --  BY END-USE SECTOR AND PERIOD                                 --
!      -------------------------------------------------------------------
!         read-in to last period
!
          DO K=1,NKMAX
             READ (IUNIT,*)I,(PH2TDKM(K,M),M=1,NM) 
          END DO

         CASE(600)
! ----------------------------------------------------------
! -- READ IN SOLUTION CRITERIA   --
! ----------------------------------------------------------
!     Same for all regions
          READ (IUNIT,*) TEST
         CASE(601)
! ----------------------------------------------------------
! -- READ IN NUMBER OF "Solar-type" elec gen technologies (sjs - 02/01)
! ----------------------------------------------------------
          READ (IUNIT,*) NStype

         CASE(604)
! ----------------------------------------------------------
! -- READ IN NUMBER OF TECHNOLOGIES IN ELECTRICITY SECTOR --
! ----------------------------------------------------------
!     Same for all regions
          READ (IUNIT,*) NNU
          NU  = NNU-4		! must also set this here -- sjs 02/01
          
         CASE(605)
! ----------------------------------------------------------
! -- READ IN INDEX FOR TECHNOLOGIES IN ELECTRICITY SECTOR --
! ----------------------------------------------------------
!     Same for all regions
          READ (IUNIT,*) IDUM, JUOIL
          READ (IUNIT,*) IDUM, JUGAS
          READ (IUNIT,*) IDUM, JUCOAL
          READ (IUNIT,*) IDUM, JUNUC
          READ (IUNIT,*) IDUM, JUSOLPV
          READ (IUNIT,*) IDUM, JUHYDRO
          READ (IUNIT,*) IDUM, JUBMASS
          READ (IUNIT,*) IDUM, JUCSCRUB
          READ (IUNIT,*) IDUM, JUOSCRUB
          READ (IUNIT,*) IDUM, JUGSCRUB
          READ (IUNIT,*) IDUM, JUH2GEN
          READ (IUNIT,*) IDUM, JUFUSION
          READ (IUNIT,*) IDUM, JUWIND
         CASE(606)
! ----------------------------------------------------------
! -- READ IN NUMBER OF SUBSECTORS IN ELECTRICITY SECTOR --
! ----------------------------------------------------------
!     Same for all regions
          READ (IUNIT,*) NU
         MsgStr = "NU read-in. Not used. Is NNU correct in input?"
         Call MCLog(1,MsgStr,0,0,0,0)
         CASE(607)
! ---------------------------------------------------------------------
! -- READ IN TRANSPORTATION PASSENGER ENERGY INTENSITY BY MODE AND FUEL
! ---------------------------------------------------------------------
     
          NewTransp = 1	! Turn on new transport module - sjs 11/00
     
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) M
          READ (IUNIT,*)          
          DO J=1,NFUEL 
	       DO L=1,NL
                READ (IUNIT,*) IDUM,(TRANEFF(ISERV,I,J,L,M),I=1,NTMODE)
             END DO
          END DO
          READ (IUNIT,*)          
          READ (IUNIT,*)          
	    DO L=1,NL
             READ (IUNIT,*) IDUM,(TTECHCHG(ISERV,I,L,M),I=1,NTMODE)
          END DO
!         Assign energy intensity to future period with technical change
!         applied.		          
          DO J=1,NFUEL 
	       DO L=1,NL
                DO MM=M+1,NM
                   DO I=1,NTMODE
                      TRANEFF(ISERV,I,J,L,MM)=TRANEFF(ISERV,I,J,L,M) &
                      *(1.0-TTECHCHG(ISERV,I,L,M))**((MM-M)*15)
                   END DO
                END DO
             END DO
          END DO
         CASE(608)
! ---------------------------------------------------------------------
! -- READ IN TRANSPORTATION PASSENGER NON-FUEL BY MODE AND FUEL
! ---------------------------------------------------------------------
     
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) M
          READ (IUNIT,*)          
          DO J=1,NFUEL 
	       DO L=1,NL
                READ (IUNIT,*) IDUM,(TRANNFCT(ISERV,I,J,L,M),I=1,NTMODE)
                DO MM=M+1,NM
                   DO I=1,NTMODE
                      TRANNFCT(ISERV,I,J,L,MM)=TRANNFCT(ISERV,I,J,L,M)
                   END DO
                END DO
             END DO
          END DO
          
         CASE(609)
! ---------------------------------------------------------------------
! -- READ IN TRANSPORTATION PASSENGER LOAD FACTOR BY MODE FOR ALL FUELS
! ---------------------------------------------------------------------
     
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) M
          READ (IUNIT,*)          
	    DO L=1,NL
             READ (IUNIT,*) (TLOADFAC(ISERV,I,L,M),I=1,NTMODE)
             DO MM=M+1,NM
                DO I=1,NTMODE
                   TLOADFAC(ISERV,I,L,MM)=TLOADFAC(ISERV,I,L,M)
                END DO
             END DO
          END DO
         CASE(611)          
! -----------------------------------------------------------------
! -- READ IN TRANSPORTATION BASE YEAR PASSENGER SERVICE BY MODE 
! -- AND BY FUEL (FOR PERIOD M=1)
! -----------------------------------------------------------------
     
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) M
          READ (IUNIT,*)          
	    DO I=1,NTMODE+1
		   DO L=1,NL
                READ (IUNIT,*)(TRANSERV(ISERV,I,J,L,M),J=1,NFUEL+1)
             END DO
          END DO
          
         CASE(612)          
! -----------------------------------------------------------------
! -- READ IN TRANSPORTATION TECHNOLOGY SCALER BY FUEL FOR EACH MODE
!    THIS VARIABLE CAN BE READ IN MULTIPLE TIMES TO SET SCALER FOR
!    NEW TECHNOLOGIES
! -----------------------------------------------------------------
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) M
          READ (IUNIT,*)          
          DO I=1,NTMODE
	       DO L=1,NL
                READ (IUNIT,*)IDUM,(SHRWTS(ISERV,I,J,L,M),J=1,NFUEL)
                DO MM=M+1,NM
                   DO J=1,NFUEL
                      SHRWTS(ISERV,I,J,L,MM)=SHRWTS(ISERV,I,J,L,M)
                   END DO  
                END DO  
             END DO
          END DO
          
         CASE(613)
! ---------------------------------------------------------------------
! -- READ IN TRANSPORTATION INCOME ELASTICITY BY MODE
! ---------------------------------------------------------------------
     
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) MI
          READ (IUNIT,*)          
	    DO L=1,NL
             READ (IUNIT,*) (TYELAS(ISERV,I,L,MI),I=1,NTMODE+1)
	    END DO
          READ (IUNIT,*) MF
          READ (IUNIT,*)          
	    DO L=1,NL
             READ (IUNIT,*) (TYELAS(ISERV,I,L,MF),I=1,NTMODE+1)
	    END DO
!         CONVERT INT TO REAL AND INTERPOLATE BETWEEN PERIODS
!         TMI IS THE NUMBER OF INTERVALS
          TMI=MF-MI
	    DO L=1,NL
             DO I=1,NTMODE+1
                DO MM=1,(MF-MI-1)
                   TMM=MM
                   TYELAS(ISERV,I,L,MI+MM)=XNTERP(TYELAS(ISERV,I,L,MI), &
                   TYELAS(ISERV,I,L,MF),TMI,TMM)
                END DO
!               ASSIGN TO REMAINING PERIODS
	          IF (MF.LT.NM) THEN
	             DO MMM=MF+1,NM
	                TYELAS(ISERV,I,L,MMM)=TYELAS(ISERV,I,L,MF)
				 END DO
	          END IF
             END DO
          END DO
         CASE(614)
! ---------------------------------------------------------------------
! -- READ IN TRANSPORTATION POP DENSITY ELASTICITY BY MODE
! ---------------------------------------------------------------------
     
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) M
          READ (IUNIT,*)          
	    DO L=1,NL
             READ (IUNIT,*) (TLELAS(ISERV,I,L,M),I=1,NTMODE)
             DO MM=M+1,NM
                DO I=1,NTMODE
                   TLELAS(ISERV,I,L,MM)=TLELAS(1,I,L,M)
                END DO
             END DO
          END DO
         CASE(615)
! ---------------------------------------------------------------------
! -- READ IN TRANSPORTATION PRICE ELASTICITY BY MODE
! ---------------------------------------------------------------------
     
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) M
          READ (IUNIT,*)          
	    DO L=1,NL
             READ (IUNIT,*) (TPELAS(ISERV,I,L,M),I=1,NTMODE+1)
             DO MM=M+1,NM
                DO I=1,NTMODE+1
                   TPELAS(ISERV,I,L,MM)=TPELAS(ISERV,I,L,M)
                END DO
             END DO
          END DO
         CASE(616)
! ---------------------------------------------------------------------
! -- READ IN TRANSPORTATION AVERAGE TRAVEL SPEED BY MODE
! ---------------------------------------------------------------------
     
          READ (IUNIT,*) ISERV
          READ (IUNIT,*) M
          READ (IUNIT,*)          
	    DO L=1,NL
             READ (IUNIT,*) (TSPEED(ISERV,I,L,M),I=1,NTMODE)
             DO MM=M+1,NM
                DO I=1,NTMODE
                   TSPEED(ISERV,I,L,MM)=TSPEED(ISERV,I,L,M)
                END DO
             END DO
          END DO
         CASE(617)
! ---------------------------------------------------------------------
! -- READ IN PERCENTAGE OF TOTAL POPULATION THAT ARE LICENSED (AUTO)
! ---------------------------------------------------------------------
     
	    DO L=1,NL
             READ (IUNIT,*) (TLICENSE(L,M),M=1,NM)
          END DO
! ------------------------------------------------------------------
	   CASE(700)
!	----- Read a cost curve for mitigation of other gases ----
		   READ (IUNIT,*) IXT,IX
		   READ (IUNIT,*)
		   DO L=1,NL
			 READ (IUNIT,*) IDUM, NLEVCCURV(IXT,IX,L), &
                              CCURV(IXT,IX,1:NLEVCCURV(IXT,IX,L),1,L)
			 READ (IUNIT,*) IDUM,IDUMR, &
                              CCURV(IXT,IX,1:NLEVCCURV(IXT,IX,L),2,L)
		   END DO
	   CASE(701)
!	------ read toggle for including other gases in emissions ----
		   READ (IUNIT,*) IOTHGASTOG

	   CASE(702)
!	------ read number of steps for phasing in below 0 part of ccurve ----
		   READ (IUNIT,*) IBZSTEPS

	   CASE(703)
!	------ read first year ccurves have an effect ----
		   READ (IUNIT,*) IBZFIRST


       CASE(704)
!   ------ read sensitivity of curve to energy prices
!          default is 0, 1 indicates direct movement with gas/electric prices ----

        READ (IUNIT,*) inumlines, NRegionstemp
		READ(IUNIT,*) idmy,idmy,idmy, (TempRegNumber(L),L=1,NRegionstemp)
		READ(IUNIT,*)
		DO I=1, inumlines
			READ(IUNIT,*) igas, II, tempStr, (CCURVADJ(igas,II,3,TempRegNumber(LL)),LL=1,NRegionstemp)
		END DO


       CASE(705)
!   ------ read ccurve tech change dim 1 (makes reductions cheaper over time)
!          default is 0, 0.01 means 1% cheaper per year ----

        READ (IUNIT,*) inumlines, NRegionstemp
		READ(IUNIT,*) idmy,idmy,idmy, (TempRegNumber(L),L=1,NRegionstemp)
		READ(IUNIT,*)
		DO I=1, inumlines
			READ(IUNIT,*) igas, II, tempStr, (CCURVADJ(igas,II,1,TempRegNumber(LL)),LL=1,NRegionstemp)
		END DO

       CASE(706)
!   ------ read ccurve tech change dim 2 (increases max reduction available)
!          default is 0, 0.01 means 1% fewer un-mitigatable emissions per year ----

        READ (IUNIT,*) inumlines, NRegionstemp
		READ(IUNIT,*) idmy,idmy,idmy, (TempRegNumber(L),L=1,NRegionstemp)
		READ(IUNIT,*)
		DO I=1, inumlines
			READ(IUNIT,*) igas, II, tempStr, (CCURVADJ(igas,II,2,TempRegNumber(LL)),LL=1,NRegionstemp)
		END DO

	   CASE(707)
!	------ read toggle for getting free reductions (regardless of iothgas) ----
		   READ (IUNIT,*) IFREEREDUX


! ------------------------------------------------------------------
! ------------------------------------------------------------------
	   CASE(800)  !other gases
!	------ read gas labels, gwp's, type of input ----
		DO I = 1, NOGMax
			READ (IUNIT,*) IDUM, OGLabel(I), tempgwp, temptype, tempfmax, tempgdp0, temptau, temptechc

!           Now check to see if this is a new gas or if inputs have already been read in:
!           If inputs have already been read, the data from the old case 800 is used;
!           if no inputs for this gas have been read yet, it will set the following
!           parameters.  mrj 10-02

			IF (SUM(OGREPORT(I,:)).EQ.0.0) THEN
			    OGGWP(I,:) = tempgwp  ! all periods the same intitially
			    OGTYPE(I,:,:) = temptype  ! all sources the same initially
				OGGDPPARMS(I,:,1,:) = tempfmax  ! all gdp controls same initially
				OGGDPPARMS(I,:,2,:) = tempgdp0
				OGGDPPARMS(I,:,3,:) = temptau
				OGGDPPARMS(I,:,4,:) = temptechc
			END IF			
		
		END DO
			

	   CASE(801) 
!	------ read a set of base emissions or coefficients ----
! modified 7/15 to allow more flexible readin
		READ (IUNIT,*) igas, inumlines, NRegionstemp, temptype
		READ(IUNIT,*) idmy,idmy, (TempRegNumber(L),L=1,NRegionstemp)
		READ(IUNIT,*)
		DO I=1, inumlines
			READ(IUNIT,*) II, OGSrcLabel(igas, II), (OGINPUT(igas,II,TempRegNumber(LL)),LL=1,NRegionstemp)
		    OGREPORT(igas,II) = 1  ! switch this gas+source ON
		    DO J=1,NRegionstemp
			   OGTYPE(igas,II,TempRegNumber(J)) = temptype
			END DO
		END DO

		IF (igas.eq.2) THEN   ! special case to convert Tg N inputs to Tg gas for consistency
			OGINPUT(igas,1:inumlines,:) = OGINPUT(igas,1:inumlines,:) * NtoN2O
		END IF


	   CASE(802) 
!	------ read a gamma for gas and source by region ----
		READ (IUNIT,*) igas, isrc
		READ(IUNIT,*)
		DO L=1,NL
			READ(IUNIT,*) IDUM, OGGAMMA(igas,isrc,L)
		END DO		


	   CASE(803)
!------- READ CALIBRATION CONTROL FRACTIONS ("user" controls)
		READ (IUNIT,*) igas, isrc
		READ (IUNIT,*)
		DO L=1,NL
		  READ (IUNIT,*) IDUM,(OGCTRLS(igas,isrc,3,L,KK),KK=2,NM) 
		END DO


	   CASE(804) 
!	------ read an input type for a gas/source by region (default read in case 800) ----
		READ (IUNIT,*) igas, isrc
		READ(IUNIT,*)
		DO L=1,NL
			READ(IUNIT,*) IDUM, OGTYPE(igas,isrc,L)
		END DO		


	   CASE(805) 
!	------ read a set of gdp control parms by region for a gas/source ----
		READ (IUNIT,*) igas, isrc
		READ(IUNIT,*)
		DO L=1,NL
			READ(IUNIT,*) IDUM, OGGDPPARMS(igas,isrc,1:3,L)
		END DO		


	   CASE(806) 
!	------ read a set of gdp control parms by region for a gas/source, with techchange ----
		READ (IUNIT,*) igas, isrc
		READ(IUNIT,*)
		DO L=1,NL
			READ(IUNIT,*) IDUM, OGGDPPARMS(igas,isrc,1:4,L)
		END DO		


	   CASE(807) 
!	------ read a set of gdp control parms for a gas by source (all regions at once), with techchange ----
		READ (IUNIT,*) igas, inumlines
		READ(IUNIT,*)
		DO I=1,inumlines
			READ(IUNIT,*) isrc, tempStr, OGGDPPARMS(igas,isrc,1:4,1)
			OGGDPPARMS(igas,isrc,1,:) = OGGDPPARMS(igas,isrc,1,1) ! set them all equal (but there must be a better way)
			OGGDPPARMS(igas,isrc,2,:) = OGGDPPARMS(igas,isrc,2,1)
			OGGDPPARMS(igas,isrc,3,:) = OGGDPPARMS(igas,isrc,3,1)
			OGGDPPARMS(igas,isrc,4,:) = OGGDPPARMS(igas,isrc,4,1)
		END DO		

!	do we want this even for the coefficients?  mj 8/02
!		IF (igas.eq.2) THEN   ! special case to convert Tg N inputs to Tg gas for consistency
!			OGCNVRG(igas,1:inumlines,:) = OGCNVRG(igas,1:inumlines,:) * NtoN2O
!		END IF


	   CASE(808) 
!	------ read GDP coefficients by region ---- sjs -- 01/03
		READ (IUNIT,*) igas, tempNRegions
		READ(IUNIT,*)
		DO LTemp=1,tempNRegions
			READ(IUNIT,*) L, temp1, temp2, temp3
			OGGDPPARMS(igas,:,1,L) = temp1
			OGGDPPARMS(igas,:,2,L) = temp2
			OGGDPPARMS(igas,:,3,L) = temp3
		END DO		


	   CASE(810) 
!	------ read coefficient to converge to (disabled if read value is <= 0) ----
! same format as case 801
		READ (IUNIT,*) igas, inumlines, NRegionstemp
		READ(IUNIT,*) idmy,idmy, (TempRegNumber(L),L=1,NRegionstemp)
		READ(IUNIT,*)
		DO I=1, inumlines
			READ(IUNIT,*) II, tempStr, (OGCNVRG(igas,II,TempRegNumber(LL)),LL=1,NRegionstemp)
		END DO

!	do we want this even for the coefficients?  mj 8/02
!		IF (igas.eq.2) THEN   ! special case to convert Tg N inputs to Tg gas for consistency
!			OGCNVRG(igas,1:inumlines,:) = OGCNVRG(igas,1:inumlines,:) * NtoN2O
!		END IF


	   CASE(811)  !gwp's differing by period
!	------ overwrites gwp's from case 800, which must already be read ----
		DO I = 1, NOGMax
			READ (IUNIT,*) IDUM, tempStr, (OGGWP(I,KK),KK=2,NM)		
		END DO

		

! ------------------------------------------------------------------
!     read in 1990 end-use energy demand for calibration
         CASE(1001)	! End-use sectors
          DoCalib = 1.
          DO L=1,NL
               READ (IUNIT,*) IDUM, K,(fdemand(L, k, j),J=1,5)  
          END DO
  
         CASE(1002)	! Electric generation sector
          DO L=1,NL
               READ (IUNIT,*) IDUM, K,(fdemand(L, k, j),J=1,7)  
          END DO
  
!     read in 1990 electric generation efficiency for calibration
         CASE(1003)	
          DO L=1,NL
               READ (IUNIT,*) IDUM,(egeneff90(L, j),J=1,7)  
          END DO
  
!     read in GNP for calibration
         CASE(1004)	
          READ (IUNIT,*) IDUM,M,gnpcalib(1,M)
          DO L=2,NL
               READ (IUNIT,*) IDUM,IDUM,gnpcalib(L,M)
          END DO
          
!     read in Total PE for calibration for one period
         CASE(1005)	
          READ (IUNIT,*) IDUM,M,PEcalib(1,M)
          DO L=2,NL
               READ (IUNIT,*) IDUM,IDUM,PEcalib(L,M)
          END DO
          
!     alt read in cal GNP from 2005 onward -- read in all at once
         CASE(1006)	
          DO L=1,NL
               READ (IUNIT,*) IDUM,(gnpcalib(L,M), M = 3,9)
          END DO
          
!     alt read in Total PE cal from 2005 onward -- read in all at once
         CASE(1007)	
           DO L=1,NL
               READ (IUNIT,*) IDUM,(PEcalib(L,M), M = 3,9)
          END DO
          
!     Read-in 1990 CO2 calibration values
         CASE(1008)	
           DO I=1,3
               READ (IUNIT,*) IDUM,CO2Calb(I)
          END DO
          
!     read in Total FE for calibration for one period
         CASE(1010)	
          FECalib = 1	! Switch to calibrating final energy, not primary energy
          READ (IUNIT,*) IDUM,M,PEcalib(1,M)
          DO L=2,NL
               READ (IUNIT,*) IDUM,IDUM,PEcalib(L,M)
          END DO
          
!     alt read in Total FE cal from 2005 onward -- read in all at once
         CASE(1011)	
           FECalib = 1	! Switch to calibrating final energy, not primary energy
           DO L=1,NL
               READ (IUNIT,*) IDUM,(PEcalib(L,M), M = 3,9)
          END DO
          
!     read in BC and OC 1990 Radiative Forcing Values
         CASE(1020)	
         READ (IUNIT,*) FBC1990
         READ (IUNIT,*) FOC1990
          
         CASE(1021)	
         READ (IUNIT,*) FSO2_dir1990
         READ (IUNIT,*) FSO2_ind1990
          
! ------------------------------------------------------------------
         CASE DEFAULT
     
     	MsgStr = "No readin instructions exist for variable #:"
        Call MCLog(2,MsgStr,0,0,0,1d0*ivarnum)
     
      END SELECT
         
      CALL NEXTTABLE(TABLEMRK,INDIC)  
      
      
      END DO  
      
      
      CLOSE(1)
      IF (verbflag .eq. 1) write (*,*) "Done Erbinput"
      RETURN
      END      



!*********************************************************************

!  .  Subroutine for parsing to comma  or blank 
!  .  maw 7/9/93

      SUBROUTINE PARSECOM(CH)
!*********************************************************************

      CHARACTER*40 CH
      I=INDEX(CH,',')-1
      IF(I.NE.-1) THEN
         CH=CH(:I)
      ELSE 
         I=INDEX(CH,' ')-1
         IF(I.NE.-1) CH=CH(:I)
      END IF
      RETURN
      END



!*********************************************************************

!  .  Subroutine to advance to next input table.  maw 7/9/93
!  .  Gets file to one line past the marker.  Returns 1 if successful,
!  .  returns a 0 on end-of-file.

      SUBROUTINE NEXTTABLE(TABLEMRK,INDIC)

!*********************************************************************
      CHARACTER*11 TABLEIN,TABLEMRK

 3000 FORMAT(A11)
      READ(1,3000,END=10) TABLEIN
      ICNT=0
      DO WHILE (TABLEIN.NE.TABLEMRK .AND. ICNT.LT.1000)
         ICNT = ICNT+1
         READ(1,3000,END=10) TABLEIN
      END DO
      INDIC=1
      RETURN

! . . On end of file
   10 CONTINUE
      INDIC=0
      RETURN

      END


!*********************************************************************
