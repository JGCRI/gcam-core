!     COMMON BLOCK FOR ERB (ENERGY) PORTION OF MINICAM
!     (Ag common block below)
! sjs - Changed 9/00 so that number of regions is input parameter, not hard coded
! ktg - Converted into module form 7/01
     
MODULE COMMON

	  IMPLICIT NONE

      CHARACTER*72 FILES(40),CASENAME,CASENOTE,DBNAME,OUTFILE,AGNAME,AGNOTE

	  LOGICAL ifPopRate                                         
      
      INTEGER,PARAMETER ::  &
       NLPMax = 18,    & !Number of regions - SJS set to max number of regions likely to use 
       NNLPMax = 19,   & !W/ slot for world totals 
       NMRKP = 50,  & !Maximum number of markets 
	   NINP=12,     & !Maximum number of inputs or market goods 
       NMP = 9,     & !Maximum number of periods  
       NJP=6,       & !number of secondary fuel categories 
       NKP=3,       & !Number of end-use sectors 
       NNKP=5,      & !Number of energy-consuming sectors (w/ utilities) 
       NIP=7,       & !Number of primary energy types 
       NUP=16,      & !Max number of electricity technologies. (02/01 -- changed. actual number set by readin) 
       NUUP=12,     & !Max number of electric plant types (02/01 -- changed. actual number set in NSET & erbinput) 
       NISSP=3,     & !Max number of subsectors 
       NHP=12,       & !Number of hydrogen input fuels or sources 
       NVINP=6,     & !Number of vintages for forests 
       NJAP=4,      & !Number of agricultural products   
       NTSERV=2,    & !Number of transportation service, passenger and freight 
       NTMODE=7,    & !Number of transportation service modes 
       NFUEL=NJP,	& !Number of fuel types for transportation modes
	   AGMODEL=1,	& !Toggle the AgLU model between the old(0) and the new(1) version
       NOGMax=25,	& !Max number of other gases in the model
       NOGSrcMax=30,& !Max number of sources for each gas
       NCCPMax=15     !Max number of cost curve points in read-ins
       
     !To add the 2000 deflator, I started with the uncited 1997 number below, 
     !and combined it with the OECD deflator for the US between 1997 and 2000. 
     !(OECD Macro Statistics Nov 2002)   mrj 11/2002  (Note that the existing
     !90 and 97 numbers are lower than OECD or BLS statistics -- but I left
     !them unchanged for consistency)
	  REAL(8),PARAMETER :: &
	   CVRT90=2.212, & !GDP deflator from 1975$ to 1990$   
       CVRT97=2.67,  & !GDP deflator from 1975$ to 1997$  
       CVRT00=2.80,  & !GDP deflator from 1975$ to 2000$  mrj 11/02
       BtuToJ=1055.0,&   !Btu to Joules conversion
       CO2toC=0.272916, & !C as % weight of CO2           mrj 1/02
       NtoN2O=1.571132    !convert N wgt to N2O wgt       mrj 1/02
 
	  CHARACTER*8 REGNAMES(NNLPMax)
	  CHARACTER*80 MsgStr ! String to use to pass msgs to log routine.

	  INTEGER M,L ! M=time period, L=region we are working with

      REAL(8) &
	  
	   ! RE Common Block
	   BSJKLM(NJP,NKP,NLPMax,NMP),RPJ(NJP),RPJK(NJP,NKP),RPK, &
       RPKK(NKP),RYJ(NJP), &
       RYJK(NJP,NKP),RYKL(2),RYKK(NKP),SJKL(NJP,NKP,NLPMax), &
       ZLM(NNLPMax,0:NMP), &
      
	   ! REPN Common Block
	   BPSLM(NLPMax,NMP),BPKL(NKP,NLPMax),BSUILM(NUP,NLPMax,NMP), &
       GNP(NLPMax,NMP), &
       GNPLM(NNLPMax,NMP),GNPBL(NLPMax),HIJ(NJP),HJ(NJP),HJK(NJP,NKP), &
       RPKL(NKP,NLPMax),PHYDRO(NLPMax),RUI(NUUP),RYL(NLPMax),SJKLP(NJP,NKP,NLPMax), &
       TEST, &
       TXILM(NJP,NLPMax,NMP),TXJKLM(NJP,NKP,NLPMax,NMP), &
      
	   ! RES Common Block
	   BESILM(NIP,NLPMax,NMP),ESRL1M(NLPMax,NMP),ESRL2M(NLPMax,NLPMax), &
       BSCIL(2,NLPMax),CIL(NIP,NLPMax),ESR1M(NMP),ESR2M(NMP), &
       ESIL1M(NIP,NLPMax,NMP),ESIL2M(NIP,NLPMax,NMP),ESRILM(NUP,NLPMax,NMP), &
       FLRL(3,NLPMax),HCILT(4,2,NLPMax,3),RCI(4,2),SFLRL(NLPMax),RYSHT, &
       EBRSLM(NLPMax,NMP),RESOURCE(NIP,NLPMax), &

	   ! DREPT Common Block
       BSKL(NKP,NLPMax),GJ(NJP),GJK(NJP,NKP),PAUIL(NJP,NLPMax), &
       PCJILM(4,2,NLPMax,NMP), &
       SynShareJILM(4,2,NLPMax,NMP), &	! Share of end-use consumption of fuel I by Synthetic J; sjs 08/01
       PJLM(NJP,NLPMax,NMP),PJKLM(NJP,NKP,NLPMax,NMP),PKLM(NKP,NLPMax,NMP), &
       PUILM(NUUP,NLPMax,NMP),TKL(NKP,NLPMax),PUKLM(NKP,NLPMax,NMP), &
      
	   ! DOSPER Common Block
	   EDIL(NIP,NLPMax),EDIKL(NIP,NNKP,NLPMax), &
       EDRIKL(NIP,NNKP,NLPMax),EFJKL(NJP,NNKP,NLPMax),ESIL(NUP,NLPMax), &
       FJKL(NJP,NKP,NLPMax), &
       FJL(NJP,NLPMax),GCI(4,2),GIJ(NJP),HYDRO(5,NLPMax), &
       PILM(NUP,NLPMax,NMP),PROL(NLPMax), &
       SCIL(4,NLPMax),SUIL(NUP,NLPMax),TESIL(NLPMax), &
       YLM(NLPMax,0:NMP),YLMCAP(NLPMax,0:NMP), &
       BIOLM(NLPMax,1),BIOPSM(5,2,1), &

	   ! Out1 Common Block
       CEFLM(NLPMax,NMP),CEFM(NMP),CONLM(NLPMax,NMP),CONM(NMP), &
       EDM(NMP), &
       EDIM(NUP,NMP),EDKL(NNKP,NLPMax),EDLM(NLPMax,NMP),EDRM(NMP),EDRKM(NMP),   &
       EDRIM(NIP,NMP),EDRIKM(NIP,NMP),EDRIL(NIP,NLPMax), &
       EDRKLM(NNKP,NLPMax,NMP),EDRLM(NLPMax,NMP), &
       EFM(NMP),EFJM(NJP,NMP),EFJL(NJP,NLPMax),EFKL(NNKP,NLPMax), &
       EFLM(NLPMax,NMP),EPCLM(NNLPMax,NMP),EPGLM(NNLPMax,NMP), &

	   ! Out2 Common Block
       ESM(NMP),ESIM(NIP,NMP),ESLM(NLPMax,NMP),ESRM(NMP), &
       ESRIM(NIP,NMP), &
       ESI1M(NIP,NMP),ESI2M(NIP,NMP),ESL1M(NLPMax,NMP),ESL2M(NLPMax,NMP), &
       ESRLM(NLPMax,NMP), &
       ESUM(NMP),ESUIM(NUP,NMP),ESUILM(NUP,NLPMax,NMP),ESULM(NLPMax,NMP), &
       EXM(NMP),EXIL(NIP,NLPMax),EXIM(NIP,NMP),EXLM(NLPMax,NMP), &
       GNPFLM(NNLPMax,NMP),GNPPCM(NNLPMax,NMP),SYNM(NMP),SYNIM(NIP,NMP), &
       SYNILM(NIP,NLPMax,NMP),SYNLM(NLPMax,NMP),SYNJIM(4,NIP,NMP)
      
	  INTEGER &
	   
	   ! PCP Common Block
	   NF,NSYN,NI,NIG,NIM,NBIP,NIS,NNIS,NJ,NJUMP,NKKL(NLPMax), &
       NKL(NLPMax),NNU,NU,NStype,	& ! Added NStype -- sjs, 02/01 
       NKMAX,NKKMAX,NL,NM,NMREAD,NMKT,MAXAGN,NCO2,NOPT(10), &
       NSTEP,NPUNT

	  REAL(8) &

	   ! RC Common Block
       COI(7),SBURNL(NLPMax,3),SFEDIL(3,NLPMax),SHALE(NLPMax,3), &
       CO2IM(11,NMP),CO2LM(NLPMax,NMP),CO2M(NMP),CO2IIM(11,NMP), &

	   ! RHM Common Block
       SVENTL(NLPMax,3),CMSWL(NLPMax), &
       CICWL(NLPMax), &
       SBIO(NLPMax,NMP), &
       EBIOL(NLPMax), &
       RPBIOL(NLPMax), &
       RYBIOL(NLPMax), &

	   ! RS Common Block
       QISLM(5,NLPMax,NMP),RIGISL(6,5,NLPMax),CIGIS(7,6),VISL(5,NLPMax), &
       RIL(5,NLPMax),STISM(5,NMP),BESIL(5,NLPMax),ESFIL(5,NLPMax), &
       RFosExpan(3,NLPMax,NMP)		! sjs -- 03/02. Var to control fossil supply limit expansion

	  INTEGER &
	   ! IOCNTRL Common Block
       IUNIT,IOUNIT,JUNIT, &

	   ! OPT Common Block
       JOPT(2),MALL,KOPT, &

	   ! NRegions Common Block
       NLP,NNLP, & !Store number of regions separately so can also use in Opticom.for
      
	   ! ERCTRL Common Block
       INFILES, &
       INFILE0

	  REAL(8) &
	   ! EG Common Block
       SOLUSE(2,6), &

	   ! TEM Common Block
       TPIM(3,NMP), &

	   ! TAX Common Block
       TXISLM(5,NLPMax,NMP),TXUILM(NUP,NLPMax,NMP),VISLM(5,NLPMax,NMP), &
       GUILM(NUP,NLPMax,NMP),HUILM(NUP,NLPMax,NMP), &
       
       ! new vars for dist losses & generation efficiency. sjs -- 04/02
       GenEff(NUP,NLPMax,NMP),DistLoss(NLPMax,NMP), &
              
       ! T&D Costs
       TRI(NJP), PUTDKM(NKP,NMP), PH2TDKM(NKP,NMP), &

	   ! PRTLOT Common Block
       CO2ILM(11,NLPMax,NMP),CO2IILM(6,NLPMax,NMP), &
       CO2DLM(NLPMax,NMP),CO2DM(NMP),CO2JKLM(6,6,NNLPMax,NMP), &
       CO2KLM(6,NNLPMax,NMP),CO2LMT(NNLPMax,NMP), &
       CH4ILM(7,NLPMax,NMP),EDILM(NIP,NLPMax,NMP), &
       ESILM(NUP,NLPMax,NMP), &
       EFJLM(NJP,NLPMax,NMP),EDRILM(NIP,NLPMax,NMP),EXILM(NIP,NLPMax,NMP), &
       EDRIKLM(NIP,NNKP,NNLPMax,NMP), &

	   ! NEWELAS Common Block
       RYKLM(NKP,NLPMax,NMP),RYJKLM(NJP,NKP,NLPMax,NMP), &

	   ! EMF Common Block
       PROLM(NLPMax,NMP),TKLM(3,NLPMax,NMP),TALPHA(3,NLPMax,NMP), &
       RBIGKLM(3,NLPMax,NMP), &
       RPARTKM(3,NMP),PARTICPT(NLPMax,NMP),EFKLM(NNKP,NLPMax,NMP), &
       RPKLM(NKP,NLPMax,NMP), &
       TKLHOLD(NKP,NLPMax,NMP),EFKM(NNKP,NMP), &

	   ! IPCC Common Block
       GJKLM(NJP,NKP,NLPMax,NMP),HJKLM(NJP,NKP,NLPMax,NMP), &
       FJKLM(NJP,NKP,NLPMax,NMP), &
       FJKM(NJP,NKP,NMP),TAXRLM(NLPMax,NMP),TXUILM0(NIP,NLPMax,NMP), &
       TXJKLM0(NJP,NKP,NLPMax,NMP),TXISLM0(5,NLPMax,NMP),taxlm(NLPMax,NMP), &
       gnpfm(NMP),&

       ! CO2TARGS Common Block
	   CEMTARGS(NLPMax,NMP),EXOFLARE(NLPMax,NMP), &

	   ! OPTI Common Block
       CFBSTOP(NMP),CO2MG(NMP),CO2DLMG(NLPMax,NMP)

	  INTEGER &
	   NTAXMODE(NLPMax), &
	   
       ! MARKT1 Common Block
       NMRK,MRK,MRKDEF(0:NMRKP,20), &
       INOIL,INGAS,INCOAL,INBMASS,INCROP,INLSTOCK,INFOREST,INCARB,NIN, &
	   IBMASS,MODL,NNI,NNJ,ICARBOUT(NLPMax,2),IPROTOC, &
	   JUOIL,JUGAS,JUCOAL,JUNUC,JUSOLPV,JUHYDRO,JUBMASS, &
       JUCSCRUB,JUOSCRUB,JUGSCRUB,JUH2GEN,JUFUSION,JUWIND, &
	   IPFIX(NINP,NLPMax,NMP),JSBMASS, &

	   ! New AgLU Market good indices
	   INFFOREST,		& ! Forward Forest Prds
	   INPAST,			& ! Pasture
	   INFOODGR,		& ! Food Grains
	   INCOARSEGR,		& ! Coarse Grains
	   INOILCROPS,		& ! Oil Crops
	   INMISCCROPS		  ! Misc Crops
	


	  REAL(8) &
	   
	   ! MARKET1 Common Block (continued)
	   MRKDEM(NMRKP),MRKPRD(NMRKP), &
       AGDEM(NINP,NLPMax),AGSUP(NINP,NLPMax),igused(nip,NLPMax,nmp), &
       SYNFUEL(4,2,NNLPMax,NMP),SYNINPUT(4,NNLPMax,NMP), &
       SYNLoss(4,2,NNLPMax,NMP),	& ! Track Syn losses. sjs 01/01 
       PSSU(NJP,NISSP),BSSUILM(NJP,NISSP,NLPMax,NMP),RUISS(NJP,NISSP), &
       PSSUM(NJP,NISSP,NLPMax,NMP),SSUIL(NJP,NISSP,NLPMax),SSJKL(NISSP), &
       RSSPJK(NKP,NISSP),PSSJKLM(NKP,NISSP), &
       BSSJKLM(NKP,NISSP,NLPMax,NMP),P(NINP,NLPMax,NMP), &
       CARBSEQM(4,NMP),CARBDISP(NLPMax,NMP), &
       REMFRAC(2,NLPMax),CARBSEQ(4,NLPMax,NMP),REMFRACP(2,NLPMax), &      
       ESERV(NNLPMax,NMP),ESERVK(NKP,NNLPMax,NMP), & 
       CSEQbyFuel(3,3,NLPMax,NMP),	&    	! Carb seq by process (elec,synf,H2) and fuel
       ECapLim(NUP), &	! Var for capacity limts. sjs - 1/01
     
       ! HYRDOGEN Common Block
	   H2FRAC(NLPMax),GIJH2(NJP), &
       PHILM(NHP,NLPMax,NMP),GHILM(NHP,NMP),HHILM(NHP,NMP), &
       SSHIL(NHP,NISSP,NLPMax),BSSHILM(NHP,NISSP,NLPMax,NMP),PSSH(NHP,NISSP), &
       ESHILM(NHP,NLPMax,NMP), &
       SHIL(NHP,NLPMax),BSHILM(NHP,NMP),RHI(NHP),ESHIM(NHP,NMP)
	   
	  INTEGER &
       JSH2,JHBMASS,JHELCTRO,NH2,KH2,NNH2, NH2electPtype, & ! sjs-- parameter to determine how price for electricity used for electrolytic H2 is determined
	   NH2_New	! number of "new" H2 technologies

	  REAL(8) &
	   ! JUN97 Common Block
	   PPPCONV(NLPMax),GNPMRKT(NNLPMax,NMP), &
       CARBFUEL(NJP,NNLPMax,NMP),GNPPPP(NNLPMax,NMP),SULFIN(NMP), & !mrj 7/98

	   ! DEMOG2 Common Block
       DemoStatMC(10,NNLPMax,nMp),   & !  Demographic statistics  
       WAgeMC(2,NNLPMax,nMp),        & !  working age population 
       GNPind(NLPMax,nMp),           & !  Index for GNP by working age method 
       RLFP(NLPMax,nMp),             & !  Rate of Labor Force Participation   
       PLFPerc(NLPMax,nMp)             !  % of pop participating in labor force -- sjs 12/00, for new gdp calc 
      
	  INTEGER & 
	   LFPercMethod                    !  Flag to turn on new method 

	  REAL(8) &
	   ! HIGH GWP Common Block  - mrj 8/98 
       HGWPREAD(14,NLPMax,NMP),	    & ! Read in high GWP gases to any period 
       HGWPGWP(14),	                & ! High GWP's by region 
       HGWPHFC(NLPMax,NMP),	        &
       HGWPPFC(NLPMax,NMP),	        &
       HGWPSF6(NLPMax,NMP),         &	             


       !TRANSPORTATION Common Block  -  SHK 1/00 
       TRANSERV(NTSERV,NTMODE+1,NFUEL+1,NLPMax,0:NMP), & !transportation service 
       TRANFUEL(NTSERV+1,NTMODE+1,NFUEL+1,NLPMax,NMP), & !transportation fuel cons 
       TRANCOST(NTSERV,NTMODE+1,NFUEL+1,NLPMax,NMP),   & !transportation cost 
       TRANEFF(NTSERV,NTMODE+1,NFUEL+1,NLPMax,NMP),    & !transportation efficiency 
       TRANFLCT(NTSERV,NTMODE+1,NFUEL+1,NLPMax,NMP),   & !transportation Fuel Cost 
       TRANNFCT(NTSERV,NTMODE+1,NFUEL+1,NLPMax,NMP),   & !transportation Non-Fuel C 
       TLOADFAC(NTSERV,NTMODE+1,NLPMax,NMP),           & !transportation load factor p/v 
       TPELAS(NTSERV,NTMODE+1,NLPMax,NMP),             & !transportation mode price elasticity 
       TYELAS(NTSERV,NTMODE+1,NLPMax,NMP),             & !transportation income elasticity 
       TLELAS(NTSERV,NTMODE,NLPMax,NMP),               & !transportation pop density elasticity 
       TSPEED(NTSERV,NTMODE,NLPMax,NMP),               & !transportation avg. travel speed 
       TTECHCHG(NTSERV,NTMODE+1,NLPMax,NMP),           & !transportation tech. chng. 
       SHRWTS(NTSERV,NTMODE,NFUEL,NLPMax,NMP),         & !trans tech share weights 
       TLICENSE(NLPMax,NMP),                           & !% of total pop that are licensed 
       TRANCOSTUL(NLPMax,NMP)                            !transportation cost unlicensed 
      
	  INTEGER &
	   IPPPInten, & !flag to specify that intensity limits are for PPP based GDPs -- sjs 04/02
	   NewTransp  !flag to turn on new transportation calcuations -- sjs 11/00
!
!       Combine agriculture common block to main common block. shk 8/99
!***********************************************************************
	
	
	! dbout Common
        
	  INTEGER & 
       vl(0:99,0:99,0:99), &
       rvl(5000), &
	   dbaridx, &
	   gblvar(5000), &	! var to flag variables to be output only globally
	   GenRelease		! Flag to indicate limited variable output
	  
	  CHARACTER(15) &
       ctstr(99), &
       sctstr(99,99)
       


!     Note:  some arrays with time indices are dimensioned from 0:NLPMax
!     for the region index since region 0 is used in the input sub
!     to indicate that the end of an input table has been reached.


	  INTEGER &

!  Globally-known indexes and other integers.
       JCROP,JLSTOCK,JBMASS,JFOREST, &
       IVMAX,IVIN,MITER,NMITER,JUNMAN
  
	  
	  REAL(8) &

!     Variables (SSSS1 Common)
       CJ(NJAP,NLPMax,NMP),        & ! profit rate per unit land 
       EFFEED(NLPMax),             & !  Tonnes livestock per tonne feed 
       ETOBIO(0:NLPMax,0:NMP),     & ! Energy output per unit biomass input 
       COSTNL(NJAP,0:NLPMax,0:NMP), & ! Per unit non-land crop costs 
       QSJLM(NJAP,0:NNLPMax,0:NMP),& ! production of ag product j 
       RJ0(NJAP,NLPMax),           & ! Crop production calibration parameter 
       RJ(NJAP,NLPMax,NMP),        & ! land productivity for ag product j 
       SFCROP(0:NLPMax,0:NMP),     & ! Share of livestock feed from crops 
       XNJ(NJAP,0:NLPMax,0:NMP),   & ! Fertilizer application      
       CROPPHYS(NLPMax,2),         & ! Conversion from dollar-weighted crops to physical 
       KJ1(NJAP,NLPMax,0:NMP),     & ! Crop management technology index multiplier 
       KJIMP1(NJAP,0:NLPMax,0:NMP),& ! Crop mgmt/tech (KJ) % annual improvement 
	   LVSTCOWSH(NLPMax,NMP),      & ! share of livestock production that is cows  mrj7/98    
       
	   ! LAND1 Common
       AHHAB(0:NLPMax,0:NMP),    & ! coefficient for human habitat land demand 
       ALJ(NJAP,0:NLPMax,0:NMP), & ! Logit coefficient for ag land market share 
       BHHAB(0:NLPMax,0:NMP),    & ! exponent for human habitat land demand 
       AUNMAN(0:NLPMax,0:NMP),   & ! exponent for share mngd. an unmngd. land 
       CAVG(0:NLPMax,0:NMP),     & ! Average return to managed land 
       QL(0:NLPMax,0:NMP),       & ! total land area in region l 
       QLAG(NLPMax,NMP),         & ! land available for agriculture 
       QLFOREST(NNLPMax,NMP),    & ! land area in managed forestland 
       QLHHAB(NNLPMax,NMP),      & ! Land area for human habitat 
       QLJLM(NJAP,NNLPMax,NMP),  & ! land allocated to ag product j in region l 
       QLPARKS(0:NLPMax,0:NMP),  & ! land area for parks, etc. not available 
       QLUNMAN(NNLPMax,0:NMP),   & ! unmanaged land area in region l 
       RHO(NLPMax),              & ! land market share logit exponent 
       SLJ(NJAP,NLPMax)            ! share of land for ag product j

	  INTEGER &
       ! FOREST1 Common
       IVINSTAR(NLPMax,NMP),     & ! Optimal vintage to harvest 
       IYSTAR(NLPMax,NMP)          ! Optimal year within optimal vintage to harvest 
       
	  REAL(8) &
	   QLFV(0:NVINP,NLPMax,NMP), & ! Land in forests in by vintage 
       TRSTOCK(0:NVINP,NLPMax,NMP), & ! Stock of trees by vintage 
       GRCOEFF(NLPMax),          & ! Calibrating coefficient for growth curve 
       DRATE(NLPMax,0:NMP),      & ! Discount rate used in forests

       ! EMIT Common
       CARBDENS(NJP,NLPMax),     & !Carbon density by land type 
       CARBLAND(NNLPMax,NMP),    & !Carbon emissions from land use change 
       AGCH4ACT(3,NNLPMax), AGN2OACT(3,NNLPMax),& !agriculture ch4 and n2o emissions (not used anymore 11/02 mj)
       BSEMIS(6,NLPMax), RICEPCT(NLPMax), FYRATIO(NLPMax), CYRATIO(NLPMax), &
       SHRUMIN(NLPMax,NMP), & !global

       ! FISH1 Common
       FISHGRO, & ! Annual growth rate of global fish stock 
       FISHM(0:NMP), & ! Global stock of fish at time period M 
       FISHSLOP, & ! Slope term in the fish production equation 
       FISHTMP, & ! Fish stock temporary variable (within model iteration) 
       QSFISHM(NMP), & ! Production of fish in period M
     
       ! PRICES1
       PAJLM(0:NJAP,NLPMax,0:NMP),  & ! Market price of marketed product j 
       PPJLM(NJAP,NLPMax,NMP),      & ! Regional price paid for j 
       PRJLM(NJAP,NLPMax,NMP),      & ! Regional price received for j 
       TRCOST(NJAP),                & ! Transportation costs 
       SUBSEV(NJAP,0:NLPMax,0:NMP), & ! Producer subsidy or (severance tax) 
       TAX(NJAP,0:NLPMax,0:NMP),    & ! Tax on price paid.
     
       ! DEMAND1
       AJ(NJAP,NLPMax), & ! Income elasticity of demand 
       BJ(NJAP,NLPMax), & ! Price elastcity of demand 
       D0J(NJAP,NLPMax), & ! Demand coefficient 
       QDCB(NNLPMax,NMP), & ! Demand for crops for livestock feed 
       QDJLM(NJAP,NNLPMax,0:NMP), & ! Final demand for ag product j 
       XMEATADJ(NLPMax,NMP)     ! % Decrease in meat demand used to scale down

	  INTEGER &
       ! SOLVE1
       MKID(NJAP)  ! Market id number for each product

      REAL(8) &
       ! CRSPNSE
       CO2RSP0(NJAP,NLPMax), & ! co2 response calibrator  
       RMAX(NJAP,NLPMax), & ! Max precip for climate function 
       RMIN(NJAP,NLPMax), & ! Min precip for climate function 
       TMAX(NJAP,NLPMax), & ! Max temperature for climate function 
       TMIN(NJAP,NLPMax), & ! Min temperature for climate function    
	   TPRSP0(NJAP,NLPMax)  ! Temp-Precip response calibrator 

	  INTEGER &
       INDCO2,  & ! Indicator variable for CO2 response 
       INDTMPR, & ! Indicator variable for temperature response 
       INDPREC    ! Indicator variable for precipitation response
          
      REAL(8) &
	   ! LINKS1
       POPLM(0:NNLPMax,0:NMP), &! population in region l 
       CO2CONC(0:NMP), &! atmospheric carbon dioxide concentration 
       PRECIP(0:NLPMax,0:NMP), &!  regional precipitation 
       TEMPR(0:NLPMax,0:NMP), & ! average temperature in region l
          
       ! MEATDMD
       AMEATSCL(NLPMax,NMP)

!     COMMON for MRJ's New Sulfur Emissions

      REAL(8) &
	   
	   ! sulfur	   - mrj 5/00 
       SO2SCT(4,3,NLPMax),			&! sulfur content 
       SO2IPBASE(NLPMax),			&! ind procs base ems 
       SO2TRBIOB(NLPMax),			&! trad biomass burn 
       SO2ASHR(4,NLPMax),			&! ash content 
       SO2ENDN(4,3,NLPMax),			&! energy density 
       SO2TAU(NLPMax),				&! "tau" parm 
       SO2GDP0(NLPMax),				&! "gdp0" parm 
       SO2CNTRLADJ(NLPMax),			&! Adjustment to emissions control level 
       SO2TauADJ(NLPMax),			&! Adjustment to emissions control timing 
       SO2MAXCTRL(4,3,NLPMax),		&! max control percentage 
       SO2IPMR(NLPMax),				&! ind procs, max reduction 
       SO2PLG(NLPMax),				&!  
       SO2EM(NLPMax,NMP),			&! holds emissions 
       SO2CTRL(4,4,NLPMax,NMP),		&! holds control level -- inc. index to hold biom - sjs 11/00 
       SO2EMSJLM(4,4,NLPMax,NMP),	&! holds detailed ems 
       SO2INDPROC(NLPMax,NMP),		&! ind proc ems 
       SO2TRBIO(NLPMax,NMP),			&! trad biomass ems 
       SO2BIOBURN(NLPMax,NMP),		&! biomass burnign ems 
       SO2SHIP(NMP),				&! shipping emissions 
       SO2EMGLBL(NMP),			&! global so2 em 
       SYNUSE(NLPMax,NMP),			&! synfuel consumption as percent of liquids 
       SO2BUNKF(NLPMax),				&!  
       SO2IPCTRL(NLPMax,NMP),		&! industrial procs control percent 
       SO2ELECBIO(NLPMax,NMP),		&!  
       SO2PETSCLIM(4,NLPMax),		&! petroleum sulfur content limit 
       SO2ALTLIQ,					&! toggle for alt liquids code 
       SO2MAGREG(NLPMax,3),			&! region mapping for MAGICC's 3 sulfur regions
	   SRESCALB(8,2),			    &! Standardized 1990 and 200 values (needed for SRES, also useful to produce more realistic CO2 conc's) 
       SO2_Cntrl_TechChange(NLPMax)	! reduction in control "GDP_O" over time
	  
	  INTEGER & 
       MagCalb,				& ! Flag to standardize emiss before MAGICC (SRES) 
       ISRESeserDmd,        & ! Flag for alternative E demand calc used in SRES ver
       CarbConstraintType,	& ! Flag: if =1 constraint is total emissions; (future:) =2 ... is CO2 conc; =3 ... is tot forcing; =4 ... is GHG forcing; sjs-11/01
       UserDeforestEm,		& ! Flag: if set, then deforestation emissions are read in, AgLU figures are not used in MAGICC. sjs-02/02

	   ! ccurves - mrj 6/00
	   NLEVCCURV(NOGMax,NOGSrcMax,NLPMax),	& ! number of points on the curve (max NCCPMax) 
	   IOTHGASTOG,              & ! toggle to switch between co2 only and tce
	   IFREEREDUX,              & ! toggle to turn free reductions on and off (regardless of iothgastog) mj11-02
	   IBZFIRST,                & ! first period reductions from cost curves appear
       IBZSTEPS                   ! number of steps for phasing in <0 redux

! Moved cement emissions from MAGLINK so that can be accessed elsewhere. sjs - 02/02  
! Also added ability to have hard-coded deforestation emissions  

     Real(8) &
      Cement(NMP),		& ! Global Cement Emissions
      DeForest(NMP)		  ! Global net deforestation emissions

	  Real(8) &				! Vars for passing emissions & results to/from MAGICC
	    MAGICCCResults(0:30,50), &	! Array of MAGICC results
	    MagEM(20,16)				! Array of MAGICC input emissions
	    
	  REAL(8) &
        
       CCURV(NOGMax,NOGSrcMax,NCCPMax,2,NLPMax), & ! main array 1 is ctax, 2 is reductions 
       CCURVADJ(NOGMax,NOGSrcMax,3,NLPMax), & ! cost curve adjustments: 1,2 are tech change, 3 is enrgy price sensitivity  mj 11/02
	   EMISSTCE(NLPMax,NMP),		& ! model emissions in TCE  
       

! OG  - mrj 6/02		Other Gases	common arrays

       OGEMISS(NOGMax,NOGSrcMax,NLPMax,NMP),	& ! emissions by gas and source 
       OGACT(NOGMax,NOGSrcMax,NLPMax,NMP),		& ! activity level by gas and source 
       OGCTRLS(NOGMax,NOGSrcMax,3,NLPMax,NMP),	& ! control levels (gdp, abatecurve, user)
       OGGDPPARMS(NOGMax,NOGSrcMax,4,NLPMax),	& ! gdp control parms (fmax, gdp0, tau, techchangepct) 
       OGCOEF(NOGMax,NOGSrcMax,NLPMax),			& ! coefficient in emissions equation
       OGGWP(NOGMax,NMP),			            & ! GWP's by gas and period
       OGINPUT(NOGMax,NOGSrcMax,NLPMax),        & ! input (either base yr emiss, or em coef) 
       OGGAMMA(NOGMax,NOGSrcMax,NLPMax),        & ! exponent for scaling activity levels
       OGREPORT(NOGMax,NOGSrcMax),              & ! switch used to determine if a gas-source is active
       OGCNVRG(NOGMax,NOGSrcMax,NLPMax),		& ! converge-to coefficient
	   COOLDEMAND(NLPMax)						  ! per-capita cooling demand -- sjs 03/03
		
      INTEGER &
       OGTYPE(NOGMax,NOGSrcMax,NLPMax),			& ! type of input above
       OGERROR(NOGMax,NOGSrcMax,NLPMax),		& ! holds error codes from emissions calculation
       NOG,										& ! max gas index used
       NOGSrc(NOGMax)							  ! max src index used (by gas)

	  CHARACTER(15) &

	   OGLabel(NOGMax), & ! labels for gases and sources	   
	   OGSrcLabel(NOGMax, NOGSrcMax)    ! labels for gases and sources


!      Calibration Common Block

	  REAL(8) &
	   fdemand(NLPMax,4,7),gnpcalib(NLPMax,NMP),  & ! Variables needed for calibration 
       egeneff90(NLPMax,7), PEcalib(NLPMax,NMP),  &
       DoCalib, FECalib								! Flag to turn on calibration proceedure

END MODULE COMMON

