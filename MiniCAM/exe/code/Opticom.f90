!***********************************************************************
!
!     COMMON Blocks for MiniCAM Optimizer
!
!     Marshall Wise  9-29-95
!
!	  rewritten into module form 7/01 ktg
!***********************************************************************      
MODULE OPTICOM
	  
IMPLICIT NONE

INTEGER,PARAMETER ::	 &
	NLPMax = 18,		 & ! Max number of regions likely to use
	NMP = 9,			 & ! Maximum number of periods 
	NLP = 14,			 & ! Number of periods we are using
	ISTEPS = 5             ! number of points to use when making cost curve

REAL(8) & 
	irate,				&
	
	GNP(NLPMax,NMP),	& ! Start of OPT1 Common Block
	CARB(NLPMax,NMP),	&
	TAX(NLPMax,NMP),	&
	CUMTARG(NLPMax),	&
	GNPD(NMP),			&
	CARBD(NMP),			&
	DX,					&
	PVGNPBI,			&
	SHADOW(3),			&
	WHICHTARG(NLPMax),  &
	DefaultTax(NLPMax,NMP), &
	
	EMISSSTEPS(0:ISTEPS,NLPMax,NMP), & ! Start of COSTVARS Common Block
	TAXSTEPS(0:ISTEPS,NLPMax,NMP),			&
	MCEMISS(NLPMax,NMP),&
	MCTAX(NLPMax,NMP)


INTEGER &
    txstart,			&
	txstart2(NLPMax),	& ! New version with different tax starting date for each region  - sjs 06/01
	IDERIV,				&
	NTAXES,				&
	LG,					&
	LGMEMBC(NLPMax,NLPMax),&
	NLINGC(NLPMax),		&
	NLG,				&
	NL,					&
	INDOPT,				&

	INFILES_opt,		& ! Number of input files to be read in - sjs 02/01 changed name so diff than erb

	OPTRUNID,			& ! Start of COSTVARS Common Block
	COSTCALC,			&
	SUMYEAR

CHARACTER*72 OptFILES(10) ! Array of file names --- any changes to these names need to be reflected in proc CloseLog
CHARACTER*80 MsgStr		  ! For convienience. String to use to pass msgs to log routine.
CHARACTER*72 OPTDBNAME


! sjs - Changed 9/00 so that number of regions is input parameter, not hard coded

END MODULE OPTICOM