# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "EMISSPROC_DIR" ) ){
    if( Sys.getenv( "EMISSIONSPROC" ) != "" ){
        EMISSPROC_DIR <- Sys.getenv( "EMISSIONSPROC" )
    } else {
        stop("Could not determine location of emissions data system. Please set the R var EMISSPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
logstart( "L1221.ghg_agr_R_S_T_Y_IRR.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical AGR emissions by region/GLU, GCAM commodity, and irrigation level" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L1211.ag_irrShare_R_C_Y_GLU_irr <- readdata( "EMISSIONS_LEVEL1_DATA", "L1211.ag_irrShare_R_C_Y_GLU_irr" )
L122.ghg_tg_R_agr_C_Y_GLU <- readdata( "EMISSIONS_LEVEL1_DATA", "L122.ghg_tg_R_agr_C_Y_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Downscaling emissions to irrigated/rainfed technologies on the basis of production shares" )
# Production shares were computed in a prior file and written out, so this can be done in one step
L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- repeat_and_add_vector( L122.ghg_tg_R_agr_C_Y_GLU, irr, c( "IRR", "RFD" ) )
L1221.ghg_tg_R_agr_C_Y_GLU_IRR[ X_EDGAR_historical_years ] <- L1221.ghg_tg_R_agr_C_Y_GLU_IRR[ X_EDGAR_historical_years ] *
  L1211.ag_irrShare_R_C_Y_GLU_irr[
    match( vecpaste( L1221.ghg_tg_R_agr_C_Y_GLU_IRR[ R_C_GLU_irr ] ),
           vecpaste( L1211.ag_irrShare_R_C_Y_GLU_irr[ R_C_GLU_irr ] ) ),
    X_EDGAR_historical_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- c( "Agriculture emissions by GCAM region / commodity / GLU / irrigation level / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L1221.ghg_tg_R_agr_C_Y_GLU_IRR, domain="EMISSIONS_LEVEL1_DATA", fn="L1221.ghg_tg_R_agr_C_Y_GLU_IRR", comments=comments.L1221.ghg_tg_R_agr_C_Y_GLU_IRR )

# Every script should finish with this line
logstop()
