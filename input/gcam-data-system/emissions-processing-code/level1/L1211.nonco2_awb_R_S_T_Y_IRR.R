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
logstart( "L1211.nonco2_awb_R_S_T_Y_IRR.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical AWB emissions by region/GLU, GCAM commodity, and irrigation level" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L161.ag_irrProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_GLU" )
L161.ag_rfdProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_GLU" )
L121.nonco2_tg_R_awb_C_Y_GLU <- readdata( "EMISSIONS_LEVEL1_DATA", "L121.nonco2_tg_R_awb_C_Y_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Downscaling emissions to irrigated and rainfed technologies on the basis of production" )
printlog( "First, computing irrigated and rainfed production shares, within region/GLU/crop" )
L161.ag_irrProd_Mt_R_C_Y_GLU[irr] <- "IRR"
L161.ag_rfdProd_Mt_R_C_Y_GLU[irr] <- "RFD" 
L1211.ag_Prod_Mt_R_C_Y_GLU_irr <- rbind( L161.ag_irrProd_Mt_R_C_Y_GLU, L161.ag_rfdProd_Mt_R_C_Y_GLU)

# Aggregate to get the total by region/GLU/crop
L1211.ag_Prod_Mt_R_C_Y_GLU <- aggregate( L1211.ag_Prod_Mt_R_C_Y_GLU_irr[ X_historical_years ],
                                         by = L1211.ag_Prod_Mt_R_C_Y_GLU_irr[ R_C_GLU ], sum )

# Divide to get the share of irr/rfd within region/GLU/crop
L1211.ag_irrShare_R_C_Y_GLU_irr <- L1211.ag_Prod_Mt_R_C_Y_GLU_irr
L1211.ag_irrShare_R_C_Y_GLU_irr[ X_historical_years ] <- L1211.ag_Prod_Mt_R_C_Y_GLU_irr[ X_historical_years ] /
  L1211.ag_Prod_Mt_R_C_Y_GLU[
    match( vecpaste( L1211.ag_irrShare_R_C_Y_GLU_irr[ R_C_GLU ] ),
           vecpaste( L1211.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ] ) ),
    X_historical_years ]
L1211.ag_irrShare_R_C_Y_GLU_irr[ is.na( L1211.ag_irrShare_R_C_Y_GLU_irr ) ] <- 0

printlog( "Then, multiply emissions by region/GLU/crop/nonCO2 by irr/rfd production shares" )
# Emissions by R_C_GLU_irr = emissions by R_C_GLU * irrShare
L1211.nonco2_tg_R_awb_C_Y_GLU_IRR <- repeat_and_add_vector( L121.nonco2_tg_R_awb_C_Y_GLU, irr, c( "IRR", "RFD" ) )
L1211.nonco2_tg_R_awb_C_Y_GLU_IRR[ X_EDGAR_historical_years ] <- L1211.nonco2_tg_R_awb_C_Y_GLU_IRR[ X_EDGAR_historical_years ] *
  L1211.ag_irrShare_R_C_Y_GLU_irr[
    match( vecpaste( L1211.nonco2_tg_R_awb_C_Y_GLU_IRR[ R_C_GLU_irr ] ),
           vecpaste( L1211.ag_irrShare_R_C_Y_GLU_irr[ R_C_GLU_irr ] ) ),
    X_EDGAR_historical_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1211.nonco2_tg_R_awb_C_Y_GLU_IRR <- c( "Ag waste burning emissions by GCAM region / commodity / GLU / irrigation level / historical year", "Unit = Tg" )
comments.L1211.ag_irrShare_R_C_Y_GLU_irr <- c( "Irrigated and rainfed production shares by GCAM region / commodity / GLU / historical year", "Unitless" )

#write tables as CSV files
writedata( L1211.nonco2_tg_R_awb_C_Y_GLU_IRR, domain="EMISSIONS_LEVEL1_DATA", fn="L1211.nonco2_tg_R_awb_C_Y_GLU_IRR", comments=comments.L1211.nonco2_tg_R_awb_C_Y_GLU_IRR )
writedata( L1211.ag_irrShare_R_C_Y_GLU_irr, domain="EMISSIONS_LEVEL1_DATA", fn="L1211.ag_irrShare_R_C_Y_GLU_irr", comments=comments.L1211.ag_irrShare_R_C_Y_GLU_irr )

# Every script should finish with this line
logstop()
