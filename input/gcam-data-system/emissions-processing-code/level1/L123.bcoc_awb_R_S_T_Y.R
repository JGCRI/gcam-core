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
logstart( "L123.bcoc_awb_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical BC/OC AWB emissions by GCAM technology, computed from RCP emissions data" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )
L121.AWBshare_R_C_Y_GLU <- readdata( "EMISSIONS_LEVEL1_DATA", "L121.AWBshare_R_C_Y_GLU" )
RCP_BC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_BC_2000" )
RCP_OC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_OC_2000" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute total regional production" )
L123.AWBshare_R_C_2000_GLU <- subset( L121.AWBshare_R_C_Y_GLU, year == "X2000")

printlog( "Compute RCP emissions by region" )
RCP_BC_2000$Non.CO2 <- "BC_AWB"
RCP_OC_2000$Non.CO2 <- "OC_AWB"
L123.RCP <- rbind( RCP_BC_2000, RCP_OC_2000 )
L123.RCP$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L123.RCP$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L123.RCP <- L123.RCP[ c( "GCAM_region_ID", "Non.CO2", "awb" ) ]

#Convert to Tg
L123.RCP$awb <- L123.RCP$awb * kg_to_tg 

printlog( "Compute emissions factors by GCAM region" )
L123.bcoc_tgmt_R_awb_2000.melt <- repeat_and_add_vector( L123.AWBshare_R_C_2000_GLU, "Non.CO2", c( "BC_AWB", "OC_AWB" ) )
L123.bcoc_tgmt_R_awb_2000.melt$total_emiss <- L123.RCP$awb[
  match( vecpaste( L123.bcoc_tgmt_R_awb_2000.melt[ c( "GCAM_region_ID", "Non.CO2" )]),
         vecpaste( L123.RCP[ c( "GCAM_region_ID", "Non.CO2" )] ) )]
L123.bcoc_tgmt_R_awb_2000.melt$emissions <- with( L123.bcoc_tgmt_R_awb_2000.melt, total_emiss * AWB_emiss_share )

printlog( "BC and OC are represented by emissions coefficients rather than emissions, as only one base year is available" )
printlog( "Matching production to emissions in the given year" )
L123.bcoc_tgmt_R_awb_2000.melt$prod <- L103.ag_Prod_Mt_R_C_Y_GLU$X2000[
  match( vecpaste( L123.bcoc_tgmt_R_awb_2000.melt[ R_C_GLU ] ),
         vecpaste( L103.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ] ) ) ]
L123.bcoc_tgmt_R_awb_2000.melt$emfact <- with( L123.bcoc_tgmt_R_awb_2000.melt, emissions / prod )

#Where production and emissions are zero, re-set NaN coefficients to 0
L123.bcoc_tgmt_R_awb_2000.melt$emfact[ is.na( L123.bcoc_tgmt_R_awb_2000.melt$emfact ) ] <- 0

#Only write out the ID columns and the emissions factor
L123.bcoc_tgmt_R_awb_2000 <- L123.bcoc_tgmt_R_awb_2000.melt[ c( R_C_GLU, "Non.CO2", "emfact" ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L123.bcoc_tgmt_R_awb_2000 <- c( "BC/OC ag waste burning emissions factors by GCAM region / 2000", "Unit = Tg / Mt" )

#write tables as CSV files
writedata( L123.bcoc_tgmt_R_awb_2000, domain="EMISSIONS_LEVEL1_DATA", fn="L123.bcoc_tgmt_R_awb_2000", comments=comments.L123.bcoc_tgmt_R_awb_2000 )

# Every script should finish with this line
logstop()
