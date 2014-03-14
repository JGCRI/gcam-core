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
printlog( "Historical emissions by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
L104.ag_Prod_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y_AEZ" )
RCP_BC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_BC_2000" )
RCP_OC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_OC_2000" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute total regional production" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- subset( L104.ag_Prod_Mt_R_C_Y_AEZ, L104.ag_Prod_Mt_R_C_Y_AEZ$GCAM_commodity %!in% c( "Pasture", "Forest" ))
L123.ag_Prod_Mt_R_Y <- aggregate( L104.ag_Prod_Mt_R_C_Y_AEZ[ X_historical_years ], by=as.list( L104.ag_Prod_Mt_R_C_Y_AEZ[ c( "GCAM_region_ID" ) ] ), sum )
L123.ag_Prod_Mt_R_Y.melt <- melt( L123.ag_Prod_Mt_R_Y, id.vars=c( "GCAM_region_ID" ) )
L123.ag_Prod_Mt_R_2000.melt <- subset( L123.ag_Prod_Mt_R_Y.melt, L123.ag_Prod_Mt_R_Y.melt$variable == "X2000" )

printlog( "Compute RCP emissions by region" )
RCP_BC_2000$Non.CO2 <- "BC_AWB"
RCP_OC_2000$Non.CO2 <- "OC_AWB"
L123.RCP <- rbind( RCP_BC_2000, RCP_OC_2000 )
L123.RCP$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L123.RCP$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L123.RCP <- L123.RCP[ names( L123.RCP ) %in% c( "GCAM_region_ID", "Non.CO2", "awb" ) ]

#Convert to Tg
L123.RCP$awb <- L123.RCP$awb * kg_to_tg 

printlog( "Compute emissions factors by GCAM region" )
L123.bcoc_tgmt_R_awb_2000.melt <- L123.ag_Prod_Mt_R_2000.melt
L123.bcoc_tgmt_R_awb_2000.melt <- repeat_and_add_vector( L123.bcoc_tgmt_R_awb_2000.melt, "Non.CO2", c( "BC_AWB", "OC_AWB" ) )
L123.bcoc_tgmt_R_awb_2000.melt$total_emiss <- L123.RCP$awb[ match( vecpaste( L123.bcoc_tgmt_R_awb_2000.melt[ c( "GCAM_region_ID", "Non.CO2" )]), vecpaste( L123.RCP[ c( "GCAM_region_ID", "Non.CO2" )] ) )]
L123.bcoc_tgmt_R_awb_2000.melt$emfact <- L123.bcoc_tgmt_R_awb_2000.melt$total_emiss / L123.bcoc_tgmt_R_awb_2000.melt$value

#Reshape
L123.bcoc_tgmt_R_awb_2000 <- L123.bcoc_tgmt_R_awb_2000.melt[ names( L123.bcoc_tgmt_R_awb_2000.melt ) %!in% c( "total_emiss", "value" )]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L123.bcoc_tgmt_R_awb_2000 <- c( "BC/OC ag waste burning emissions factors by GCAM region / 2000", "Unit = Tg / Mt" )

#write tables as CSV files
writedata( L123.bcoc_tgmt_R_awb_2000, domain="EMISSIONS_LEVEL1_DATA", fn="L123.bcoc_tgmt_R_awb_2000", comments=comments.L123.bcoc_tgmt_R_awb_2000 )

# Every script should finish with this line
logstop()
