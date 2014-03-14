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
logstart( "L121.nonco2_awb_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y_AEZ" )
EDGAR_SO2 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_SO2" )
EDGAR_CO <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CO" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )
EDGAR_VOC <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NMVOC" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )
EDGAR_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NH3" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute share of regional production by technology" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- subset( L104.ag_Prod_Mt_R_C_Y_AEZ, L104.ag_Prod_Mt_R_C_Y_AEZ$GCAM_commodity %!in% c( "Pasture", "Forest" ))
L121.ag_Prod_Mt_R_Y <- aggregate( L104.ag_Prod_Mt_R_C_Y_AEZ[ X_historical_years ], by=as.list( L104.ag_Prod_Mt_R_C_Y_AEZ[ c( "GCAM_region_ID" ) ] ), sum )
L121.ag_Prod_Mt_R_Y.melt <- melt( L121.ag_Prod_Mt_R_Y, id.vars=c( "GCAM_region_ID" ) )
L121.ag_pct_R_C_Y_AEZ.melt <- melt( L104.ag_Prod_Mt_R_C_Y_AEZ, id.vars=c( "GCAM_region_ID", "GCAM_commodity", "AEZ" ))
L121.ag_pct_R_C_Y_AEZ.melt$total_prod <- L121.ag_Prod_Mt_R_Y.melt$value[ match( vecpaste( L121.ag_pct_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), vecpaste( L121.ag_Prod_Mt_R_Y.melt[ c( "GCAM_region_ID", "variable" )]))]
L121.ag_pct_R_C_Y_AEZ.melt$prod_share <- L121.ag_pct_R_C_Y_AEZ.melt$value / L121.ag_pct_R_C_Y_AEZ.melt$total_prod

printlog( "Compute EDGAR emissions by region" )
EDGAR_SO2$Non.CO2 <- "SO2_AWB"
EDGAR_CO$Non.CO2 <- "CO_AWB"
EDGAR_NOx$Non.CO2 <- "NOx_AWB"
EDGAR_VOC$Non.CO2 <- "NMVOC_AWB"
EDGAR_CH4$Non.CO2 <- "CH4_AWB"
EDGAR_N2O$Non.CO2 <- "N2O_AWB"
EDGAR_NH3$Non.CO2 <- "NH3_AWB"
EDGAR_VOC <- EDGAR_VOC[ names( EDGAR_VOC ) %!in% c( "X2009", "X2010" )]
L121.EDGAR <- rbind( EDGAR_SO2, EDGAR_CO, EDGAR_NOx, EDGAR_VOC, EDGAR_CH4, EDGAR_N2O, EDGAR_NH3 )
L121.EDGAR$sector <- EDGAR_sector$agg_sector[ match( L121.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L121.EDGAR$iso <- EDGAR_nation$iso[ match( L121.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L121.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L121.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L121.EDGAR <- L121.EDGAR[ names( L121.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L121.EDGAR <- na.omit( L121.EDGAR )
L121.EDGAR <- aggregate( L121.EDGAR[ X_EDGAR_historical_years ], by=as.list( L121.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector")]), sum)
L121.EDGAR.melt <- melt( L121.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ))

#Convert to Tg
L121.EDGAR.melt$value <- L121.EDGAR.melt$value / 1000.0 

#Subset for AWB
L121.EDGAR_awb.melt <- subset( L121.EDGAR.melt, L121.EDGAR.melt$sector == "ag_waste_burning" )

printlog( "Compute emissions by GCAM region, commodity, and AEZ" )
L121.nonco2_tg_R_awb_C_Y_AEZ.melt <- L121.ag_pct_R_C_Y_AEZ.melt[ names( L121.ag_pct_R_C_Y_AEZ.melt ) %in% c( R_C_AEZ, "variable", "prod_share" )]
L121.nonco2_tg_R_awb_C_Y_AEZ.melt <- repeat_and_add_vector( L121.nonco2_tg_R_awb_C_Y_AEZ.melt, "Non.CO2", awb_gases )
L121.nonco2_tg_R_awb_C_Y_AEZ.melt$total_emiss <- L121.EDGAR_awb.melt$value[ match( vecpaste( L121.nonco2_tg_R_awb_C_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste( L121.EDGAR_awb.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L121.nonco2_tg_R_awb_C_Y_AEZ.melt$emissions <- L121.nonco2_tg_R_awb_C_Y_AEZ.melt$total_emiss * L121.nonco2_tg_R_awb_C_Y_AEZ.melt$prod_share

#Reshape
L121.nonco2_tg_R_awb_C_Y_AEZ <- dcast( L121.nonco2_tg_R_awb_C_Y_AEZ.melt, GCAM_region_ID + Non.CO2 + GCAM_commodity + AEZ ~ variable, value = c( "emissions" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L121.nonco2_tg_R_awb_C_Y_AEZ <- c( "Ag waste burning emissions by GCAM region / commodity / AEZ / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L121.nonco2_tg_R_awb_C_Y_AEZ, domain="EMISSIONS_LEVEL1_DATA", fn="L121.nonco2_tg_R_awb_C_Y_AEZ", comments=comments.L121.nonco2_tg_R_awb_C_Y_AEZ )

# Every script should finish with this line
logstop()
