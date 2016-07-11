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
logstart( "L171.nonco2_awb_R_S_T_Y_IRR.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical AWB emissions by GCAM technology, computed from EDGAR emissions data" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
L161.ag_irrProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_AEZ" )
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
L161.ag_irrProd_Mt_R_C_Y_AEZ$irr <- "IRR"
L161.ag_rfdProd_Mt_R_C_Y_AEZ$irr <- "RFD" 
L171.ag_Prod_Mt_R_C_Y_AEZ <- rbind( L161.ag_irrProd_Mt_R_C_Y_AEZ, L161.ag_rfdProd_Mt_R_C_Y_AEZ)
L171.ag_Prod_Mt_R_C_Y_AEZ <- subset( L171.ag_Prod_Mt_R_C_Y_AEZ, L171.ag_Prod_Mt_R_C_Y_AEZ$GCAM_commodity %!in% c( "Pasture", "Forest" ))
L171.ag_Prod_Mt_R_C_Y_AEZ.melt <- melt( L171.ag_Prod_Mt_R_C_Y_AEZ, id.vars=c("GCAM_region_ID", "GCAM_commodity", "irr", "year" ))
L171.ag_Prod_Mt_R_Y <- aggregate( L171.ag_Prod_Mt_R_C_Y_AEZ.melt$value, by=as.list( L171.ag_Prod_Mt_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "year" ) ] ), sum )
L171.ag_pct_R_C_Y_AEZ.melt <- L171.ag_Prod_Mt_R_C_Y_AEZ.melt
L171.ag_pct_R_C_Y_AEZ.melt$total_prod <- L171.ag_Prod_Mt_R_Y$x[ match( vecpaste( L171.ag_pct_R_C_Y_AEZ.melt[ c( "GCAM_region_ID", "year" )]), vecpaste( L171.ag_Prod_Mt_R_Y[ c( "GCAM_region_ID", "year" )]))]
L171.ag_pct_R_C_Y_AEZ.melt$prod_share <- L171.ag_pct_R_C_Y_AEZ.melt$value / L171.ag_pct_R_C_Y_AEZ.melt$total_prod
names( L171.ag_pct_R_C_Y_AEZ.melt )[ names( L171.ag_pct_R_C_Y_AEZ.melt ) == "variable" ] <- "AEZ"

printlog( "Compute EDGAR emissions by region" )
EDGAR_SO2$Non.CO2 <- "SO2_AWB"
EDGAR_CO$Non.CO2 <- "CO_AWB"
EDGAR_NOx$Non.CO2 <- "NOx_AWB"
EDGAR_VOC$Non.CO2 <- "NMVOC_AWB"
EDGAR_CH4$Non.CO2 <- "CH4_AWB"
EDGAR_N2O$Non.CO2 <- "N2O_AWB"
EDGAR_NH3$Non.CO2 <- "NH3_AWB"
EDGAR_VOC <- EDGAR_VOC[ names( EDGAR_VOC ) %!in% c( "X2009", "X2010" )]
L171.EDGAR <- rbind( EDGAR_SO2, EDGAR_CO, EDGAR_NOx, EDGAR_VOC, EDGAR_CH4, EDGAR_N2O, EDGAR_NH3 )
L171.EDGAR$sector <- EDGAR_sector$agg_sector[ match( L171.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L171.EDGAR$iso <- EDGAR_nation$iso[ match( L171.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L171.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L171.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L171.EDGAR <- L171.EDGAR[ names( L171.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L171.EDGAR <- na.omit( L171.EDGAR )
L171.EDGAR <- aggregate( L171.EDGAR[ X_EDGAR_historical_years ], by=as.list( L171.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector")]), sum)
L171.EDGAR.melt <- melt( L171.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ))

#Convert to Tg
L171.EDGAR.melt$value <- L171.EDGAR.melt$value / 1000.0 

#Subset for AWB
L171.EDGAR_awb.melt <- subset( L171.EDGAR.melt, L171.EDGAR.melt$sector == "ag_waste_burning" )

printlog( "Compute emissions by GCAM region, commodity, and AEZ" )
L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt <- L171.ag_pct_R_C_Y_AEZ.melt[ names( L171.ag_pct_R_C_Y_AEZ.melt ) %in% c( R_C_AEZ, "irr", "year", "prod_share" )]
L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt$xyear <- paste( "X", L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt$year, sep="")
L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt$prod_share[ is.na( L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt$prod_share ) ] <- 0
L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt <- repeat_and_add_vector( L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt, "Non.CO2", awb_gases )
L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt$total_emiss <- L171.EDGAR_awb.melt$value[ match( vecpaste( L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt[ c( "GCAM_region_ID", "Non.CO2", "xyear" )]), vecpaste( L171.EDGAR_awb.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt$emissions <- L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt$total_emiss * L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt$prod_share

#Reshape
L171.nonco2_tg_R_awb_C_Y_AEZ_IRR <- dcast( L171.nonco2_tg_R_awb_C_Y_AEZ_IRR.melt, GCAM_region_ID + Non.CO2 + GCAM_commodity + irr + AEZ ~ xyear, value.var = c( "emissions" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L171.nonco2_tg_R_awb_C_Y_AEZ_IRR <- c( "Ag waste burning emissions by GCAM region / commodity / AEZ / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L171.nonco2_tg_R_awb_C_Y_AEZ_IRR, domain="EMISSIONS_LEVEL1_DATA", fn="L171.nonco2_tg_R_awb_C_Y_AEZ_IRR", comments=comments.L171.nonco2_tg_R_awb_C_Y_AEZ_IRR )

# Every script should finish with this line
logstop()
