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
logstart( "L131.nonco2_proc_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions from the processing sector by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
EPA_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_ghg_tech" )
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
EDGAR_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NH3" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )
EDGAR_VOC <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NMVOC" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )
EDGAR_SO2 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_SO2" )
EDGAR_CO <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CO" )
EPA_Ind <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_FCCC_IndProc_2005")

# -----------------------------------------------------------------------------
# 2. Perform computations
# Compute subsector share of sectoral emissions using EPA data
printlog( "Compute share of sectoral emissions in each subsector using EPA data" )
L131.EPA_nonco2_indproc <- EPA_Ind
L131.EPA_nonco2_indproc$sector <- EPA_tech$sector[ match( L131.EPA_nonco2_indproc$Source_Category, EPA_tech$Source_Category )]
L131.EPA_nonco2_indproc$subsector <- EPA_tech$fuel[ match( L131.EPA_nonco2_indproc$Source_Category, EPA_tech$Source_Category )]

#Remove unnecessary columns and melt
L131.EPA_nonco2_indproc <- L131.EPA_nonco2_indproc[ names( L131.EPA_nonco2_indproc ) %!in% c( "EPA_Source_Category_Raw", "Source_Category") ]
L131.EPA_nonco2_indproc.melt <- melt( L131.EPA_nonco2_indproc, id.vars=c( "sector", "subsector" ) )
L131.EPA_nonco2_indproc.melt <- na.omit( L131.EPA_nonco2_indproc.melt )
L131.EPA_nonco2_indproc.melt <- aggregate( L131.EPA_nonco2_indproc.melt$value, by=as.list( L131.EPA_nonco2_indproc.melt[ c( "sector", "subsector", "variable" ) ]), sum )

#Compute subsector share of sectoral total
L131.nonco2_pct_R_prc_S_S_2005 <- GCAM_sector_tech[ names( GCAM_sector_tech ) %in% c( "supplysector", "subsector", "stub.technology", "EPA_agg_sector", "EPA_agg_fuel_ghg", "EDGAR_agg_sector" )]
L131.nonco2_pct_R_prc_S_S_2005 <- subset( L131.nonco2_pct_R_prc_S_S_2005, L131.nonco2_pct_R_prc_S_S_2005$EDGAR_agg_sector %in% prc_sectors )
L131.nonco2_pct_R_prc_S_S_2005 <- repeat_and_add_vector( L131.nonco2_pct_R_prc_S_S_2005, "Non.CO2", c( "CH4", "N2O", "NMVOC", "NOx", "SO2", "CO", "VOC" ))
L131.nonco2_pct_R_prc_S_S_2005$tech_emissions <- L131.EPA_nonco2_indproc.melt$x[ match( vecpaste( L131.nonco2_pct_R_prc_S_S_2005[ c( "EPA_agg_sector", "EPA_agg_fuel_ghg", "Non.CO2" )]), vecpaste( L131.EPA_nonco2_indproc.melt[ c( "sector", "subsector", "variable" )] ) )]
L131.nonco2_pct_R_prc_S_S_2005$tech_emissions[ is.na( L131.nonco2_pct_R_prc_S_S_2005$tech_emissions ) ] <- 1
L131.nonco2_gg_R_prc_S_2005 <- aggregate( L131.nonco2_pct_R_prc_S_S_2005$tech_emissions, by=as.list( L131.nonco2_pct_R_prc_S_S_2005[ c( "EPA_agg_sector", "Non.CO2" )]), sum )
L131.nonco2_pct_R_prc_S_S_2005$sector_emissions <- L131.nonco2_gg_R_prc_S_2005$x[ match( vecpaste(L131.nonco2_pct_R_prc_S_S_2005[ c( "EPA_agg_sector", "Non.CO2" )] ), vecpaste( L131.nonco2_gg_R_prc_S_2005[ c( "EPA_agg_sector", "Non.CO2" )]))]
L131.nonco2_pct_R_prc_S_S_2005$tech_share <- L131.nonco2_pct_R_prc_S_S_2005$tech_emissions / L131.nonco2_pct_R_prc_S_S_2005$sector_emissions

printlog( "Disaggregate EDGAR emissions to subsectors" )
#First aggregate all EDGAR data and subset for processing sectors
EDGAR_CH4$Non.CO2 <- "CH4"
EDGAR_N2O$Non.CO2 <- "N2O"
EDGAR_VOC$Non.CO2 <- "NMVOC"
EDGAR_NOx$Non.CO2 <- "NOx"
EDGAR_SO2$Non.CO2 <- "SO2"
EDGAR_NH3$Non.CO2 <- "NH3"
EDGAR_CO$Non.CO2 <- "CO"
EDGAR_VOC <- EDGAR_VOC[ names( EDGAR_VOC ) %!in% c( "X2009", "X2010" ) ]
L131.EDGAR <- rbind( EDGAR_CH4, EDGAR_N2O, EDGAR_VOC, EDGAR_NOx, EDGAR_SO2, EDGAR_CO, EDGAR_NH3 )
L131.EDGAR$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L131.EDGAR$IPCC_description, EDGAR_sector$IPCC_description )]
L131.EDGAR$iso <- EDGAR_nation$iso[ match( L131.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L131.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L131.EDGAR$iso, iso_GCAM_regID$iso )]   
L131.EDGAR <- subset( L131.EDGAR, L131.EDGAR$EDGAR_agg_sector %in% prc_sectors )
L131.EDGAR <- L131.EDGAR[ names( L131.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "Name", "IPCC", "IPCC_description", "iso" )]
L131.EDGAR.melt <- melt( L131.EDGAR, id.vars=c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector" ))
L131.EDGAR.melt <- na.omit( L131.EDGAR.melt )
L131.EDGAR.melt <- aggregate( L131.EDGAR.melt$value, by=as.list( L131.EDGAR.melt[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "variable" ) ]), sum)
names( L131.EDGAR.melt )[ names( L131.EDGAR.melt ) == "x" ] <- "EDGAR_emissions"
L131.EDGAR.melt$EDGAR_emissions <- L131.EDGAR.melt$EDGAR_emissions * gg_to_tg

#Now, map in all data and compute emissions
L131.nonco2_tg_R_prc_S_S_Yh.melt <- GCAM_sector_tech[ names( GCAM_sector_tech ) %in% c( "supplysector", "subsector", "stub.technology", "EDGAR_agg_sector", "EPA_agg_sector", "EPA_agg_fuel_ghg" )]
L131.nonco2_tg_R_prc_S_S_Yh.melt <- subset( L131.nonco2_tg_R_prc_S_S_Yh.melt, L131.nonco2_tg_R_prc_S_S_Yh.melt$EDGAR_agg_sector %in% prc_sectors )
L131.nonco2_tg_R_prc_S_S_Yh.melt <- repeat_and_add_vector( L131.nonco2_tg_R_prc_S_S_Yh.melt, "Non.CO2", c( "CH4", "N2O", "NMVOC", "NOx", "SO2", "CO", "VOC" ))
L131.nonco2_tg_R_prc_S_S_Yh.melt$tech_share <- L131.nonco2_pct_R_prc_S_S_2005$tech_share[ match( vecpaste( L131.nonco2_tg_R_prc_S_S_Yh.melt[ c( "supplysector", "subsector", "stub.technology", "Non.CO2" )]), vecpaste( L131.nonco2_pct_R_prc_S_S_2005[ c( "supplysector", "subsector", "stub.technology", "Non.CO2" )]))]
L131.nonco2_tg_R_prc_S_S_Yh.melt <- repeat_and_add_vector( L131.nonco2_tg_R_prc_S_S_Yh.melt, "xyear", X_EDGAR_historical_years )
L131.nonco2_tg_R_prc_S_S_Yh.melt <- repeat_and_add_vector( L131.nonco2_tg_R_prc_S_S_Yh.melt, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
L131.nonco2_tg_R_prc_S_S_Yh.melt$EDGAR_emissions <- L131.EDGAR.melt$EDGAR_emissions[ match( vecpaste( L131.nonco2_tg_R_prc_S_S_Yh.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2", "xyear" )]), vecpaste( L131.EDGAR.melt[ c( "GCAM_region_ID", "EDGAR_agg_sector", "Non.CO2", "variable" )]) )]
L131.nonco2_tg_R_prc_S_S_Yh.melt <- na.omit( L131.nonco2_tg_R_prc_S_S_Yh.melt )
L131.nonco2_tg_R_prc_S_S_Yh.melt$input.emissions <- L131.nonco2_tg_R_prc_S_S_Yh.melt$EDGAR_emissions * L131.nonco2_tg_R_prc_S_S_Yh.melt$tech_share
L131.nonco2_tg_R_prc_S_S_Yh.melt <- aggregate( L131.nonco2_tg_R_prc_S_S_Yh.melt$input.emissions, by=as.list( L131.nonco2_tg_R_prc_S_S_Yh.melt[ c( "GCAM_region_ID", "supplysector", "subsector", "stub.technology", "Non.CO2", "xyear" )]), sum )
names( L131.nonco2_tg_R_prc_S_S_Yh.melt )[ names( L131.nonco2_tg_R_prc_S_S_Yh.melt ) == "x" ] <- "input.emissions"

#Reshape
L131.nonco2_tg_R_prc_S_S_Yh <- dcast( L131.nonco2_tg_R_prc_S_S_Yh.melt, GCAM_region_ID + Non.CO2 + supplysector + subsector + stub.technology ~ xyear, value = c( "input.emissions" ) )
L131.nonco2_tg_R_prc_S_S_Yh[ is.na( L131.nonco2_tg_R_prc_S_S_Yh ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L131.nonco2_tg_R_prc_S_S_Yh <- c( "GHG emissions by GCAM region / sector / technology / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L131.nonco2_tg_R_prc_S_S_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L131.nonco2_tg_R_prc_S_S_Yh", comments=comments.L131.nonco2_tg_R_prc_S_S_Yh )

# Every script should finish with this line
logstop()
