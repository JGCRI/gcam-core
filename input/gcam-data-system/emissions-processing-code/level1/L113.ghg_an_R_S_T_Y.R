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
logstart( "L113.ghg_an_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical GHG emissions for animals by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
EPA_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_ghg_tech" )
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L107.an_Prod_Mt_R_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L107.an_Prod_Mt_R_C_Sys_Fd_Y" )
L103.ghg_tgmt_USA_an_Sepa_F_2005 <- readdata( "EMISSIONS_LEVEL1_DATA", "L103.ghg_tgmt_USA_an_Sepa_F_2005" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Computing unscaled emissions by country and technology" )
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt <- melt( L107.an_Prod_Mt_R_C_Sys_Fd_Y, id.vars=c( "GCAM_region_ID", "GCAM_commodity", "system", "feed" ) ) 
names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt ) == "variable" ] <- "xyear"
names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt ) == "value" ] <- "production"
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$EPA_agg_sector <- GCAM_sector_tech$EPA_agg_sector[ match( vecpaste( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt[ c( "GCAM_commodity", "system", "feed" )]), vecpaste( GCAM_sector_tech[ c( "sector", "fuel", "technology" )]) )]

#Add Gas Name
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt <- repeat_and_add_vector( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt, "Non.CO2", c( "N2O_AGR", "CH4_AGR" ))

#Match in emissions factors
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$emfact <- L103.ghg_tgmt_USA_an_Sepa_F_2005$ch4_em_factor[ match( vecpaste( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt[ c( "EPA_agg_sector" )]), vecpaste( L103.ghg_tgmt_USA_an_Sepa_F_2005[ c( "sector" ) ] ) )]

#Compute unscaled emissions
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$epa_emissions <- L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$production * L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$emfact
  
#Aggregate by sector and region
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt <- na.omit( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt )
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$EDGAR_agg_sector <- GCAM_sector_tech$EDGAR_agg_sector[ match( vecpaste( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt[ c( "GCAM_commodity", "system", "feed" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel", "technology" )] ))]
L113.ghg_tg_R_an_C_Yh.melt <- aggregate( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$epa_emissions, by=as.list( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "xyear" ) ] ), sum )
names( L113.ghg_tg_R_an_C_Yh.melt )[ names( L113.ghg_tg_R_an_C_Yh.melt ) == "x" ] <- "EPA_emissions"

printlog( "Compute EDGAR emissions by region and sector" )
EDGAR_CH4$Non.CO2 <- "CH4_AGR"
EDGAR_N2O$Non.CO2 <- "N2O_AGR"
L113.EDGAR <- rbind( EDGAR_CH4, EDGAR_N2O )
L113.EDGAR$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L113.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L113.EDGAR$iso <- EDGAR_nation$iso[ match( L113.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L113.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L113.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L113.EDGAR <- L113.EDGAR[ names( L113.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L113.EDGAR <- na.omit( L113.EDGAR )
L113.EDGAR <- aggregate( L113.EDGAR[ X_EDGAR_historical_years ], by=as.list( L113.EDGAR[ R_G_Sedgar ]), sum)
L113.EDGAR.melt <- melt( L113.EDGAR, id.vars = R_G_Sedgar )

printlog( "Scale EPA emissions by tech to match EDGAR totals")
#First compute scalers
L113.emiss_scaler <- L113.ghg_tg_R_an_C_Yh.melt
L113.emiss_scaler$EDGAR_emissions <- L113.EDGAR.melt$value[ match( vecpaste( L113.emiss_scaler[ c( R_G_Sedgar, "xyear" )]), vecpaste( L113.EDGAR.melt[ c( R_G_Sedgar, "variable" )]))]
L113.emiss_scaler$scaler <- L113.emiss_scaler$EDGAR_emissions / L113.emiss_scaler$EPA_emissions / 1000.0

#Now, scale EPA emissions
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$scaler <- L113.emiss_scaler$scaler[ match( vecpaste( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "xyear" )]), vecpaste( L113.emiss_scaler[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "xyear" )]) ) ]
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$emissions <- L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$epa_emissions * L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt$scaler
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt <- subset( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt, xyear %in% X_EDGAR_historical_years )
L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt[ is.na( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt ) ] <- 0

#Rename columns
names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt ) == "GCAM_commodity" ] <- "supplysector"
names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt ) == "system" ] <- "subsector"
names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt ) == "feed" ] <- "stub.technology"

#Reshape
L113.ghg_tg_R_an_C_Sys_Fd_Yh <- dcast( L113.ghg_tg_R_an_C_Sys_Fd_Yh.melt,
                                       GCAM_region_ID + Non.CO2 + supplysector + subsector + stub.technology ~ xyear, value.var = "emissions" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L113.ghg_tg_R_an_C_Sys_Fd_Yh <- c( "Animal GHG emissions by GCAM region / sector / technology / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L113.ghg_tg_R_an_C_Sys_Fd_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L113.ghg_tg_R_an_C_Sys_Fd_Yh", comments=comments.L113.ghg_tg_R_an_C_Sys_Fd_Yh )

# Every script should finish with this line
logstop()
