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
logstart( "L115.nh3_an_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L107.an_Prod_Mt_R_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L107.an_Prod_Mt_R_C_Sys_Fd_Y" )
L105.nh3_tgmt_USA_an_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L105.nh3_tgmt_USA_an_Yh" )
EDGAR_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NH3" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Compute emissions using EPA emissions factors and FAO animal production
printlog( "Computing unscaled emissions by country and technology" )
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt <- melt( L107.an_Prod_Mt_R_C_Sys_Fd_Y, id.vars=c( "GCAM_region_ID", "GCAM_commodity", "system", "feed" ) ) 
names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt ) == "variable" ] <- "xyear"
names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt ) == "value" ] <- "production"

#Add Gas Name
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$Non.CO2 <- "NH3_AGR"

#Match in emissions factors
L115.nh3_tgmt_USA_an_Yh.melt <- melt( L105.nh3_tgmt_USA_an_Yh, id.vars=c( "sector", "fuel" ) )
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$emfact <- L115.nh3_tgmt_USA_an_Yh.melt$value[ match( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$xyear, L115.nh3_tgmt_USA_an_Yh.melt$variable )]

#Compute unscaled emissions
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$epa_emissions <- L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$production * L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$emfact
  
#Aggregate by sector and region
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt <- na.omit( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt )
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$EDGAR_agg_sector <- GCAM_sector_tech$EDGAR_agg_sector[ match( vecpaste( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt[ c( "GCAM_commodity", "system", "feed" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel", "technology" )] ))]
L115.nh3_tg_R_an_C_Yh.melt <- aggregate( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$epa_emissions, by=as.list( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "xyear" ) ] ), sum )
names( L115.nh3_tg_R_an_C_Yh.melt )[ names( L115.nh3_tg_R_an_C_Yh.melt ) == "x" ] <- "EPA_emissions"

printlog( "Compute EDGAR emissions by region and sector" )
L115.EDGAR <- EDGAR_NH3
L115.EDGAR$Non.CO2 <- "NH3_AGR"
L115.EDGAR$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L115.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L115.EDGAR$iso <- EDGAR_nation$iso[ match( L115.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L115.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L115.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L115.EDGAR <- L115.EDGAR[ names( L115.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L115.EDGAR <- na.omit( L115.EDGAR )
L115.EDGAR <- aggregate( L115.EDGAR[ X_EDGAR_historical_years ], by=as.list( L115.EDGAR[ R_G_Sedgar ]), sum)
L115.EDGAR.melt <- melt( L115.EDGAR, id.vars = R_G_Sedgar )

printlog( "Scale EPA emissions by tech to match EDGAR totals")
#First compute scalers
L115.emiss_scaler <- L115.nh3_tg_R_an_C_Yh.melt
L115.emiss_scaler$EDGAR_emissions <- L115.EDGAR.melt$value[ match( vecpaste( L115.emiss_scaler[ c( R_G_Sedgar, "xyear" )]), vecpaste( L115.EDGAR.melt[ c( R_G_Sedgar, "variable" )]))]
L115.emiss_scaler$scaler <- L115.emiss_scaler$EDGAR_emissions / L115.emiss_scaler$EPA_emissions / 1000.0

#Now, scale EPA emissions
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$scaler <- L115.emiss_scaler$scaler[ match( vecpaste( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "xyear" )]), vecpaste( L115.emiss_scaler[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "xyear" )]) ) ]
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$emissions <- L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$epa_emissions * L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt$scaler
L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt[ is.na( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt ) ] <- 0

#Rename columns
names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt ) == "GCAM_commodity" ] <- "supplysector"
names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt ) == "system" ] <- "subsector"
names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt )[ names( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt ) == "feed" ] <- "stub.technology"

#Reshape
L115.nh3_tg_R_an_C_Sys_Fd_Yh <- dcast( L115.nh3_tg_R_an_C_Sys_Fd_Yh.melt, GCAM_region_ID + Non.CO2 + supplysector + subsector + stub.technology ~ xyear, value = c( "emissions" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L115.nh3_tg_R_an_C_Sys_Fd_Yh <- c( "Animal NH3 emissions by GCAM region / sector / technology / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L115.nh3_tg_R_an_C_Sys_Fd_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L115.nh3_tg_R_an_C_Sys_Fd_Yh", comments=comments.L115.nh3_tg_R_an_C_Sys_Fd_Yh )

# Every script should finish with this line
logstop()
