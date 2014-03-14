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
logstart( "L112.ghg_en_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
EPA_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_ghg_tech" )
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L101.in_EJ_R_en_Si_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.in_EJ_R_en_Si_F_Yh" )
L102.ghg_tgej_USA_en_Sepa_F_2005 <- readdata( "EMISSIONS_LEVEL1_DATA", "L102.ghg_tgej_USA_en_Sepa_F_2005" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Compute emissions using EPA emissions factors and IEA fuel consumption
printlog( "Computing unscaled emissions by country and technology" )
L112.ghg_tg_R_en_Si_F_Yh.melt <- melt( L101.in_EJ_R_en_Si_F_Yh, id.vars=c( "GCAM_region_ID", "sector", "fuel", "technology" ) ) 
names( L112.ghg_tg_R_en_Si_F_Yh.melt )[ names( L112.ghg_tg_R_en_Si_F_Yh.melt ) == "variable" ] <- "xyear"
names( L112.ghg_tg_R_en_Si_F_Yh.melt )[ names( L112.ghg_tg_R_en_Si_F_Yh.melt ) == "value" ] <- "energy"
L112.ghg_tg_R_en_Si_F_Yh.melt$EPA_agg_sector <- GCAM_sector_tech$EPA_agg_sector[ match( vecpaste( L112.ghg_tg_R_en_Si_F_Yh.melt[ c( "sector", "fuel" )]), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )]) )]
L112.ghg_tg_R_en_Si_F_Yh.melt$EPA_agg_fuel <- GCAM_sector_tech$EPA_agg_fuel[ match( vecpaste( L112.ghg_tg_R_en_Si_F_Yh.melt[ c( "sector", "fuel" )]), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )]) )]

#Remove years where we don't have emissions data
L112.ghg_tg_R_en_Si_F_Yh.melt <- subset( L112.ghg_tg_R_en_Si_F_Yh.melt, L112.ghg_tg_R_en_Si_F_Yh.melt$xyear %in% X_EDGAR_historical_years )

#Duplicate for all gases
L112.ghg_tg_R_en_Si_F_Yh.melt <- repeat_and_add_vector( L112.ghg_tg_R_en_Si_F_Yh.melt, "Non.CO2", GHG_names )

#Match in emissions factors
names( L102.ghg_tgej_USA_en_Sepa_F_2005 )[ names( L102.ghg_tgej_USA_en_Sepa_F_2005 ) == "ch4_em_factor" ] <- "CH4"
names( L102.ghg_tgej_USA_en_Sepa_F_2005 )[ names( L102.ghg_tgej_USA_en_Sepa_F_2005 ) == "n2o_em_factor" ] <- "N2O"
L102.ghg_tgej_USA_en_Sepa_F_2005.melt <- melt( L102.ghg_tgej_USA_en_Sepa_F_2005, id.vars = c( "sector", "fuel" ))
L112.ghg_tg_R_en_Si_F_Yh.melt$emfact <- L102.ghg_tgej_USA_en_Sepa_F_2005.melt$value[ match( vecpaste( L112.ghg_tg_R_en_Si_F_Yh.melt[ c( "Non.CO2", "EPA_agg_sector", "EPA_agg_fuel" )]), vecpaste( L102.ghg_tgej_USA_en_Sepa_F_2005.melt[ c( "variable", "sector", "fuel" ) ] ) )]

#Compute unscaled emissions
L112.ghg_tg_R_en_Si_F_Yh.melt$epa_emissions <- L112.ghg_tg_R_en_Si_F_Yh.melt$energy * L112.ghg_tg_R_en_Si_F_Yh.melt$emfact
  
#Aggregate by sector and region
L112.ghg_tg_R_en_Si_F_Yh.melt <- na.omit( L112.ghg_tg_R_en_Si_F_Yh.melt )
L112.ghg_tg_R_en_Si_F_Yh.melt$EDGAR_agg_sector <- GCAM_sector_tech$EDGAR_agg_sector[ match( vecpaste( L112.ghg_tg_R_en_Si_F_Yh.melt[ c( "sector", "fuel" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )] ))]
L112.ghg_tg_R_en_Sedgar_Yh.melt <- aggregate( L112.ghg_tg_R_en_Si_F_Yh.melt$epa_emissions, by=as.list( L112.ghg_tg_R_en_Si_F_Yh.melt[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "xyear" ) ] ), sum )
names( L112.ghg_tg_R_en_Sedgar_Yh.melt )[ names( L112.ghg_tg_R_en_Sedgar_Yh.melt ) == "x" ] <- "EPA_emissions"

printlog( "Compute EDGAR emissions by region and sector" )
EDGAR_CH4$Non.CO2 <- "CH4"
EDGAR_N2O$Non.CO2 <- "N2O"
L112.EDGAR <- rbind( EDGAR_CH4, EDGAR_N2O )
L112.EDGAR$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L112.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L112.EDGAR$iso <- EDGAR_nation$iso[ match( L112.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L112.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L112.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L112.EDGAR <- L112.EDGAR[ names( L112.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L112.EDGAR <- na.omit( L112.EDGAR )
L112.EDGAR <- aggregate( L112.EDGAR[ X_EDGAR_historical_years ], by=as.list( L112.EDGAR[ R_G_Sedgar ]), sum)
L112.EDGAR.melt <- melt( L112.EDGAR, id.vars = R_G_Sedgar )

printlog( "Scale EPA emissions by tech to match EDGAR totals")
#First compute scalers
L112.emiss_scaler <- L112.ghg_tg_R_en_Sedgar_Yh.melt
L112.emiss_scaler$EDGAR_emissions <- L112.EDGAR.melt$value[ match( vecpaste( L112.emiss_scaler[ c( R_G_Sedgar, "xyear" )]), vecpaste( L112.EDGAR.melt[ c( R_G_Sedgar, "variable" )]))]
L112.emiss_scaler$scaler <- L112.emiss_scaler$EDGAR_emissions / L112.emiss_scaler$EPA_emissions / 1000.0

#Now, scale EPA emissions
L112.ghg_tg_R_en_Si_F_Yh.melt$scaler <- L112.emiss_scaler$scaler[ match( vecpaste( L112.ghg_tg_R_en_Si_F_Yh.melt[ c( R_G_Sedgar, "xyear" )]), vecpaste( L112.emiss_scaler[ c( R_G_Sedgar, "xyear" )]) ) ]
L112.ghg_tg_R_en_Si_F_Yh.melt$emissions <- L112.ghg_tg_R_en_Si_F_Yh.melt$epa_emissions * L112.ghg_tg_R_en_Si_F_Yh.melt$scaler
L112.ghg_tg_R_en_Si_F_Yh.melt[ is.na( L112.ghg_tg_R_en_Si_F_Yh.melt ) ] <- 0

printlog( "Map in GCAM sector, technology, and driver type")
L112.ghg_tg_R_en_S_F_Yh.melt <- L112.ghg_tg_R_en_Si_F_Yh.melt
L112.ghg_tg_R_en_S_F_Yh.melt$supplysector <- GCAM_sector_tech$supplysector[ match( vecpaste( L112.ghg_tg_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L112.ghg_tg_R_en_S_F_Yh.melt$subsector <- GCAM_sector_tech$subsector[ match( vecpaste( L112.ghg_tg_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L112.ghg_tg_R_en_S_F_Yh.melt$stub.technology <- GCAM_sector_tech$stub.technology[ match( vecpaste( L112.ghg_tg_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L112.ghg_tg_R_en_S_F_Yh.melt <- aggregate( L112.ghg_tg_R_en_S_F_Yh.melt$emissions, by=as.list( L112.ghg_tg_R_en_S_F_Yh.melt[ R_G_StubTechYr ] ), sum)
names( L112.ghg_tg_R_en_S_F_Yh.melt )[ names( L112.ghg_tg_R_en_S_F_Yh.melt ) == "x" ] <- "input.emissions"

#Reshape
L112.ghg_tg_R_en_S_F_Yh <- dcast( L112.ghg_tg_R_en_S_F_Yh.melt, GCAM_region_ID + Non.CO2 + supplysector + subsector + stub.technology ~ xyear, value = c( "input.emissions" ) )

printlog( "Compute emissions factor by GCAM sector, technology, and driver type" )
#First compute energy by sector
L112.in_EJ_R_en_S_F_Yh.melt <- melt( L101.in_EJ_R_en_Si_F_Yh, id.vars=c( "GCAM_region_ID", "sector", "fuel", "technology" ) )
L112.in_EJ_R_en_S_F_Yh.melt$supplysector <- GCAM_sector_tech$supplysector[ match( vecpaste( L112.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L112.in_EJ_R_en_S_F_Yh.melt$subsector <- GCAM_sector_tech$subsector[ match( vecpaste( L112.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L112.in_EJ_R_en_S_F_Yh.melt$stub.technology <- GCAM_sector_tech$stub.technology[ match( vecpaste( L112.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
names( L112.in_EJ_R_en_S_F_Yh.melt )[ names( L112.in_EJ_R_en_S_F_Yh.melt ) == "variable" ] <- "xyear"
L112.in_EJ_R_en_S_F_Yh.melt <- aggregate( L112.in_EJ_R_en_S_F_Yh.melt$value, by=as.list( L112.in_EJ_R_en_S_F_Yh.melt[ R_StubTechYr ]), sum)
names( L112.in_EJ_R_en_S_F_Yh.melt )[ names( L112.in_EJ_R_en_S_F_Yh.melt ) == "x" ] <- "energy"

L112.ghg_tg_R_en_S_F_Yh.melt$energy <- L112.in_EJ_R_en_S_F_Yh.melt$energy[ match( vecpaste( L112.ghg_tg_R_en_S_F_Yh.melt[ R_StubTechYr ] ), vecpaste( L112.in_EJ_R_en_S_F_Yh.melt[ R_StubTechYr ] ) )] 
L112.ghg_tg_R_en_S_F_Yh.melt$emfact <- L112.ghg_tg_R_en_S_F_Yh.melt$input.emissions / L112.ghg_tg_R_en_S_F_Yh.melt$energy
L112.ghg_tg_R_en_S_F_Yh.melt[ is.na( L112.ghg_tg_R_en_S_F_Yh.melt ) ] <- 0
L112.ghg_tg_R_en_S_F_Yh.melt[ L112.ghg_tg_R_en_S_F_Yh.melt$emfact == "Inf" ] <- 0

#Reshape
L112.ghg_tgej_R_en_S_F_Yh <- dcast( L112.ghg_tg_R_en_S_F_Yh.melt, GCAM_region_ID + Non.CO2 + supplysector + subsector + stub.technology ~ xyear, value = c( "emfact" ) )


# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L112.ghg_tg_R_en_S_F_Yh <- c( "GHG emissions by GCAM region / sector / technology / historical year", "Unit = Tg" )
comments.L112.ghg_tgej_R_en_S_F_Yh <- c( "GHG emissions factors by GCAM region / sector / technology / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L112.ghg_tg_R_en_S_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L112.ghg_tg_R_en_S_F_Yh", comments=comments.L112.ghg_tg_R_en_S_F_Yh )
writedata( L112.ghg_tgej_R_en_S_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L112.ghg_tgej_R_en_S_F_Yh", comments=comments.L112.ghg_tgej_R_en_S_F_Yh )

# Every script should finish with this line
logstop()
