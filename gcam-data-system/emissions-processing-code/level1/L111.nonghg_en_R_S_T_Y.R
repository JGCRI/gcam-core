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
logstart( "L111.nonghg_en_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
EPA_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_tech" )
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L101.in_EJ_R_en_Si_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.in_EJ_R_en_Si_F_Yh" )
L101.so2_tgej_USA_en_Sepa_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.so2_tgej_USA_en_Sepa_F_Yh" )
L101.co_tgej_USA_en_Sepa_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.co_tgej_USA_en_Sepa_F_Yh" )
L101.nox_tgej_USA_en_Sepa_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.nox_tgej_USA_en_Sepa_F_Yh" )
L101.voc_tgej_USA_en_Sepa_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.voc_tgej_USA_en_Sepa_F_Yh" )
L101.nh3_tgej_USA_en_Sepa_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.nh3_tgej_USA_en_Sepa_F_Yh" )
EDGAR_SO2 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_SO2" )
EDGAR_CO <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CO" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )
EDGAR_VOC <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NMVOC" )
EDGAR_NH3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NH3" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Compute emissions using EPA emissions factors and IEA fuel consumption
printlog( "First add gas name and bind all dataframes together. This will make future processing easier" )
L101.so2_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "SO2"
L101.co_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "CO"
L101.nox_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NOx"
L101.voc_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NMVOC"
L101.nh3_tgej_USA_en_Sepa_F_Yh$Non.CO2 <- "NH3"

L101.nonghg_tgej_USA_en_Sepa_F_Yh <- rbind( L101.so2_tgej_USA_en_Sepa_F_Yh, L101.co_tgej_USA_en_Sepa_F_Yh, L101.nox_tgej_USA_en_Sepa_F_Yh, L101.voc_tgej_USA_en_Sepa_F_Yh, L101.nh3_tgej_USA_en_Sepa_F_Yh )
#Add extra columns for post-2002 data
L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2003 <- L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2002
L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2004 <- L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2002
L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2005 <- L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2002
L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2006 <- L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2002
L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2007 <- L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2002
L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2008 <- L101.nonghg_tgej_USA_en_Sepa_F_Yh$X2002
L111.nonghg_tgej_USA_en_Sepa_F_Yh.melt <- melt( L101.nonghg_tgej_USA_en_Sepa_F_Yh, id.vars=c( "Non.CO2", "sector", "fuel" ) )

printlog( "Computing unscaled emissions by country and technology" )
L111.nonghg_tg_R_en_Si_F_Yh.melt <- melt( L101.in_EJ_R_en_Si_F_Yh, id.vars=c( "GCAM_region_ID", "sector", "fuel", "technology" ) ) 
names( L111.nonghg_tg_R_en_Si_F_Yh.melt )[ names( L111.nonghg_tg_R_en_Si_F_Yh.melt ) == "variable" ] <- "xyear"
names( L111.nonghg_tg_R_en_Si_F_Yh.melt )[ names( L111.nonghg_tg_R_en_Si_F_Yh.melt ) == "value" ] <- "energy"
L111.nonghg_tg_R_en_Si_F_Yh.melt$EPA_agg_sector <- GCAM_sector_tech$EPA_agg_sector[ match( vecpaste( L111.nonghg_tg_R_en_Si_F_Yh.melt[ c( "sector", "fuel" )]), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )]) )]
L111.nonghg_tg_R_en_Si_F_Yh.melt$EPA_agg_fuel <- GCAM_sector_tech$EPA_agg_fuel[ match( vecpaste( L111.nonghg_tg_R_en_Si_F_Yh.melt[ c( "sector", "fuel" )]), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )]) )]

#Duplicate for all gases
L111.nonghg_tg_R_en_Si_F_Yh.melt <- repeat_and_add_vector( L111.nonghg_tg_R_en_Si_F_Yh.melt, "Non.CO2", nonghg_gases )

#Match in emissions factors
L111.nonghg_tg_R_en_Si_F_Yh.melt$emfact <- L111.nonghg_tgej_USA_en_Sepa_F_Yh.melt$value[ match( vecpaste( L111.nonghg_tg_R_en_Si_F_Yh.melt[ c( "Non.CO2", "EPA_agg_sector", "EPA_agg_fuel", "xyear" )]), vecpaste( L111.nonghg_tgej_USA_en_Sepa_F_Yh.melt[ c( "Non.CO2", "sector", "fuel", "variable" ) ] ) )]

#Compute unscaled emissions
L111.nonghg_tg_R_en_Si_F_Yh.melt$epa_emissions <- L111.nonghg_tg_R_en_Si_F_Yh.melt$energy * L111.nonghg_tg_R_en_Si_F_Yh.melt$emfact
L111.nonghg_tg_R_en_Si_F_Yh.melt <- na.omit( L111.nonghg_tg_R_en_Si_F_Yh.melt )
  
#Aggregate by EDGAR sector and region
L111.nonghg_tg_R_en_Si_F_Yh.melt$EDGAR_agg_sector <- GCAM_sector_tech$EDGAR_agg_sector[ match( vecpaste( L111.nonghg_tg_R_en_Si_F_Yh.melt[ c( "sector", "fuel" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )] ))]
L111.nonghg_tg_R_en_Sedgar_Yh.melt <- aggregate( L111.nonghg_tg_R_en_Si_F_Yh.melt$epa_emissions, by=as.list( L111.nonghg_tg_R_en_Si_F_Yh.melt[ c( "GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "xyear" ) ] ), sum )
names( L111.nonghg_tg_R_en_Sedgar_Yh.melt )[ names( L111.nonghg_tg_R_en_Sedgar_Yh.melt ) == "x" ] <- "EPA_emissions"

printlog( "Compute EDGAR emissions by region and sector" )
EDGAR_SO2$Non.CO2 <- "SO2"
EDGAR_CO$Non.CO2 <- "CO"
EDGAR_NOx$Non.CO2 <- "NOx"
EDGAR_NH3$Non.CO2 <- "NH3"
EDGAR_VOC$Non.CO2 <- "NMVOC"
EDGAR_VOC <- EDGAR_VOC[ names( EDGAR_VOC ) %!in% c( "X2009", "X2010" )]
L111.EDGAR <- rbind( EDGAR_SO2, EDGAR_CO, EDGAR_NOx, EDGAR_VOC, EDGAR_NH3 )
L111.EDGAR$EDGAR_agg_sector <- EDGAR_sector$agg_sector[ match( L111.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L111.EDGAR$iso <- EDGAR_nation$iso[ match( L111.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L111.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L111.EDGAR$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L111.EDGAR <- L111.EDGAR[ names( L111.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L111.EDGAR <- na.omit( L111.EDGAR )
L111.EDGAR <- aggregate( L111.EDGAR[ X_EDGAR_historical_years ], by=as.list( L111.EDGAR[ R_G_Sedgar ]), sum)
L111.EDGAR.melt <- melt( L111.EDGAR, id.vars = R_G_Sedgar )

printlog( "Scale EPA emissions by tech to match EDGAR totals")
#First compute scalers
L111.emiss_scaler <- L111.nonghg_tg_R_en_Sedgar_Yh.melt
L111.emiss_scaler$EDGAR_emissions <- L111.EDGAR.melt$value[ match( vecpaste( L111.emiss_scaler[ c( R_G_Sedgar, "xyear" )]), vecpaste( L111.EDGAR.melt[ c("GCAM_region_ID", "Non.CO2", "EDGAR_agg_sector", "variable" )]))]
L111.emiss_scaler$scaler <- L111.emiss_scaler$EDGAR_emissions / L111.emiss_scaler$EPA_emissions / 1000.0

#Now, scale EPA emissions
L111.nonghg_tg_R_en_Si_F_Yh.melt$scaler <- L111.emiss_scaler$scaler[ match( vecpaste( L111.nonghg_tg_R_en_Si_F_Yh.melt[ c( R_G_Sedgar, "xyear" )]), vecpaste( L111.emiss_scaler[ c( R_G_Sedgar, "xyear" )]) ) ]
L111.nonghg_tg_R_en_Si_F_Yh.melt$emissions <- L111.nonghg_tg_R_en_Si_F_Yh.melt$epa_emissions * L111.nonghg_tg_R_en_Si_F_Yh.melt$scaler
L111.nonghg_tg_R_en_Si_F_Yh.melt[ is.na( L111.nonghg_tg_R_en_Si_F_Yh.melt ) ] <- 0

printlog( "Map in final GCAM sector, technology, and driver type")
L111.nonghg_tg_R_en_S_F_Yh.melt <- L111.nonghg_tg_R_en_Si_F_Yh.melt
L111.nonghg_tg_R_en_S_F_Yh.melt$supplysector <- GCAM_sector_tech$supplysector[ match( vecpaste( L111.nonghg_tg_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L111.nonghg_tg_R_en_S_F_Yh.melt$subsector <- GCAM_sector_tech$subsector[ match( vecpaste( L111.nonghg_tg_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L111.nonghg_tg_R_en_S_F_Yh.melt$stub.technology <- GCAM_sector_tech$stub.technology[ match( vecpaste( L111.nonghg_tg_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L111.nonghg_tg_R_en_S_F_Yh.melt <- aggregate( L111.nonghg_tg_R_en_S_F_Yh.melt$emissions, by=as.list( L111.nonghg_tg_R_en_S_F_Yh.melt[ R_G_StubTechYr ]), sum)
names( L111.nonghg_tg_R_en_S_F_Yh.melt )[ names( L111.nonghg_tg_R_en_S_F_Yh.melt ) == "x" ] <- "input.emissions"

#Reshape
L111.nonghg_tg_R_en_S_F_Yh <- dcast( L111.nonghg_tg_R_en_S_F_Yh.melt, GCAM_region_ID + Non.CO2 + supplysector + subsector + stub.technology ~ xyear, value = c( "input.emissions" ) )
 
printlog( "Compute emissions factor by GCAM sector, technology, and driver type" )
#First compute energy by sector
L101.in_EJ_R_en_S_F_Yh.melt <- melt( L101.in_EJ_R_en_Si_F_Yh, id.vars=c( "GCAM_region_ID", "sector", "fuel", "technology" ) )
L101.in_EJ_R_en_S_F_Yh.melt$supplysector <- GCAM_sector_tech$supplysector[ match( vecpaste( L101.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L101.in_EJ_R_en_S_F_Yh.melt$subsector <- GCAM_sector_tech$subsector[ match( vecpaste( L101.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
L101.in_EJ_R_en_S_F_Yh.melt$stub.technology <- GCAM_sector_tech$stub.technology[ match( vecpaste( L101.in_EJ_R_en_S_F_Yh.melt[ c( "sector", "fuel", "technology" ) ] ) , vecpaste( GCAM_sector_tech[c( "sector", "fuel", "technology" )] ) ) ]
names( L101.in_EJ_R_en_S_F_Yh.melt )[ names( L101.in_EJ_R_en_S_F_Yh.melt ) == "variable" ] <- "xyear"
L101.in_EJ_R_en_S_F_Yh.melt <- aggregate( L101.in_EJ_R_en_S_F_Yh.melt$value, by=as.list( L101.in_EJ_R_en_S_F_Yh.melt[ R_StubTechYr ]), sum)
names( L101.in_EJ_R_en_S_F_Yh.melt )[ names( L101.in_EJ_R_en_S_F_Yh.melt ) == "x" ] <- "energy"

L111.nonghg_tg_R_en_S_F_Yh.melt$energy <- L101.in_EJ_R_en_S_F_Yh.melt$energy[ match( vecpaste( L111.nonghg_tg_R_en_S_F_Yh.melt[ R_StubTechYr ] ), vecpaste( L101.in_EJ_R_en_S_F_Yh.melt[ R_StubTechYr ] ) )] 
L111.nonghg_tg_R_en_S_F_Yh.melt$emfact <- L111.nonghg_tg_R_en_S_F_Yh.melt$input.emissions / L111.nonghg_tg_R_en_S_F_Yh.melt$energy
L111.nonghg_tg_R_en_S_F_Yh.melt[ is.na( L111.nonghg_tg_R_en_S_F_Yh.melt ) ] <- 0
L111.nonghg_tg_R_en_S_F_Yh.melt[ L111.nonghg_tg_R_en_S_F_Yh.melt$emfact == "Inf" ] <- 0

#Reshape
L111.nonghg_tgej_R_en_S_F_Yh <- dcast( L111.nonghg_tg_R_en_S_F_Yh.melt, GCAM_region_ID + Non.CO2 + supplysector + subsector + stub.technology ~ xyear, value = c( "emfact" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L111.nonghg_tg_R_en_S_F_Yh <- c( "Non-GHG emissions by GCAM region / sector / technology / historical year", "Unit = Tg" )
comments.L111.nonghg_tgej_R_en_S_F_Yh <- c( "Non-GHG emissions factors by GCAM region / sector / technology / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L111.nonghg_tg_R_en_S_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L111.nonghg_tg_R_en_S_F_Yh", comments=comments.L111.nonghg_tg_R_en_S_F_Yh )
writedata( L111.nonghg_tgej_R_en_S_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L111.nonghg_tgej_R_en_S_F_Yh", comments=comments.L111.nonghg_tgej_R_en_S_F_Yh )

# Every script should finish with this line
logstop()
