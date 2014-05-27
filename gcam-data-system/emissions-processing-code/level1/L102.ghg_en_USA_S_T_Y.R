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
logstart( "L102.ghg_en_USA_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions factors for energy by GCAM fuel, computed from EPA emissions data and IEA energy balances" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
IEA_flow_sector <- readdata( "ENERGY_MAPPINGS", "IEA_flow_sector" )
IEA_product_fuel <- readdata( "ENERGY_MAPPINGS", "IEA_product_fuel" )
EPA_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_ghg_tech" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L101.in_EJ_R_en_Si_F_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L101.in_EJ_R_en_Si_F_Yh" )
EPA_FCCC_GHG_2005 <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_FCCC_GHG_2005" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Convert EPA GHG emissions inventory to Tg and aggregate by sector and fuel" )
L102.ghg_tg_USA_en_Sepa_F_2005 <- EPA_FCCC_GHG_2005
L102.ghg_tg_USA_en_Sepa_F_2005$sector <- EPA_tech$sector[ match( L102.ghg_tg_USA_en_Sepa_F_2005$Source_Category , EPA_tech$Source_Category )]
L102.ghg_tg_USA_en_Sepa_F_2005$fuel <- EPA_tech$fuel[ match( L102.ghg_tg_USA_en_Sepa_F_2005$Source_Category , EPA_tech$Source_Category )]
L102.ghg_tg_USA_en_Sepa_F_2005 <- aggregate( L102.ghg_tg_USA_en_Sepa_F_2005[ GHG_names ], by=as.list( L102.ghg_tg_USA_en_Sepa_F_2005[ c( "sector", "fuel" ) ] ), sum )

#Provide values for missing EPA sectors.
#NOTE: THIS IS A HACK. EPA DOESN'T HAVE EMISSIONS IN SOME SECTORS, WHEN EDGAR DOES. THIS WILL MAKE EMISSIONS PROPORTIONAL TO FUEL USE. NOT SURE IF THIS IS THE BEST STRATEGY.
L102.ghg_tg_USA_en_Sepa_F_2005[ is.na( L102.ghg_tg_USA_en_Sepa_F_2005 ) ] <- 1

#Convert to Tg
L102.ghg_tg_USA_en_Sepa_F_2005[ GHG_names ] <- L102.ghg_tg_USA_en_Sepa_F_2005[ GHG_names ] * gg_to_tg  

printlog( "Compute GHG emissions factors by dividing EPA inventory by IEA energy balances" )
#Subset for USA only in 2005 and aggregate to EPA categories"
L102.in_EJ_USA_en_Si_F_Yh <- subset( L101.in_EJ_R_en_Si_F_Yh, L101.in_EJ_R_en_Si_F_Yh$GCAM_region_ID == "1" )
L102.in_EJ_USA_en_Si_F_Yh.melt <- melt( L102.in_EJ_USA_en_Si_F_Yh, id.vars = c( "GCAM_region_ID", "sector", "fuel", "technology" ) )
L102.in_EJ_USA_en_Si_F_Yh.melt$EPA_agg_sector <- GCAM_sector_tech$EPA_agg_sector[ match( vecpaste( L102.in_EJ_USA_en_Si_F_Yh.melt[ c( "sector", "fuel" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )]) )]
L102.in_EJ_USA_en_Si_F_Yh.melt$EPA_agg_fuel <- GCAM_sector_tech$EPA_agg_fuel_ghg[ match( vecpaste( L102.in_EJ_USA_en_Si_F_Yh.melt[ c( "sector", "fuel" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )]) )]
L102.in_EJ_USA_en_Sepa_F_Yh.melt <- aggregate( L102.in_EJ_USA_en_Si_F_Yh.melt$value, by=as.list( L102.in_EJ_USA_en_Si_F_Yh.melt[ c( "EPA_agg_sector", "EPA_agg_fuel", "variable" )]), sum )
names( L102.in_EJ_USA_en_Sepa_F_Yh.melt )[ names( L102.in_EJ_USA_en_Sepa_F_Yh.melt ) == "x" ] <- "energy"
L102.in_EJ_USA_en_Sepa_F_2005.melt <- subset( L102.in_EJ_USA_en_Sepa_F_Yh.melt, L102.in_EJ_USA_en_Sepa_F_Yh.melt$variable == "X2005" )

L102.ghg_tgej_USA_en_Sepa_F_2005 <- L102.ghg_tg_USA_en_Sepa_F_2005
L102.ghg_tgej_USA_en_Sepa_F_2005$energy <- L102.in_EJ_USA_en_Sepa_F_2005.melt$energy[ match( vecpaste( L102.ghg_tgej_USA_en_Sepa_F_2005[ c( "sector", "fuel" )]), vecpaste( L102.in_EJ_USA_en_Sepa_F_2005.melt[ c( "EPA_agg_sector", "EPA_agg_fuel" ) ] )  )]
L102.ghg_tgej_USA_en_Sepa_F_2005$ch4_em_factor <- L102.ghg_tgej_USA_en_Sepa_F_2005$CH4 / L102.ghg_tgej_USA_en_Sepa_F_2005$energy
L102.ghg_tgej_USA_en_Sepa_F_2005$n2o_em_factor <- L102.ghg_tgej_USA_en_Sepa_F_2005$N2O / L102.ghg_tgej_USA_en_Sepa_F_2005$energy
L102.ghg_tgej_USA_en_Sepa_F_2005$ch4_em_factor[ L102.ghg_tgej_USA_en_Sepa_F_2005$ch4_em_factor == "Inf" ] <- 0
L102.ghg_tgej_USA_en_Sepa_F_2005$n2o_em_factor[ L102.ghg_tgej_USA_en_Sepa_F_2005$n2o_em_factor == "Inf" ] <- 0
L102.ghg_tgej_USA_en_Sepa_F_2005$ch4_em_factor[ is.na( L102.ghg_tgej_USA_en_Sepa_F_2005$ch4_em_factor ) ] <- 0
L102.ghg_tgej_USA_en_Sepa_F_2005$n2o_em_factor[ is.na( L102.ghg_tgej_USA_en_Sepa_F_2005$n2o_em_factor ) ] <- 0

#Drop unnecessary columns
L102.ghg_tgej_USA_en_Sepa_F_2005 <- L102.ghg_tgej_USA_en_Sepa_F_2005[ names( L102.ghg_tgej_USA_en_Sepa_F_2005 ) %!in% c( "CH4", "CO2", "N2O", "energy" )]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L102.ghg_tgej_USA_en_Sepa_F_2005 <- c( "GHG emissions factors for the USA energy sector by sector / fuel / 2005", "Unit = Tg / EJ" )

#write tables as CSV files
writedata( L102.ghg_tgej_USA_en_Sepa_F_2005, domain="EMISSIONS_LEVEL1_DATA", fn="L102.ghg_tgej_USA_en_Sepa_F_2005", comments=comments.L102.ghg_tgej_USA_en_Sepa_F_2005 )

# Every script should finish with this line
logstop()
