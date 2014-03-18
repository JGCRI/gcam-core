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
logstart( "L101.nonghg_en_USA_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions factors by GCAM technology, computed from EPA emissions data and IEA energy balances" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
IEA_flow_sector <- readdata( "ENERGY_MAPPINGS", "IEA_flow_sector" )
IEA_product_fuel <- readdata( "ENERGY_MAPPINGS", "IEA_product_fuel" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
EPA_tech <- readdata( "EMISSIONS_MAPPINGS", "EPA_tech" )
L1231.in_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.in_EJ_R_elec_F_tech_Yh" )
L1322.in_EJ_R_indenergy_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indenergy_F_Yh" )
L144.in_EJ_R_bld_serv_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L144.in_EJ_R_bld_serv_F_Yh" )
L154.in_EJ_R_trn_m_sz_tech_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L154.in_EJ_R_trn_m_sz_tech_F_Yh" )
L1322.Fert_Prod_MtN_R_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L1322.Fert_Prod_MtN_R_F_Y" )
L1321.in_EJ_R_cement_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L1321.in_EJ_R_cement_F_Y" )
L124.in_EJ_R_heat_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L124.in_EJ_R_heat_F_Yh" )
L111.Prod_EJ_R_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L111.Prod_EJ_R_F_Yh" )
epa_so2 <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_SO2" )
epa_co <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_CO" )
epa_nox <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_NOx" )
epa_voc <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_VOC" )
epa_nh3 <- readdata( "EMISSIONS_LEVEL0_DATA", "EPA_NH3" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Combine all energy driver data into a single dataframe
printlog( "Combine all energy data into a single dataframe")
#Add technology column
L1322.in_EJ_R_indenergy_F_Yh$technology <- L1322.in_EJ_R_indenergy_F_Yh$fuel
L144.in_EJ_R_bld_serv_F_Yh$sector <- L144.in_EJ_R_bld_serv_F_Yh$service
L144.in_EJ_R_bld_serv_F_Yh$technology <- L144.in_EJ_R_bld_serv_F_Yh$fuel
L144.in_EJ_R_bld_serv_F_Yh <- L144.in_EJ_R_bld_serv_F_Yh[ names( L144.in_EJ_R_bld_serv_F_Yh ) != "service" ]
L1322.Fert_Prod_MtN_R_F_Y$technology <- L1322.Fert_Prod_MtN_R_F_Y$fuel
L1321.in_EJ_R_cement_F_Y$technology <- L1321.in_EJ_R_cement_F_Y$fuel
L124.in_EJ_R_heat_F_Yh$technology <- L124.in_EJ_R_heat_F_Yh$fuel
L111.Prod_EJ_R_F_Yh$technology <- L111.Prod_EJ_R_F_Yh$fuel

L154.in_EJ_R_trn_m_sz_tech_F_Yh$technology <- L154.in_EJ_R_trn_m_sz_tech_F_Yh$fuel
L154.in_EJ_R_trn_m_sz_tech_F_Yh$fuel <- L154.in_EJ_R_trn_m_sz_tech_F_Yh$mode
L154.in_EJ_R_trn_m_sz_tech_F_Yh$sector <- L154.in_EJ_R_trn_m_sz_tech_F_Yh$UCD_sector
L154.in_EJ_R_trn_m_sz_tech_F_Yh <- L154.in_EJ_R_trn_m_sz_tech_F_Yh[ names( L154.in_EJ_R_trn_m_sz_tech_F_Yh ) %!in% c( "mode", "UCD_sector", "size.class", "UCD_technology", "UCD_fuel" )]

#Bind all together
L101.in_EJ_R_en_Si_F_Yh <- rbind( L1231.in_EJ_R_elec_F_tech_Yh, L1322.in_EJ_R_indenergy_F_Yh, L144.in_EJ_R_bld_serv_F_Yh, L154.in_EJ_R_trn_m_sz_tech_F_Yh, L1322.Fert_Prod_MtN_R_F_Y, L1321.in_EJ_R_cement_F_Y, L124.in_EJ_R_heat_F_Yh, L111.Prod_EJ_R_F_Yh  )
      
printlog( "Subset for USA only and aggregate to EPA categories")
L101.in_EJ_USA_en_Si_F_Yh <- subset( L101.in_EJ_R_en_Si_F_Yh, L101.in_EJ_R_en_Si_F_Yh$GCAM_region_ID == "1" )
L101.in_EJ_USA_en_Si_F_Yh.melt <- melt( L101.in_EJ_USA_en_Si_F_Yh, id.vars = c( "GCAM_region_ID", "sector", "fuel", "technology" ) )
L101.in_EJ_USA_en_Si_F_Yh.melt$EPA_agg_sector <- GCAM_sector_tech$EPA_agg_sector[ match( vecpaste( L101.in_EJ_USA_en_Si_F_Yh.melt[ c( "sector", "fuel" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )]) )]
L101.in_EJ_USA_en_Si_F_Yh.melt$EPA_agg_fuel <- GCAM_sector_tech$EPA_agg_fuel[ match( vecpaste( L101.in_EJ_USA_en_Si_F_Yh.melt[ c( "sector", "fuel" )] ), vecpaste( GCAM_sector_tech[ c( "sector", "fuel" )]) )]
L101.in_EJ_USA_en_Sepa_F_Yh.melt <- aggregate( L101.in_EJ_USA_en_Si_F_Yh.melt$value, by=as.list( L101.in_EJ_USA_en_Si_F_Yh.melt[ c( "EPA_agg_sector", "EPA_agg_fuel", "variable" )]), sum )
names( L101.in_EJ_USA_en_Sepa_F_Yh.melt )[ names( L101.in_EJ_USA_en_Sepa_F_Yh.melt) == "x" ] <- "energy"

printlog( "Convert EPA SO2 emissions inventory to Tg and aggregate by sector and technology" )
L101.so2_tg_USA_en_Sepa_F_Yh <- epa_so2
L101.so2_tg_USA_en_Sepa_F_Yh$sector <- EPA_tech$sector[ match( L101.so2_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.so2_tg_USA_en_Sepa_F_Yh$fuel <- EPA_tech$fuel[ match( L101.so2_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.so2_tg_USA_en_Sepa_F_Yh <- aggregate( L101.so2_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ], by=as.list( L101.so2_tg_USA_en_Sepa_F_Yh[ c( "sector", "fuel" ) ] ), sum )

#Drop missing values
L101.so2_tg_USA_en_Sepa_F_Yh[ is.na( L101.so2_tg_USA_en_Sepa_F_Yh ) ] <- 0      

#Convert to Tg
L101.so2_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ] <- L101.so2_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ] * tst_to_tg  

printlog( "Convert EPA CO emissions inventory to Tg and aggregate by sector and technology" )
L101.co_tg_USA_en_Sepa_F_Yh <- epa_co
L101.co_tg_USA_en_Sepa_F_Yh$sector <- EPA_tech$sector[ match( L101.co_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.co_tg_USA_en_Sepa_F_Yh$fuel <- EPA_tech$fuel[ match( L101.co_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.co_tg_USA_en_Sepa_F_Yh <- aggregate( L101.co_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ], by=as.list( L101.co_tg_USA_en_Sepa_F_Yh[ c( "sector", "fuel" ) ] ), sum )

#Drop missing values
L101.co_tg_USA_en_Sepa_F_Yh[ is.na( L101.co_tg_USA_en_Sepa_F_Yh ) ] <- 0      

#Convert to Tg
L101.co_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ] <- L101.co_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ] * tst_to_tg  

printlog( "Convert EPA NOx emissions inventory to Tg and aggregate by sector and technology" )
L101.nox_tg_USA_en_Sepa_F_Yh <- epa_nox
L101.nox_tg_USA_en_Sepa_F_Yh$sector <- EPA_tech$sector[ match( L101.nox_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.nox_tg_USA_en_Sepa_F_Yh$fuel <- EPA_tech$fuel[ match( L101.nox_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.nox_tg_USA_en_Sepa_F_Yh <- aggregate( L101.nox_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ], by=as.list( L101.nox_tg_USA_en_Sepa_F_Yh[ c( "sector", "fuel" ) ] ), sum )

#Drop missing values
L101.nox_tg_USA_en_Sepa_F_Yh[ is.na( L101.nox_tg_USA_en_Sepa_F_Yh ) ] <- 0      

#Convert to Tg
L101.nox_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ] <- L101.nox_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ] * tst_to_tg  

printlog( "Convert EPA VOC emissions inventory to Tg and aggregate by sector and technology" )
L101.voc_tg_USA_en_Sepa_F_Yh <- epa_voc
L101.voc_tg_USA_en_Sepa_F_Yh$sector <- EPA_tech$sector[ match( L101.voc_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.voc_tg_USA_en_Sepa_F_Yh$fuel <- EPA_tech$fuel[ match( L101.voc_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.voc_tg_USA_en_Sepa_F_Yh <- aggregate( L101.voc_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ], by=as.list( L101.voc_tg_USA_en_Sepa_F_Yh[ c( "sector", "fuel" ) ] ), sum )

#Drop missing values
L101.voc_tg_USA_en_Sepa_F_Yh[ is.na( L101.voc_tg_USA_en_Sepa_F_Yh ) ] <- 0      

#Convert to Tg
L101.voc_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ] <- L101.voc_tg_USA_en_Sepa_F_Yh[ EPA_historical_years ] * tst_to_tg  

printlog( "Convert EPA NH3 emissions inventory to Tg and aggregate by sector and technology" )
L101.nh3_tg_USA_en_Sepa_F_Yh <- epa_nh3
L101.nh3_tg_USA_en_Sepa_F_Yh$sector <- EPA_tech$sector[ match( L101.nh3_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.nh3_tg_USA_en_Sepa_F_Yh$fuel <- EPA_tech$fuel[ match( L101.nh3_tg_USA_en_Sepa_F_Yh$Source_Category , EPA_tech$EPA_Category )]
L101.nh3_tg_USA_en_Sepa_F_Yh <- aggregate( L101.nh3_tg_USA_en_Sepa_F_Yh[ EPA_NH3_historical_years ], by=as.list( L101.nh3_tg_USA_en_Sepa_F_Yh[ c( "sector", "fuel" ) ] ), sum )

#Drop missing values
L101.nh3_tg_USA_en_Sepa_F_Yh[ is.na( L101.nh3_tg_USA_en_Sepa_F_Yh ) ] <- 0      

#Convert to Tg
L101.nh3_tg_USA_en_Sepa_F_Yh[ EPA_NH3_historical_years ] <- L101.nh3_tg_USA_en_Sepa_F_Yh[ EPA_NH3_historical_years ] * tst_to_tg  

printlog( "Compute SO2 emissions factors by dividing EPA inventory by IEA energy balances" )
#First melt data frame   
L101.so2_tg_USA_en_Sepa_F_Yh.melt <- melt( L101.so2_tg_USA_en_Sepa_F_Yh, id.vars = c( "sector", "fuel" ))

L101.so2_tgej_USA_en_Sepa_F_Yh.melt <- L101.in_EJ_USA_en_Sepa_F_Yh.melt
L101.so2_tgej_USA_en_Sepa_F_Yh.melt$emissions <- L101.so2_tg_USA_en_Sepa_F_Yh.melt$value[ match( vecpaste( L101.so2_tgej_USA_en_Sepa_F_Yh.melt[ c( "variable", "EPA_agg_sector", "EPA_agg_fuel" )]), vecpaste( L101.so2_tg_USA_en_Sepa_F_Yh.melt[ c( "variable", "sector", "fuel" ) ] )  )]
L101.so2_tgej_USA_en_Sepa_F_Yh.melt$emissions[ is.na( L101.so2_tgej_USA_en_Sepa_F_Yh.melt$emissions ) ] <- 0
L101.so2_tgej_USA_en_Sepa_F_Yh.melt$em_factor <- L101.so2_tgej_USA_en_Sepa_F_Yh.melt$emissions / L101.so2_tgej_USA_en_Sepa_F_Yh.melt$energy
L101.so2_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.so2_tgej_USA_en_Sepa_F_Yh.melt$em_factor == "Inf" ] <- 0

#Compute sector emissions. If these are zero, then reset all fuel em_factors to 1.
L101.so2_tgej_USA_en_Sepa_Yh.melt <- aggregate( L101.so2_tgej_USA_en_Sepa_F_Yh.melt$emissions, by=as.list( L101.so2_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ] ), sum )
L101.so2_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions <- L101.so2_tgej_USA_en_Sepa_Yh.melt$x[ match( vecpaste( L101.so2_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ]), vecpaste( L101.so2_tgej_USA_en_Sepa_Yh.melt[ c( "EPA_agg_sector", "variable" )]))]
L101.so2_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.so2_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions == 0 ] <- 1
names( L101.so2_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.so2_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_sector" ] <- "sector"
names( L101.so2_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.so2_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_fuel" ] <- "fuel"

#Reshape
L101.so2_tgej_USA_en_Sepa_F_Yh <- dcast( L101.so2_tgej_USA_en_Sepa_F_Yh.melt, sector + fuel ~ variable, value = c( "em_factor" ) )

printlog( "Compute CO emissions factors by dividing EPA inventory by IEA energy balances" )
#First melt both data sets   
L101.co_tg_USA_en_Sepa_F_Yh.melt <- melt( L101.co_tg_USA_en_Sepa_F_Yh, id.vars = c( "sector", "fuel" ))

L101.co_tgej_USA_en_Sepa_F_Yh.melt <- L101.in_EJ_USA_en_Sepa_F_Yh.melt
L101.co_tgej_USA_en_Sepa_F_Yh.melt$emissions <- L101.co_tg_USA_en_Sepa_F_Yh.melt$value[ match( vecpaste( L101.co_tgej_USA_en_Sepa_F_Yh.melt[ c( "variable", "EPA_agg_sector", "EPA_agg_fuel" )]), vecpaste( L101.co_tg_USA_en_Sepa_F_Yh.melt[ c( "variable", "sector", "fuel" ) ] )  )]
L101.co_tgej_USA_en_Sepa_F_Yh.melt$emissions[ is.na( L101.co_tgej_USA_en_Sepa_F_Yh.melt$emissions ) ] <- 0
L101.co_tgej_USA_en_Sepa_F_Yh.melt$em_factor <- L101.co_tgej_USA_en_Sepa_F_Yh.melt$emissions / L101.co_tgej_USA_en_Sepa_F_Yh.melt$energy
L101.co_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.co_tgej_USA_en_Sepa_F_Yh.melt$em_factor == "Inf" ] <- 0

#Compute sector emissions. If these are zero, then reset all fuel em_factors to 1.
L101.co_tgej_USA_en_Sepa_Yh.melt <- aggregate( L101.co_tgej_USA_en_Sepa_F_Yh.melt$emissions, by=as.list( L101.co_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ] ), sum )
L101.co_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions <- L101.co_tgej_USA_en_Sepa_Yh.melt$x[ match( vecpaste( L101.co_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ]), vecpaste( L101.co_tgej_USA_en_Sepa_Yh.melt[ c( "EPA_agg_sector", "variable" )]))]
L101.co_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.co_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions == 0 ] <- 1
names( L101.co_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.co_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_sector" ] <- "sector"
names( L101.co_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.co_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_fuel" ] <- "fuel"

#Reshape
L101.co_tgej_USA_en_Sepa_F_Yh <- dcast( L101.co_tgej_USA_en_Sepa_F_Yh.melt, sector + fuel ~ variable, value = c( "em_factor" ) )

printlog( "Compute NOx emissions factors by dividing EPA inventory by IEA energy balances" )
#First melt both data sets   
L101.nox_tg_USA_en_Sepa_F_Yh.melt <- melt( L101.nox_tg_USA_en_Sepa_F_Yh, id.vars = c( "sector", "fuel" ))

L101.nox_tgej_USA_en_Sepa_F_Yh.melt <- L101.in_EJ_USA_en_Sepa_F_Yh.melt
L101.nox_tgej_USA_en_Sepa_F_Yh.melt$emissions <- L101.nox_tg_USA_en_Sepa_F_Yh.melt$value[ match( vecpaste( L101.nox_tgej_USA_en_Sepa_F_Yh.melt[ c( "variable", "EPA_agg_sector", "EPA_agg_fuel" )]), vecpaste( L101.nox_tg_USA_en_Sepa_F_Yh.melt[ c( "variable", "sector", "fuel" ) ] )  )]
L101.nox_tgej_USA_en_Sepa_F_Yh.melt$emissions[ is.na( L101.nox_tgej_USA_en_Sepa_F_Yh.melt$emissions ) ] <- 0
L101.nox_tgej_USA_en_Sepa_F_Yh.melt$em_factor <- L101.nox_tgej_USA_en_Sepa_F_Yh.melt$emissions / L101.nox_tgej_USA_en_Sepa_F_Yh.melt$energy
L101.nox_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.nox_tgej_USA_en_Sepa_F_Yh.melt$em_factor == "Inf" ] <- 0

#Compute sector emissions. If these are zero, then reset all fuel em_factors to 1.
L101.nox_tgej_USA_en_Sepa_Yh.melt <- aggregate( L101.nox_tgej_USA_en_Sepa_F_Yh.melt$emissions, by=as.list( L101.nox_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ] ), sum )
L101.nox_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions <- L101.nox_tgej_USA_en_Sepa_Yh.melt$x[ match( vecpaste( L101.nox_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ]), vecpaste( L101.nox_tgej_USA_en_Sepa_Yh.melt[ c( "EPA_agg_sector", "variable" )]))]
L101.nox_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.nox_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions == 0 ] <- 1
names( L101.nox_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.nox_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_sector" ] <- "sector"
names( L101.nox_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.nox_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_fuel" ] <- "fuel"

#Reshape
L101.nox_tgej_USA_en_Sepa_F_Yh <- dcast( L101.nox_tgej_USA_en_Sepa_F_Yh.melt, sector + fuel ~ variable, value = c( "em_factor" ) )

printlog( "Compute VOC emissions factors by dividing EPA inventory by IEA energy balances" )
#First melt both data sets   
L101.voc_tg_USA_en_Sepa_F_Yh.melt <- melt( L101.voc_tg_USA_en_Sepa_F_Yh, id.vars = c( "sector", "fuel" ))

L101.voc_tgej_USA_en_Sepa_F_Yh.melt <- L101.in_EJ_USA_en_Sepa_F_Yh.melt
L101.voc_tgej_USA_en_Sepa_F_Yh.melt$emissions <- L101.voc_tg_USA_en_Sepa_F_Yh.melt$value[ match( vecpaste( L101.voc_tgej_USA_en_Sepa_F_Yh.melt[ c( "variable", "EPA_agg_sector", "EPA_agg_fuel" )]), vecpaste( L101.voc_tg_USA_en_Sepa_F_Yh.melt[ c( "variable", "sector", "fuel" ) ] )  )]
L101.voc_tgej_USA_en_Sepa_F_Yh.melt$emissions[ is.na( L101.voc_tgej_USA_en_Sepa_F_Yh.melt$emissions ) ] <- 0
L101.voc_tgej_USA_en_Sepa_F_Yh.melt$em_factor <- L101.voc_tgej_USA_en_Sepa_F_Yh.melt$emissions / L101.voc_tgej_USA_en_Sepa_F_Yh.melt$energy
L101.voc_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.voc_tgej_USA_en_Sepa_F_Yh.melt$em_factor == "Inf" ] <- 0

#Compute sector emissions. If these are zero, then reset all fuel em_factors to 1.
L101.voc_tgej_USA_en_Sepa_Yh.melt <- aggregate( L101.voc_tgej_USA_en_Sepa_F_Yh.melt$emissions, by=as.list( L101.voc_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ] ), sum )
L101.voc_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions <- L101.voc_tgej_USA_en_Sepa_Yh.melt$x[ match( vecpaste( L101.voc_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ]), vecpaste( L101.voc_tgej_USA_en_Sepa_Yh.melt[ c( "EPA_agg_sector", "variable" )]))]
L101.voc_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.voc_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions == 0 ] <- 1
names( L101.voc_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.voc_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_sector" ] <- "sector"
names( L101.voc_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.voc_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_fuel" ] <- "fuel"

#Reshape
L101.voc_tgej_USA_en_Sepa_F_Yh <- dcast( L101.voc_tgej_USA_en_Sepa_F_Yh.melt, sector + fuel ~ variable, value = c( "em_factor" ) )

printlog( "Compute NH3 emissions factors by dividing EPA inventory by IEA energy balances" )
#First melt both data sets   
L101.nh3_tg_USA_en_Sepa_F_Yh.melt <- melt( L101.nh3_tg_USA_en_Sepa_F_Yh, id.vars = c( "sector", "fuel" ))

L101.nh3_tgej_USA_en_Sepa_F_Yh.melt <- subset( L101.in_EJ_USA_en_Sepa_F_Yh.melt, L101.in_EJ_USA_en_Sepa_F_Yh.melt$variable %!in% X_NH3_extra_years )
L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$emissions <- L101.nh3_tg_USA_en_Sepa_F_Yh.melt$value[ match( vecpaste( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt[ c( "variable", "EPA_agg_sector", "EPA_agg_fuel" )]), vecpaste( L101.nh3_tg_USA_en_Sepa_F_Yh.melt[ c( "variable", "sector", "fuel" ) ] )  )]
L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$emissions[ is.na( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$emissions ) ] <- 0
L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$em_factor <- L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$emissions / L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$energy
L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$em_factor == "Inf" ] <- 0

#Compute sector emissions. If these are zero, then reset all fuel em_factors to 1.
L101.nh3_tgej_USA_en_Sepa_Yh.melt <- aggregate( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$emissions, by=as.list( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ] ), sum )
L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions <- L101.nh3_tgej_USA_en_Sepa_Yh.melt$x[ match( vecpaste( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt[ c( "EPA_agg_sector", "variable" ) ]), vecpaste( L101.nh3_tgej_USA_en_Sepa_Yh.melt[ c( "EPA_agg_sector", "variable" )]))]
L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$em_factor[ L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$sector_emissions == 0 ] <- 1
names( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_sector" ] <- "sector"
names( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt )[ names( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt ) == "EPA_agg_fuel" ] <- "fuel"

#Add extra years
for ( y in X_NH3_extra_years ) {
  temp <- subset( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt, L101.nh3_tgej_USA_en_Sepa_F_Yh.melt$variable == "X1990" )
  temp$variable <- y
  L101.nh3_tgej_USA_en_Sepa_F_Yh.melt <- rbind( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt, temp )
}

#Reshape
L101.nh3_tgej_USA_en_Sepa_F_Yh <- dcast( L101.nh3_tgej_USA_en_Sepa_F_Yh.melt, sector + fuel ~ variable, value = c( "em_factor" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L101.so2_tgej_USA_en_Sepa_F_Yh <- c( "SO2 emissions factors for the USA by EPA sector / fuel / historical year", "Unit = Tg / EJ" )
comments.L101.co_tgej_USA_en_Sepa_F_Yh <- c( "CO emissions factors for the USA by EPA sector / fuel / historical year", "Unit = Tg / EJ" )
comments.L101.nox_tgej_USA_en_Sepa_F_Yh <- c( "NOx emissions factors for the USA by EPA sector / fuel / historical year", "Unit = Tg / EJ" )
comments.L101.voc_tgej_USA_en_Sepa_F_Yh <- c( "VOC emissions factors for the USA by EPA sector / fuel / historical year", "Unit = Tg / EJ" )
comments.L101.nh3_tgej_USA_en_Sepa_F_Yh <- c( "NH3 emissions factors for the USA by EPA sector / fuel / historical year", "Unit = Tg / EJ" )
comments.L101.in_EJ_R_en_Si_F_Yh <- c( "Energy consumption by region / GCAM sector / fuel / historical year", "Unit = EJ")

#write tables as CSV files
writedata( L101.so2_tgej_USA_en_Sepa_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L101.so2_tgej_USA_en_Sepa_F_Yh", comments=comments.L101.so2_tgej_USA_en_Sepa_F_Yh )
writedata( L101.co_tgej_USA_en_Sepa_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L101.co_tgej_USA_en_Sepa_F_Yh", comments=comments.L101.co_tgej_USA_en_Sepa_F_Yh )
writedata( L101.nox_tgej_USA_en_Sepa_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L101.nox_tgej_USA_en_Sepa_F_Yh", comments=comments.L101.nox_tgej_USA_en_Sepa_F_Yh )
writedata( L101.voc_tgej_USA_en_Sepa_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L101.voc_tgej_USA_en_Sepa_F_Yh", comments=comments.L101.voc_tgej_USA_en_Sepa_F_Yh )
writedata( L101.nh3_tgej_USA_en_Sepa_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L101.nh3_tgej_USA_en_Sepa_F_Yh", comments=comments.L101.nh3_tgej_USA_en_Sepa_F_Yh )
writedata( L101.in_EJ_R_en_Si_F_Yh, domain="EMISSIONS_LEVEL1_DATA", fn="L101.in_EJ_R_en_Si_F_Yh", comments=comments.L101.in_EJ_R_en_Si_F_Yh )

# Every script should finish with this line
logstop()
