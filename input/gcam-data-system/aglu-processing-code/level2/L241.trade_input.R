# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "L241.trade_input.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for demands of all aglu commodities (crop, animal, forest)" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_agSupplySector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySector" )
A_demand_technology <- readdata( "AGLU_ASSUMPTIONS", "A_demand_technology" )
A_an_input_technology <- readdata( "AGLU_ASSUMPTIONS", "A_an_input_technology" )
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_irr" )
L101.ag_kcalg_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_kcalg_R_C_Y" )
L102.pcgdp_thous90USD_SSP_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_SSP_R_Y" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )

# -----------------------------------------------------------------------------
# 2. Build tables
# First, determine which regions are in which groupings.
L241.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_SSP_R_Y, L102.pcgdp_thous90USD_SSP_R_Y$scenario == "SSP4" )
L241.pcgdp_2010 <- L241.pcgdp_2010[ names( L241.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L241.pcgdp_2010 <- add_region_name( L241.pcgdp_2010 )
L241.pcgdp_2010$X2010 <- L241.pcgdp_2010$X2010 * conv_1990_2010_USD
L241.high_reg <- L241.pcgdp_2010$region[ L241.pcgdp_2010$X2010 > hi_growth_pcgdp ]
L241.low_reg <- L241.pcgdp_2010$region[ L241.pcgdp_2010$X2010 < lo_growth_pcgdp ]

#Create table of regions, technologies, and all base years
# NOTE: Easiest if the model base years are subsetted from a full table as a last step in the construction of each of these tables
A_demand_technology_R <- write_to_all_regions_ag( A_demand_technology, c( names_Tech, "minicam.energy.input", "market.name" ) )
A_demand_technology_R$stub.technology <- A_demand_technology_R$technology

A_an_input_technology_R <- write_to_all_regions_ag( A_an_input_technology, c( names_Tech, "minicam.energy.input", "market.name" ) )
A_an_input_technology_R$stub.technology <- A_an_input_technology_R$technology

#Adding lookup vectors to level1 output tables
printlog( "Adding region names to Level1 data tables" )
aglu_demand_calyears <- historical_years[ historical_years %in% model_years ]
aglu_demand_futureyears <- c( model_years[ !model_years %in% aglu_demand_calyears ] )
L241.ag_kcalg_R_C_Y.melt <- interpolate_and_melt( L101.ag_kcalg_R_C_Y, aglu_demand_calyears )
L241.ag_kcalg_R_C_Y.melt  <- add_region_name( L241.ag_kcalg_R_C_Y.melt )
L241.ag_kcalg_R_C_Yf.melt <- subset( L241.ag_kcalg_R_C_Y.melt, L241.ag_kcalg_R_C_Y.melt$year == max( aglu_demand_calyears ) )

printlog( "L241.StubAgTradeCoeff_food: coefficient for ag_trade of food crops (incl secondary products)" )
L241.StubAgTradeCoeff_food <- subset( A_demand_technology_R, supplysector == "FoodDemand_Crops" )
L241.StubAgTradeCoeff_food$coefficient <- local_food_fract / round( L241.ag_kcalg_R_C_Yf.melt$value[
      match( vecpaste( L241.StubAgTradeCoeff_food[ c( "region", "technology" ) ] ),
             vecpaste( L241.ag_kcalg_R_C_Yf.melt[ c( "region", C ) ] ) ) ],
      digits_calOutput )
L241.StubAgTradeCoeff_food$minicam.energy.input <- "ag_trade"
L241.StubAgTradeCoeff_food <- repeat_and_add_vector( L241.StubAgTradeCoeff_food, "year", aglu_demand_futureyears )
L241.StubAgTradeCoeff_food <- L241.StubAgTradeCoeff_food[ names_StubTechCoef_NM ]

printlog( "L241.StubAgTradeCoeff_nonfood: coefficient for ag_trade of non-food crops (incl secondary products)" )
L241.StubAgTradeCoeff_nonfood <- subset( A_demand_technology_R, supplysector == "NonFoodDemand_Crops" )
L241.StubAgTradeCoeff_nonfood$coefficient <- local_food_fract 
L241.StubAgTradeCoeff_nonfood$minicam.energy.input <- "ag_trade"
L241.StubAgTradeCoeff_nonfood <- repeat_and_add_vector( L241.StubAgTradeCoeff_nonfood, "year", aglu_demand_futureyears )
L241.StubAgTradeCoeff_nonfood <- L241.StubAgTradeCoeff_nonfood[ names_StubTechCoef_NM ]

printlog( "L241.StubAgTradeCoeff_feed: coefficient for ag_trade of feed crops (incl secondary products)" )
L241.StubAgTradeCoeff_feed <- subset( A_an_input_technology_R, supplysector == "FeedCrops" )
L241.StubAgTradeCoeff_feed$coefficient <- local_food_fract 
L241.StubAgTradeCoeff_feed$minicam.energy.input <- "ag_trade"
L241.StubAgTradeCoeff_feed <- repeat_and_add_vector( L241.StubAgTradeCoeff_feed, "year", aglu_demand_futureyears )
L241.StubAgTradeCoeff_feed <- L241.StubAgTradeCoeff_feed[ names_StubTechCoef_NM ]

printlog( "L241.AgProdTech_RES_output: output coefficient for RES ag_trade of food crops (incl secondary products)" )
L241.AgProdTech_RES_output <- repeat_and_add_vector( A_agSupplySector, "region", GCAM_region_names$region )
L241.AgProdTech_RES_output <- subset( L241.AgProdTech_RES_output, L241.AgProdTech_RES_output$AgSupplySector %!in% c( "Pasture", "Forest", "biomass" ))
L241.AgProdTech_RES_output <- repeat_and_add_vector( L241.AgProdTech_RES_output, "AEZ", AEZs )
L241.AgProdTech_RES_output$AgSupplySubsector <- paste( L241.AgProdTech_RES_output$AgSupplySector, L241.AgProdTech_RES_output$AEZ, sep="" )
L241.AgProdTech_RES_output <- repeat_and_add_vector( L241.AgProdTech_RES_output, "IRR_RFD", c( "IRR", "RFD" ) )
L241.AgProdTech_RES_output$AgProductionTechnology <- paste( L241.AgProdTech_RES_output$AgSupplySubsector, L241.AgProdTech_RES_output$IRR_RFD, sep="" )
L241.AgProdTech_RES_output <- repeat_and_add_vector( L241.AgProdTech_RES_output, "year", aglu_demand_futureyears )
L241.AgProdTech_RES_output$res.secondary.output <- "ag_trade"
L241.AgProdTech_RES_output$output.ratio <- 1
L241.AgProdTech_RES_output <- L241.AgProdTech_RES_output[ names_AgRES ]

printlog( "L241.RES_Market: market for policy portfolio" )
L241.RES_Market <- GCAM_region_names
L241.RES_Market$policy.portfolio.standard <- "ag_trade"
L241.RES_Market$market <- "Med_to_High_Income"
L241.RES_Market$market[ L241.RES_Market$region %in% L241.low_reg ] <- L241.RES_Market$region[ L241.RES_Market$region %in% L241.low_reg ]
L241.RES_Market$policyType <- "RES"
L241.RES_Market <- repeat_and_add_vector( L241.RES_Market, "year", aglu_demand_futureyears )
L241.RES_Market$constraint <- 1
L241.RES_Market <- L241.RES_Market[ names( L241.RES_Market ) != "GCAM_region_ID" ]

#Remove any regions for which agriculture and land use are not modeled
L241.StubAgTradeCoeff_food       <- subset( L241.StubAgTradeCoeff_food, !region %in% no_aglu_regions )
L241.StubAgTradeCoeff_nonfood    <- subset( L241.StubAgTradeCoeff_nonfood, !region %in% no_aglu_regions )
L241.StubAgTradeCoeff_feed       <- subset( L241.StubAgTradeCoeff_feed, !region %in% no_aglu_regions )
L241.AgProdTech_RES_output       <- subset( L241.AgProdTech_RES_output, !region %in% no_aglu_regions )
L241.RES_Market                  <- subset( L241.RES_Market, !region %in% no_aglu_regions )
L241.AgProdTech_RES_output       <- remove_AEZ_nonexist( L241.AgProdTech_RES_output )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L241.StubAgTradeCoeff_food, "StubTechCoef_NM", "AGLU_LEVEL2_DATA", "L241.StubAgTradeCoeff_food", "AGLU_XML_BATCH", "batch_ssp4_trade.xml" )
write_mi_data( L241.StubAgTradeCoeff_nonfood, "StubTechCoef_NM", "AGLU_LEVEL2_DATA", "L241.StubAgTradeCoeff_nonfood", "AGLU_XML_BATCH", "batch_ssp4_trade.xml" )
write_mi_data( L241.StubAgTradeCoeff_feed, "StubTechCoef_NM", "AGLU_LEVEL2_DATA", "L241.StubAgTradeCoeff_feed", "AGLU_XML_BATCH", "batch_ssp4_trade.xml" )
write_mi_data( L241.AgProdTech_RES_output, "AgRES", "AGLU_LEVEL2_DATA", "L241.AgProdTech_RES_output", "AGLU_XML_BATCH", "batch_ssp4_trade.xml" )
write_mi_data( L241.RES_Market, "PolicyPortfolioStd", "AGLU_LEVEL2_DATA", "L241.RES_Market", "AGLU_XML_BATCH", "batch_ssp4_trade.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ssp4_trade.xml", "AGLU_XML_FINAL", "ssp4_trade.xml", "", xml_tag="outFile" )

logstop()
