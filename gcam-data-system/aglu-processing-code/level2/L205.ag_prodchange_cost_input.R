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
logstart( "L205.ag_prodchange_cost_input.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for agricultural costs and productivity change" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_bio_cost_yield <- readdata( "AGLU_ASSUMPTIONS", "A_bio_cost_yield" )
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ" )
L113.ag_bioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L113.ag_bioYield_GJm2_R_AEZ_ref" )
L114.ag_YieldRatio_R_C_Y_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L114.ag_YieldRatio_R_C_Y_AEZ_ref" )
L114.bio_YieldRatio_R_AEZ_Y_ref <- readdata( "AGLU_LEVEL1_DATA", "L114.bio_YieldRatio_R_AEZ_Y_ref" )
L115.ag_CCI_rcp_gcm_cm_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L115.ag_CCI_rcp_gcm_cm_R_C_AEZ" )
L115.bio_CCI_rcp_gcm_cm_R_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L115.bio_CCI_rcp_gcm_cm_R_AEZ" )
L122.ag_EcYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_EcYield_kgm2_R_C_Y_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L133.ag_Cost_75USDkg_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L133.ag_Cost_75USDkg_C_AEZ" )
L102.pcgdp_thous90USD_SSP_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_SSP_R_Y")

# -----------------------------------------------------------------------------

printlog( "Merging biomass and ag crops into the same dataframes, for yields and yield ratios" )
#Yields
L205.ag_bioYield_GJm2_R_AEZ_ref <- melt( L113.ag_bioYield_GJm2_R_AEZ_ref, id.vars = R, variable.name = AEZ )
L205.ag_bioYield_GJm2_R_AEZ_ref[[C]] <- "biomass"
X_yield_hist_years <- X_historical_years[ X_historical_years %in% c( X_model_base_years, X_model_future_years ) ]
L205.ag_bioYield_GJm2_R_AEZ_ref[ X_yield_hist_years ] <- L205.ag_bioYield_GJm2_R_AEZ_ref[ "value" ]
L205.ag_EcYield_kgm2_R_C_Y_AEZ <- L122.ag_EcYield_kgm2_R_C_Y_AEZ[ c( R_C_AEZ, X_yield_hist_years ) ]
L205.ag_EcYield_kgm2_R_C_Y_AEZ <- rbind( L205.ag_EcYield_kgm2_R_C_Y_AEZ, L205.ag_bioYield_GJm2_R_AEZ_ref[ names( L205.ag_EcYield_kgm2_R_C_Y_AEZ ) ] )

#Yield ratios
L114.bio_YieldRatio_R_AEZ_Y_ref[[C]] <- "biomass"
L205.ag_YieldRatio_R_C_Y_AEZ_ref <- rbind( L114.ag_YieldRatio_R_C_Y_AEZ_ref, L114.bio_YieldRatio_R_AEZ_Y_ref )

L115.bio_CCI_rcp_gcm_cm_R_AEZ[[C]] <- "biomass"
L205.ag_CCI_rcp_gcm_cm_R_C_AEZ <- rbind( L115.ag_CCI_rcp_gcm_cm_R_C_AEZ, L115.bio_CCI_rcp_gcm_cm_R_AEZ )

printlog( "Calculating crop yields in all time periods for the reference scenario" )
L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt <- melt( L205.ag_YieldRatio_R_C_Y_AEZ_ref, id.vars = R_C_Y, variable.name = AEZ )
#Only use the yield ratios in years beyond the historical time series, for which actual yield data is available
L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt <- subset( L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt, year > max( historical_years ) )
L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt$Xyear <- paste0( "X", L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt$year )
L205.ag_YieldRatio_R_C_Y_AEZ_ref <- dcast( L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt, GCAM_region_ID + GCAM_commodity + AEZ ~ Xyear )

#Only multiply in the ratios for "model future years" that are greater than the last historical year
#Distinction applies for using earlier final calibration years
X_yieldratio_years <- sort( unique( L205.ag_YieldRatio_R_C_Y_AEZ_ref.melt$Xyear ) )
L205.ag_EcYield_kgm2_R_C_Y_AEZ[ X_yieldratio_years ] <- L205.ag_EcYield_kgm2_R_C_Y_AEZ[[ X_final_historical_year ]] *
      L205.ag_YieldRatio_R_C_Y_AEZ_ref[
           match( vecpaste( L205.ag_EcYield_kgm2_R_C_Y_AEZ[ R_C_AEZ ] ), vecpaste( L205.ag_YieldRatio_R_C_Y_AEZ_ref[ R_C_AEZ ] ) ),
      X_yieldratio_years ]

#Some small region x crop combinations aren't in the future yield improvement dataset. Copy yields forward from final historical period
L205.ag_EcYield_kgm2_R_C_Y_AEZ_NAs <- L205.ag_EcYield_kgm2_R_C_Y_AEZ[ !complete.cases( L205.ag_EcYield_kgm2_R_C_Y_AEZ ), ]
L205.ag_EcYield_kgm2_R_C_Y_AEZ_NAs[ X_yieldratio_years ] <- L205.ag_EcYield_kgm2_R_C_Y_AEZ_NAs[[ X_final_historical_year ]]
L205.ag_EcYield_kgm2_R_C_Y_AEZ[ !complete.cases( L205.ag_EcYield_kgm2_R_C_Y_AEZ ), ] <- L205.ag_EcYield_kgm2_R_C_Y_AEZ_NAs

#Scenarios with climate impacts
#First, subset only the region/commodity/aez combinations that have climate impacts
L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ <- L205.ag_EcYield_kgm2_R_C_Y_AEZ[ vecpaste( L205.ag_EcYield_kgm2_R_C_Y_AEZ[ R_C_AEZ ] ) %in%
      vecpaste( L205.ag_CCI_rcp_gcm_cm_R_C_AEZ[ R_C_AEZ ] ), ]
L205.all_rcp_gcm_cm <- unique( L205.ag_CCI_rcp_gcm_cm_R_C_AEZ[ rcp_gcm_cm ] )
L205.all_rcp_gcm_cm$scenID <- 1:nrow( L205.all_rcp_gcm_cm )
L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ <- repeat_and_add_vector( L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ, "scenID", L205.all_rcp_gcm_cm$scenID )
L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ[ rcp_gcm_cm ] <- L205.all_rcp_gcm_cm[
      match( L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ$scenID, L205.all_rcp_gcm_cm$scenID ), rcp_gcm_cm]

#Then, multiply the base yield ratios by the climate impact yield ratios
L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ[ X_yieldratio_years ] <- L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ[ X_yieldratio_years ] *
      L205.ag_CCI_rcp_gcm_cm_R_C_AEZ[ match(
          vecpaste( L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ[ R_C_AEZ ] ),
          vecpaste( L205.ag_CCI_rcp_gcm_cm_R_C_AEZ[ R_C_AEZ ] ) ),
      X_yieldratio_years ]

printlog( "Translating yields to rates of agricultural productivity change" )
#Build a dataframe of the timestep lengths, of the same dimensions as the yield dataframe (except without the initial period)
timesteps <- model_years[ 2:length( model_years ) ] - model_years[ 1:( length( model_years ) -1 ) ]
names( timesteps ) <- X_model_years[ 2:length( X_model_years ) ]
df.timesteps <- t( as.data.frame( timesteps ) )
df.timesteps <- df.timesteps[ rep( 1, length.out = nrow( L205.ag_EcYield_kgm2_R_C_Y_AEZ ) ), ]
df.timesteps_scen <- df.timesteps[ rep( 1, length.out = nrow( L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ ) ), ]

#Calculate the yield ratio from the prior period in the yield dataframe
L205.ag_YieldRatio_R_C_Y_AEZ_ref <- L205.ag_EcYield_kgm2_R_C_Y_AEZ[ c( R_C_AEZ, X_model_years[ 2:length( X_model_years ) ] ) ]
L205.ag_YieldRatio_R_C_Y_AEZ_ref[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L205.ag_EcYield_kgm2_R_C_Y_AEZ[ X_model_years[ 2:length( X_model_years ) ] ] /
      L205.ag_EcYield_kgm2_R_C_Y_AEZ[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L205.ag_YieldRatio_R_C_Y_AEZ_ref[ is.na( L205.ag_YieldRatio_R_C_Y_AEZ_ref ) ] <- 1

L205.ag_YieldRatio_scen_R_C_Y_AEZ <- L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ[ c( "scenID", rcp_gcm_cm, R_C_AEZ, X_model_years[ 2:length( X_model_years ) ] ) ]
L205.ag_YieldRatio_scen_R_C_Y_AEZ[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ[ X_model_years[ 2:length( X_model_years ) ] ] /
      L205.ag_EcYield_kgm2_scen_R_C_Y_AEZ[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L205.ag_YieldRatio_scen_R_C_Y_AEZ[ is.na( L205.ag_YieldRatio_scen_R_C_Y_AEZ ) ] <- 1

#Solve for the yield rate using the ratio and the timestep
L205.ag_YieldRate_R_C_Y_AEZ_ref <- L205.ag_YieldRatio_R_C_Y_AEZ_ref
L205.ag_YieldRate_R_C_Y_AEZ_ref[ X_model_years[ 2:length( X_model_years ) ] ] <-
      ( L205.ag_YieldRatio_R_C_Y_AEZ_ref[ X_model_years[ 2:length( X_model_years ) ] ] ) ^
      (1 / df.timesteps ) - 1

L205.ag_YieldRate_scen_R_C_Y_AEZ <- L205.ag_YieldRatio_scen_R_C_Y_AEZ
L205.ag_YieldRate_scen_R_C_Y_AEZ[ X_model_years[ 2:length( X_model_years ) ] ] <-
      ( L205.ag_YieldRatio_scen_R_C_Y_AEZ[ X_model_years[ 2:length( X_model_years ) ] ] ) ^
      (1 / df.timesteps ) - 1

printlog( "Table L205.AgProdChange_ref: Reference scenario ag prod change (not incl biomass)" )
L205.AgProdChange_ref <- melt( L205.ag_YieldRate_R_C_Y_AEZ_ref, id.vars = R_C_AEZ, variable.name = "Xyear" )
L205.AgProdChange_ref$year <- sub( "X", "", L205.AgProdChange_ref$Xyear )
L205.AgProdChange_ref$AgProdChange <- round( L205.AgProdChange_ref$value, digits_AgProdChange )
L205.AgProdChange_ref <- subset( L205.AgProdChange_ref, year %in% model_future_years )
L205.AgProdChange_ref <- add_region_name( L205.AgProdChange_ref )
L205.AgProdChange_ref <- add_agtech_names( L205.AgProdChange_ref )[ names_AgProdChange ]
#If the final calibration year is less than the final historical year, this method will return Inf for crops that are 0 in one year
# and non-zero in subsequent years. e.g. Korea and FSU FodderGrass. Setting the agprodchange to 0, and keeping these techs out.
L205.AgProdChange_ref[ L205.AgProdChange_ref == Inf ] <- 0

printlog( "Renaming specified bioenergy crops in the productivity change tables" )
L205.AgProdChange_ref <- rename_biocrops( L205.AgProdChange_ref, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

# Climate impacts scenarios: Follow the same steps, but with all scenarios in a single table
L205.ag_YieldRate_scen_R_C_Y_AEZ.melt <- melt( L205.ag_YieldRate_scen_R_C_Y_AEZ, id.vars = c( "scenID", rcp_gcm_cm, R_C_AEZ ), variable.name = "Xyear" )
L205.ag_YieldRate_scen_R_C_Y_AEZ.melt$year <- sub( "X", "", L205.ag_YieldRate_scen_R_C_Y_AEZ.melt$Xyear )
L205.ag_YieldRate_scen_R_C_Y_AEZ.melt$AgProdChange <- round( L205.ag_YieldRate_scen_R_C_Y_AEZ.melt$value, digits_AgProdChange )
L205.ag_YieldRate_scen_R_C_Y_AEZ.melt <- subset( L205.ag_YieldRate_scen_R_C_Y_AEZ.melt, year %in% model_future_years )
L205.ag_YieldRate_scen_R_C_Y_AEZ.melt <- add_region_name( L205.ag_YieldRate_scen_R_C_Y_AEZ.melt )
L205.ag_YieldRate_scen_R_C_Y_AEZ.melt <- add_agtech_names( L205.ag_YieldRate_scen_R_C_Y_AEZ.melt )
L205.ag_YieldRate_scen_R_C_Y_AEZ.melt[ L205.ag_YieldRate_scen_R_C_Y_AEZ.melt == Inf ] <- 0

#Then, split the CCI scenarios into different tables, assigning object names based on the rcp x gcm x crop model identifiers
# NOTE: NEED TO WRITE OUT THE DATA IN THIS STEP TOO. not sure how to split object from object name if referred to later
for( i in 1:max( L205.ag_YieldRate_scen_R_C_Y_AEZ.melt$scenID ) ){
	scenarios <- unique( L205.ag_YieldRate_scen_R_C_Y_AEZ.melt[ c( "scenID", rcp_gcm_cm ) ] )
	scen_strings <- paste( scenarios$rcp, scenarios$gcm, scenarios$cropmodel, sep = "_" )
	objectname <- paste0( "L205.AgProdChange_", scen_strings[i] )
	object <- subset( L205.ag_YieldRate_scen_R_C_Y_AEZ.melt, scenID == i )[ names_AgProdChange ]
	object <- rename_biocrops( object, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
              lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
	object <- remove_AEZ_nonexist( object )
	assign( objectname, object )
	batchXMLstring <- paste0( "batch_ag_prodchange_",
	                  substr( objectname, regexpr( "AgProdChange_", objectname ) + 13, nchar( objectname ) ),
	                  ".xml" )
	write_mi_data( object, "AgProdChange", "AGLU_LEVEL2_DATA", objectname, "AGLU_XML_BATCH", batchXMLstring )
	XMLstring <- sub( "batch_", "", batchXMLstring )
	insert_file_into_batchxml( "AGLU_XML_BATCH", batchXMLstring, "AGLU_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}

#COSTS
#Round costs to specified number of digits; this is ready for model input
L205.ag_Cost_75USDkg_C_AEZ <- L133.ag_Cost_75USDkg_C_AEZ
L205.ag_Cost_75USDkg_C_AEZ[ AEZs ] <- round( L205.ag_Cost_75USDkg_C_AEZ[ AEZs ], digits_calPrice )

printlog( "Table L205.AgCost_ag: costs of ag commodities (excl forest and biomass)" )
#Melt table of costs and write it out to all regions and time periods
L205.ag_Cost_75USDkg_C_AEZ.melt <- melt( L205.ag_Cost_75USDkg_C_AEZ, id.vars = C, variable.name = AEZ )
names( L205.ag_Cost_75USDkg_C_AEZ.melt )[ names( L205.ag_Cost_75USDkg_C_AEZ.melt ) == "value" ] <- "nonLandVariableCost"
L205.ag_Cost_75USDkg_C_AEZ.melt <- add_agtech_names( L205.ag_Cost_75USDkg_C_AEZ.melt )
L205.ag_Cost_75USDkg_C_AEZ.melt_repR <- repeat_and_add_vector( L205.ag_Cost_75USDkg_C_AEZ.melt, R, GCAM_region_names[[R]] )
L205.ag_Cost_75USDkg_C_AEZ.melt_repR <- add_region_name( L205.ag_Cost_75USDkg_C_AEZ.melt_repR )
L205.ag_Cost_75USDkg_C_AEZ.melt_repR_Y <- repeat_and_add_vector( L205.ag_Cost_75USDkg_C_AEZ.melt_repR, Y, model_years )
L205.AgCost_ag <- L205.ag_Cost_75USDkg_C_AEZ.melt_repR_Y[ names_AgCost ]

printlog( "Table L205.AgCost_For: costs of forest production" )
L205.AgCost_For <- data.frame( region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      GCAM_commodity = "Forest",
      AEZ = sort( rep( AEZs, times = nrow( GCAM_region_names ) ) ) )
L205.AgCost_For <- add_agtech_names( L205.AgCost_For )
L205.AgCost_For <- repeat_and_add_vector( L205.AgCost_For, Y, model_years )
L205.AgCost_For$nonLandVariableCost <- cost_For_75USDm3
L205.AgCost_For <- L205.AgCost_For[ names_AgCost ]

printlog( "Table L205.AgCost_bio: costs of bioenergy production" )
L205.AgCost_bio <- data.frame( region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      GCAM_commodity = "biomass",
      AEZ = sort( rep( AEZs, times = nrow( GCAM_region_names ) ) ), stringsAsFactors = F )
L205.AgCost_bio <- add_agtech_names( L205.AgCost_bio )
L205.AgCost_bio <- repeat_and_add_vector( L205.AgCost_bio, Y, model_years )

#NOTE: costs differ for the different specified bioenergy crops, so replace the names prior to matching in costs
printlog( "Renaming specified bioenergy crops prior to matching in the costs" )
L205.AgCost_bio <- rename_biocrops( L205.AgCost_bio, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ) 

#Map in costs from assumptions table
L205.AgCost_bio$nonLandVariableCost <- A_bio_cost_yield$cost_USDGJ[
      match( L205.AgCost_bio$GCAM_commodity, A_bio_cost_yield$GCAM_commodity ) ]

#Reset costs in hi-cost AEZs
L205.AgCost_bio$nonLandVariableCost[ L205.AgCost_bio$AEZ %in% AEZs_hi_bio_cost ] <- A_bio_cost_yield$cost_hi_USDGJ[
      match( L205.AgCost_bio$GCAM_commodity[ L205.AgCost_bio$AEZ %in% AEZs_hi_bio_cost ],
             A_bio_cost_yield$GCAM_commodity ) ]

#Use lower costs in AEZ07 in USA and Canada
L205.AgCost_bio$nonLandVariableCost[ L205.AgCost_bio$AEZ == "AEZ07" &
      L205.AgCost_bio$region %in% c( "USA", "Canada" ) ] <- A_bio_cost_yield$cost_USDGJ[
           match( L205.AgCost_bio$GCAM_commodity[ L205.AgCost_bio$AEZ == "AEZ07" &
                  L205.AgCost_bio$region %in% c( "USA", "Canada" ) ], A_bio_cost_yield$GCAM_commodity ) ]
L205.AgCost_bio <- L205.AgCost_bio[ names_AgCost ]

printlog( "Table L205.AgProdChange_high: High ag prod change (not incl biomass)" )
L205.AgProdChange_high <- L205.AgProdChange_ref
L205.AgProdChange_high$AgProdChange <- L205.AgProdChange_high$AgProdChange * hi_ag_prod_growth_mult

printlog( "Table L205.AgProdChange_low: Low ag prod change (not incl biomass)" )
L205.AgProdChange_low <- L205.AgProdChange_ref
L205.AgProdChange_low$AgProdChange <- L205.AgProdChange_low$AgProdChange * low_ag_prod_growth_mult

printlog( "Table L205.AgProdChange_SSP4: SSP4 ag prod change (not incl biomass)" )
L205.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_SSP_R_Y, L102.pcgdp_thous90USD_SSP_R_Y$scenario == "SSP4" )
L205.pcgdp_2010 <- L205.pcgdp_2010[ names( L205.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L205.pcgdp_2010 <- add_region_name( L205.pcgdp_2010 )
L205.pcgdp_2010$X2010 <- L205.pcgdp_2010$X2010 * conv_1990_2010_USD
L205.high_reg <- L205.pcgdp_2010$region[ L205.pcgdp_2010$X2010 > hi_growth_pcgdp ]
L205.low_reg <- L205.pcgdp_2010$region[ L205.pcgdp_2010$X2010 < lo_growth_pcgdp ]

L205.AgProdChange_ssp4_lo <- subset( L205.AgProdChange_low, L205.AgProdChange_low$region %in% L205.low_reg )
L205.AgProdChange_ssp4_hi <- subset( L205.AgProdChange_high, L205.AgProdChange_high$region %in% L205.high_reg )
L205.AgProdChange_ssp4_med <- subset( L205.AgProdChange_ref, L205.AgProdChange_ref$region %!in% c( L205.high_reg, L205.low_reg ) )
L205.AgProdChange_ssp4 <- rbind( L205.AgProdChange_ssp4_lo, L205.AgProdChange_ssp4_med, L205.AgProdChange_ssp4_hi )

printlog( "Removing non-existent AEZs from all tables")
L205.AgProdChange_ref <- remove_AEZ_nonexist( L205.AgProdChange_ref )
L205.AgCost_ag <- remove_AEZ_nonexist( L205.AgCost_ag )
L205.AgCost_For <- remove_AEZ_nonexist( L205.AgCost_For )
L205.AgCost_bio <- remove_AEZ_nonexist( L205.AgCost_bio )
L205.AgProdChange_low <- remove_AEZ_nonexist( L205.AgProdChange_low )
L205.AgProdChange_high <- remove_AEZ_nonexist( L205.AgProdChange_high )
L205.AgProdChange_ssp4 <- remove_AEZ_nonexist( L205.AgProdChange_ssp4 )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L205.AgProdChange_ref, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ref",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ref.xml" ) 
write_mi_data( L205.AgCost_ag, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_ag", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 
write_mi_data( L205.AgCost_For, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_For", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 
write_mi_data( L205.AgCost_bio, "AgCost", "AGLU_LEVEL2_DATA", "L205.AgCost_bio", "AGLU_XML_BATCH", "batch_ag_cost.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ref.xml", "AGLU_XML_FINAL", "ag_prodchange_ref.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_cost.xml", "AGLU_XML_FINAL", "ag_cost.xml", "", xml_tag="outFile" )

write_mi_data( L205.AgProdChange_high, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp1",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp1.xml" ) 
write_mi_data( L205.AgProdChange_ref, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp2",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp2.xml" ) 
write_mi_data( L205.AgProdChange_low, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp3",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp3.xml" ) 
write_mi_data( L205.AgProdChange_ssp4, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp4",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp4.xml" ) 
write_mi_data( L205.AgProdChange_high, IDstring="AgProdChange", domain="AGLU_LEVEL2_DATA", fn="L205.AgProdChange_ssp5",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_prodchange_ssp5.xml" ) 
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp1.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp1.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp2.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp2.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp3.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp3.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp4.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp4.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp5.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp5.xml", "", xml_tag="outFile" )


logstop()
