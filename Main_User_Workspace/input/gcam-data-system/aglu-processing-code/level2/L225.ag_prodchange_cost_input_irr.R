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
logstart( "L225.ag_prodchange_cost_input_irr.R" )
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
A_biocrops_R_AEZ_irr <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_irr" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L162.ag_irrYieldRatio_R_C_Y_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L162.ag_irrYieldRatio_R_C_Y_AEZ_ref" )
L162.ag_rfdYieldRatio_R_C_Y_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L162.ag_rfdYieldRatio_R_C_Y_AEZ_ref" )
L162.bio_irrYieldRatio_R_Y_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L162.bio_irrYieldRatio_R_Y_AEZ_ref" )
L162.bio_rfdYieldRatio_R_Y_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L162.bio_rfdYieldRatio_R_Y_AEZ_ref" )
L163.ag_irrBioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_AEZ_ref" )
L163.ag_rfdBioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_AEZ_ref" )
L164.ag_Cost_75USDkg_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L164.ag_Cost_75USDkg_C_AEZ" )
L171.ag_irrEcYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_irrEcYield_kgm2_R_C_Y_AEZ" )
L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ" )
L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr <- readdata( "AGLU_LEVEL1_DATA", "L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr" )
L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr <- readdata( "AGLU_LEVEL1_DATA", "L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr" )
L102.pcgdp_thous90USD_SSP_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_SSP_R_Y")

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Merging irrigated and rainfed dataframes with an identifier column" )
L162.ag_irrYieldRatio_R_C_Y_AEZ_ref$Irr_Rfd <- "IRR"
L162.ag_rfdYieldRatio_R_C_Y_AEZ_ref$Irr_Rfd <- "RFD"
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref <- rbind( L162.ag_irrYieldRatio_R_C_Y_AEZ_ref, L162.ag_rfdYieldRatio_R_C_Y_AEZ_ref )

L162.bio_irrYieldRatio_R_Y_AEZ_ref $Irr_Rfd <- "IRR"
L162.bio_rfdYieldRatio_R_Y_AEZ_ref$Irr_Rfd <- "RFD"
L225.bio_YieldRatio_R_Y_AEZ_irr_ref <- rbind( L162.bio_irrYieldRatio_R_Y_AEZ_ref, L162.bio_rfdYieldRatio_R_Y_AEZ_ref )
L225.bio_YieldRatio_R_Y_AEZ_irr_ref[[C]] <- "biomass"

L163.ag_irrBioYield_GJm2_R_AEZ_ref$Irr_Rfd <- "IRR"
L163.ag_rfdBioYield_GJm2_R_AEZ_ref$Irr_Rfd <- "RFD"
L225.ag_BioYield_GJm2_R_AEZ_irr_ref <- rbind( L163.ag_irrBioYield_GJm2_R_AEZ_ref, L163.ag_rfdBioYield_GJm2_R_AEZ_ref )
L225.ag_BioYield_GJm2_R_AEZ_irr_ref[[C]] <- "biomass"

L171.ag_irrEcYield_kgm2_R_C_Y_AEZ$Irr_Rfd <- "IRR"
L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ$Irr_Rfd <- "RFD"
L225.ag_irrEcYield_kgm2_R_C_Y_AEZ_irr <- rbind( L171.ag_irrEcYield_kgm2_R_C_Y_AEZ, L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ )

printlog( "Merging biomass with agricultural crops for future yield calculation")
#Yields
L225.ag_bioYield_GJm2_R_AEZ_irr_ref.melt <- melt( L225.ag_BioYield_GJm2_R_AEZ_irr_ref, id.vars = R_C_irr, variable.name = AEZ )
X_yield_hist_years <- X_historical_years[ X_historical_years %in% c( X_model_base_years, X_model_future_years ) ]
L225.ag_bioYield_GJm2_R_AEZ_irr_ref.melt[ X_yield_hist_years ] <- L225.ag_bioYield_GJm2_R_AEZ_irr_ref.melt[ "value" ]
L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr <- rbind(
      L225.ag_irrEcYield_kgm2_R_C_Y_AEZ_irr[ c( R_C_AEZ_irr, X_yield_hist_years ) ],
      L225.ag_bioYield_GJm2_R_AEZ_irr_ref.melt[ c( R_C_AEZ_irr, X_yield_hist_years ) ] )

#Yield ratios
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref <- rbind( L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref, L225.bio_YieldRatio_R_Y_AEZ_irr_ref )

#Climate change impacts
L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr[[C]] <- "biomass"
L225.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr <- rbind( L172.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr, L172.bio_CCI_rcp_gcm_cm_R_AEZ_irr )

printlog( "Calculating crop yields in all time periods for the reference scenario" )
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref.melt <- melt( L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref, id.vars = R_C_Y_irr, variable.name = AEZ )
# Only use the yield ratios in years beyond the historical time series, as actual yields are available during the historical period
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref.melt <- subset( L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref.melt, year > max( historical_years ) )
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref.melt$Xyear <- paste0( "X", L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref.melt$year )
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref <- dcast( L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref.melt, GCAM_region_ID + GCAM_commodity + AEZ + Irr_Rfd ~ Xyear )

#Only multiply in the ratios for "model future years" that are greater than the last historical year
#Distinction applies for using earlier final calibration years
X_yieldratio_years <- sort( unique( L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref.melt$Xyear ) )
L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[ X_yieldratio_years ] <- L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[[ X_final_historical_year ]] *
      L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref[
           match( vecpaste( L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[ R_C_AEZ_irr ] ), vecpaste( L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref[ R_C_AEZ_irr ] ) ),
      X_yieldratio_years ]

#Some region x crop combinations aren't in the future yield improvement dataset. Copy yields forward from final historical period
L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[ !complete.cases( L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr ), X_yieldratio_years ] <-
      L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[[ X_final_historical_year ]][ !complete.cases( L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr ) ]

#Scenarios with climate impacts
#First, subset only the region/commodity/aez combinations that have climate impacts
L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr <- L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[
      vecpaste( L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[ R_C_AEZ_irr ] ) %in%
      vecpaste( L225.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ R_C_AEZ_irr ] ), ]
L225.all_rcp_gcm_cm <- unique( L225.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ rcp_gcm_cm ] )
L225.all_rcp_gcm_cm$scenID <- 1:nrow( L225.all_rcp_gcm_cm )
L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr <- repeat_and_add_vector( L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr, "scenID", L225.all_rcp_gcm_cm$scenID )
L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr[ rcp_gcm_cm ] <- L225.all_rcp_gcm_cm[
      match( L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr $scenID, L225.all_rcp_gcm_cm$scenID ), rcp_gcm_cm]

#Then, multiply the reference yields by the climate impact yield ratios
L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr[ X_yieldratio_years ] <- L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr[ X_yieldratio_years ] *
      L225.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ match(
          vecpaste( L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr[ R_C_AEZ ] ),
          vecpaste( L225.ag_CCI_rcp_gcm_cm_R_C_AEZ_irr[ R_C_AEZ ] ) ),
      X_yieldratio_years ]

printlog( "Translating yields to rates of agricultural productivity change" )
#Build a dataframe of the timestep lengths, of the same dimensions as the yield dataframe (except without the initial period)
timesteps <- model_years[ 2:length( model_years ) ] - model_years[ 1:( length( model_years ) -1 ) ]
names( timesteps ) <- X_model_years[ 2:length( X_model_years ) ]
df.timesteps <- t( as.data.frame( timesteps ) )
df.timesteps <- df.timesteps[ rep( 1, length.out = nrow( L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr ) ), ]
df.timesteps_scen <- df.timesteps[ rep( 1, length.out = nrow( L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr ) ), ]

#Calculate the yield ratio from the prior period in the yield dataframe
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref <- L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[ c( R_C_AEZ_irr, X_model_years[ 2:length( X_model_years ) ] ) ]
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[ X_model_years[ 2:length( X_model_years ) ] ] /
      L225.ag_EcYield_kgm2_R_C_Y_AEZ_irr[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref[ is.na( L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref ) ] <- 1

L225.ag_YieldRatio_scen_R_C_Y_AEZ_irr <- L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr[ c( "scenID", rcp_gcm_cm, R_C_AEZ_irr, X_model_years[ 2:length( X_model_years ) ] ) ]
L225.ag_YieldRatio_scen_R_C_Y_AEZ_irr[ X_model_years[ 2:length( X_model_years ) ] ] <-
      L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr[ X_model_years[ 2:length( X_model_years ) ] ] /
      L225.ag_EcYield_kgm2_scen_R_C_Y_AEZ_irr[ X_model_years[ 1:( length( X_model_years ) - 1 ) ] ]
L225.ag_YieldRatio_scen_R_C_Y_AEZ_irr[ is.na( L225.ag_YieldRatio_scen_R_C_Y_AEZ_irr ) ] <- 1

#Solve for the yield rate using the ratio and the timestep
L225.ag_YieldRate_R_C_Y_AEZ_irr_ref <- L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref
L225.ag_YieldRate_R_C_Y_AEZ_irr_ref[ X_model_years[ 2:length( X_model_years ) ] ] <-
      ( L225.ag_YieldRatio_R_C_Y_AEZ_irr_ref[ X_model_years[ 2:length( X_model_years ) ] ] ) ^
      (1 / df.timesteps ) - 1

L225.ag_YieldRate_scen_R_C_Y_AEZ_irr <- L225.ag_YieldRatio_scen_R_C_Y_AEZ_irr
L225.ag_YieldRate_scen_R_C_Y_AEZ_irr[ X_model_years[ 2:length( X_model_years ) ] ] <-
      ( L225.ag_YieldRatio_scen_R_C_Y_AEZ_irr[ X_model_years[ 2:length( X_model_years ) ] ] ) ^
      (1 / df.timesteps ) - 1

printlog( "Table L225.AgProdChange_irr_ref: Reference scenario ag prod change (not incl biomass)" )
L225.AgProdChange_irr_ref <- melt( L225.ag_YieldRate_R_C_Y_AEZ_irr_ref, id.vars = R_C_AEZ_irr, variable.name = "Xyear" )
L225.AgProdChange_irr_ref$year <- sub( "X", "", L225.AgProdChange_irr_ref$Xyear )
L225.AgProdChange_irr_ref$AgProdChange <- round( L225.AgProdChange_irr_ref$value, digits_AgProdChange )
L225.AgProdChange_irr_ref <- subset( L225.AgProdChange_irr_ref, year %in% model_future_years )
L225.AgProdChange_irr_ref <- add_region_name( L225.AgProdChange_irr_ref )
L225.AgProdChange_irr_ref <- add_agtech_names( L225.AgProdChange_irr_ref )[ names_AgProdChange ]
#If the final calibration year is less than the final historical year, this method will return Inf for crops that are 0 in one year
# and non-zero in subsequent years. e.g. Korea and FSU FodderGrass. Setting the agprodchange to 0, and keeping these techs out.
L225.AgProdChange_irr_ref[ L225.AgProdChange_irr_ref == Inf ] <- 0

printlog( "Renaming specified bioenergy crops in the productivity change tables" )
L225.AgProdChange_irr_ref <- rename_biocrops( L225.AgProdChange_irr_ref, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgProductionTechnology",
      lookup_matchvar = "old_AgProductionTechnology", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

# Climate impacts scenarios: Follow the same steps, but with all scenarios in a single table
L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt <- melt( L225.ag_YieldRate_scen_R_C_Y_AEZ_irr, id.vars = c( "scenID", rcp_gcm_cm, R_C_AEZ_irr ), variable.name = "Xyear" )
L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt$year <- sub( "X", "", L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt$Xyear )
L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt$AgProdChange <- round( L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt$value, digits_AgProdChange )
L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt <- subset( L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt, year %in% model_future_years )
L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt <- add_region_name( L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt )
L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt <- add_agtech_names( L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt )
L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt[ L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt == Inf ] <- 0

#Then, split the CCI scenarios into different tables, assigning object names based on the rcp x gcm x crop model identifiers
# NOTE: This is done in the same loop that writes out the data (below). Not sure how to split object from object name if referred to later

#COSTS
#Round costs to specified number of digits; this is ready for model input
L225.ag_Cost_75USDkg_C_AEZ_irr <- L164.ag_Cost_75USDkg_C_AEZ
L225.ag_Cost_75USDkg_C_AEZ_irr[ AEZs ] <- round( L225.ag_Cost_75USDkg_C_AEZ_irr[ AEZs ], digits_calPrice )

printlog( "Table L225.AgCost_ag: costs of ag commodities (excl forest and biomass)" )
#Melt table of costs and write it out to all regions and time periods
L225.ag_Cost_75USDkg_C_AEZ_irr.melt <- melt( L225.ag_Cost_75USDkg_C_AEZ_irr, id.vars = C, variable.name = AEZ, value.name = "nonLandVariableCost" )

#Base costs are the same for rainfed and irrigated, so repeat by irrigation
L225.ag_Cost_75USDkg_C_AEZ_irr.melt <- repeat_and_add_vector( L225.ag_Cost_75USDkg_C_AEZ_irr.melt, irr, c( "IRR", "RFD" ) )
L225.ag_Cost_75USDkg_C_AEZ_irr.melt <- add_agtech_names( L225.ag_Cost_75USDkg_C_AEZ_irr.melt )
L225.ag_Cost_75USDkg_C_AEZ_irr.melt_repR <- repeat_and_add_vector( L225.ag_Cost_75USDkg_C_AEZ_irr.melt, R, GCAM_region_names[[R]] )
L225.ag_Cost_75USDkg_C_AEZ_irr.melt_repR <- add_region_name( L225.ag_Cost_75USDkg_C_AEZ_irr.melt_repR )
L225.ag_Cost_75USDkg_C_AEZ_irr.melt_repR_Y <- repeat_and_add_vector( L225.ag_Cost_75USDkg_C_AEZ_irr.melt_repR, Y, model_years )
L225.AgCost_ag <- L225.ag_Cost_75USDkg_C_AEZ_irr.melt_repR_Y[ names_AgCost ]

printlog( "Table L225.AgCost_For: costs of forest production" )
L225.AgCost_For <- data.frame( region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      GCAM_commodity = "Forest",
      AEZ = sort( rep( AEZs, times = nrow( GCAM_region_names ) ) ) )
L225.AgCost_For <- add_agtech_names( L225.AgCost_For )
L225.AgCost_For <- repeat_and_add_vector( L225.AgCost_For, Y, model_years )
L225.AgCost_For$nonLandVariableCost <- cost_For_75USDm3
L225.AgCost_For <- L225.AgCost_For[ names_AgCost ]

printlog( "Table L225.AgCost_bio: costs of bioenergy production" )
L225.AgCost_bio <- data.frame( region = rep( GCAM_region_names$region, times = length( AEZs ) ),
      GCAM_commodity = "biomass",
      AEZ = sort( rep( AEZs, times = nrow( GCAM_region_names ) ) ), stringsAsFactors = F )
L225.AgCost_bio <- repeat_and_add_vector( L225.AgCost_bio, irr, c( "IRR", "RFD" ) )
L225.AgCost_bio <- add_agtech_names( L225.AgCost_bio )
L225.AgCost_bio <- repeat_and_add_vector( L225.AgCost_bio, Y, model_years )

#NOTE: costs differ for the different specified bioenergy crops, so replace the names prior to matching in costs
printlog( "Renaming specified bioenergy crops prior to matching in the costs" )
L225.AgCost_bio <- rename_biocrops( L225.AgCost_bio, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgProductionTechnology",
      lookup_matchvar = "old_AgProductionTechnology", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" ) 

#Map in costs from assumptions table
L225.AgCost_bio$nonLandVariableCost <- A_bio_cost_yield$cost_USDGJ[
      match( L225.AgCost_bio$GCAM_commodity, A_bio_cost_yield$GCAM_commodity ) ]

#Reset costs in hi-cost AEZs
L225.AgCost_bio$nonLandVariableCost[ L225.AgCost_bio$AEZ %in% AEZs_hi_bio_cost ] <- A_bio_cost_yield$cost_hi_USDGJ[
      match( L225.AgCost_bio$GCAM_commodity[ L225.AgCost_bio$AEZ %in% AEZs_hi_bio_cost ],
             A_bio_cost_yield$GCAM_commodity ) ]

#Use lower costs in AEZ07 in USA and Canada
L225.AgCost_bio$nonLandVariableCost[ L225.AgCost_bio$AEZ == "AEZ07" &
      L225.AgCost_bio$region %in% c( "USA", "Canada" ) ] <- A_bio_cost_yield$cost_USDGJ[
           match( L225.AgCost_bio$GCAM_commodity[ L225.AgCost_bio$AEZ == "AEZ07" &
                  L225.AgCost_bio$region %in% c( "USA", "Canada" ) ], A_bio_cost_yield$GCAM_commodity ) ]
L225.AgCost_bio <- L225.AgCost_bio[ names_AgCost ]

printlog( "Table L225.AgProdChange_irr_high: High ag prod change (not incl biomass)" )
L225.AgProdChange_irr_high <- L225.AgProdChange_irr_ref
L225.AgProdChange_irr_high$AgProdChange <- L225.AgProdChange_irr_high$AgProdChange * hi_ag_prod_growth_mult

printlog( "Table L225.AgProdChange_irr_low: Low ag prod change (not incl biomass)" )
L225.AgProdChange_irr_low <- L225.AgProdChange_irr_ref
L225.AgProdChange_irr_low$AgProdChange <- L225.AgProdChange_irr_low$AgProdChange * low_ag_prod_growth_mult

printlog( "Table L225.AgProdChange_irr_ssp4: SSP4 ag prod change (not incl biomass)" )
L225.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_SSP_R_Y, L102.pcgdp_thous90USD_SSP_R_Y$scenario == "SSP4" )
L225.pcgdp_2010 <- L225.pcgdp_2010[ names( L225.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L225.pcgdp_2010 <- add_region_name( L225.pcgdp_2010 )
L225.pcgdp_2010$X2010 <- L225.pcgdp_2010$X2010 * conv_1990_2010_USD
L225.high_reg <- L225.pcgdp_2010$region[ L225.pcgdp_2010$X2010 > hi_growth_pcgdp ]
L225.low_reg <- L225.pcgdp_2010$region[ L225.pcgdp_2010$X2010 < lo_growth_pcgdp ]

L225.AgProdChange_irr_ssp4_lo <- subset( L225.AgProdChange_irr_low, L225.AgProdChange_irr_low$region %in% L225.low_reg )
L225.AgProdChange_irr_ssp4_hi <- subset( L225.AgProdChange_irr_high, L225.AgProdChange_irr_high$region %in% L225.high_reg )
L225.AgProdChange_irr_ssp4_med <- subset( L225.AgProdChange_irr_ref, L225.AgProdChange_irr_ref$region %!in% c( L225.high_reg, L225.low_reg ) )
L225.AgProdChange_irr_ssp4 <- rbind( L225.AgProdChange_irr_ssp4_lo, L225.AgProdChange_irr_ssp4_med, L225.AgProdChange_irr_ssp4_hi )

printlog( "Removing non-existent AEZs from all tables")
L225.AgProdChange_irr_ref <- remove_AEZ_nonexist( L225.AgProdChange_irr_ref )
L225.AgCost_ag <- remove_AEZ_nonexist( L225.AgCost_ag )
L225.AgCost_For <- remove_AEZ_nonexist( L225.AgCost_For )
L225.AgCost_bio <- remove_AEZ_nonexist( L225.AgCost_bio )
L225.AgProdChange_irr_low <- remove_AEZ_nonexist( L225.AgProdChange_irr_low )
L225.AgProdChange_irr_high <- remove_AEZ_nonexist( L225.AgProdChange_irr_high )
L225.AgProdChange_irr_ssp4 <- remove_AEZ_nonexist( L225.AgProdChange_irr_ssp4 )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( i in 1:max( L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt$scenID ) ){
	scenarios <- unique( L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt[ c( "scenID", rcp_gcm_cm ) ] )
	scen_strings <- paste( scenarios$rcp, scenarios$gcm, scenarios$cropmodel, sep = "_" )
	objectname <- paste0( "L225.AgProdChange_", scen_strings[i] )
	object <- subset( L225.ag_YieldRate_scen_R_C_Y_AEZ_irr.melt, scenID == i )[ names_AgProdChange ]
	object <- rename_biocrops( object, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgProductionTechnology",
              lookup_matchvar = "old_AgProductionTechnology", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
	object <- remove_AEZ_nonexist( object )
	assign( objectname, object )
	batchXMLstring <- paste0( "batch_ag_prodchange_",
	                  substr( objectname, regexpr( "AgProdChange_", objectname ) + 13, nchar( objectname ) ),
	                  "_IRR.xml" )
	write_mi_data( object, "AgProdChange", "AGLU_LEVEL2_DATA", objectname, "AGLU_XML_BATCH", batchXMLstring )
	XMLstring <- sub( "batch_", "", batchXMLstring )
	insert_file_into_batchxml( "AGLU_XML_BATCH", batchXMLstring, "AGLU_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}


write_mi_data( L225.AgProdChange_irr_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L225.AgProdChange_irr_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ref_IRR.xml" ) 
write_mi_data( L225.AgProdChange_irr_high, "AgProdChange", "AGLU_LEVEL2_DATA", "L225.AgProdChange_irr_high", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp1_IRR.xml" ) 
write_mi_data( L225.AgProdChange_irr_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L225.AgProdChange_irr_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp2_IRR.xml" ) 
write_mi_data( L225.AgProdChange_irr_low, "AgProdChange", "AGLU_LEVEL2_DATA", "L225.AgProdChange_irr_low", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp3_IRR.xml" ) 
write_mi_data( L225.AgProdChange_irr_ssp4, "AgProdChange", "AGLU_LEVEL2_DATA", "L225.AgProdChange_irr_ssp4", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp4_IRR.xml" ) 
write_mi_data( L225.AgProdChange_irr_high, "AgProdChange", "AGLU_LEVEL2_DATA", "L225.AgProdChange_irr_high", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp5_IRR.xml" ) 
write_mi_data( L225.AgCost_ag, "AgCost", "AGLU_LEVEL2_DATA", "L225.AgCost_ag", "AGLU_XML_BATCH", "batch_ag_cost_IRR.xml" ) 
write_mi_data( L225.AgCost_For, "AgCost", "AGLU_LEVEL2_DATA", "L225.AgCost_For", "AGLU_XML_BATCH", "batch_ag_cost_IRR.xml" ) 
write_mi_data( L225.AgCost_bio, "AgCost", "AGLU_LEVEL2_DATA", "L225.AgCost_bio", "AGLU_XML_BATCH", "batch_ag_cost_IRR.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ref_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ref_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp1_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp1_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp2_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp2_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp3_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp3_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp4_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp4_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp5_IRR.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp5_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_cost_IRR.xml", "AGLU_XML_FINAL", "ag_cost_IRR.xml", "", xml_tag="outFile" )

logstop()
