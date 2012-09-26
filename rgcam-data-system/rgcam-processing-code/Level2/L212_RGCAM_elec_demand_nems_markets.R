# L212_RGCAM_elec_demand_nems_markets.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L212_RGCAM_elec_demand_nems_markets.R" )
printlog( "RGCAM electricity demand input file for electricity markets organized by the NEMS regions" )

# -----------------------------------------------------------------------------
# 1. Read files

A_elecD_Delete <- readdata( "A_elecD_Delete" )
A_elecD_misc <- readdata( "A_elecD_misc" )
A_elecD_subs_logit <- readdata( "A_elecD_subs_logit" )
A_elecD_tech_logit <- readdata( "A_elecD_tech_logit" )
A_elecD_subs_interp <- readdata( "A_elecD_subs_interp" )

A_elecD_tech_interp <- readdata( "A_elecD_tech_interp" )
A_elecD_tech_input <- readdata( "A_elecD_tech_input" )

A_elecNEMS_tech_input <- readdata( "A_elecNEMS_tech_input" )

L155_state_elec_supply <- readdata( "L155_state_elec_supply" )
L103_out_EJ_state_ownuse_elec <- readdata( "L103_out_EJ_state_ownuse_elec" )
L103_in_EJ_state_ownuse_elec <- readdata( "L103_in_EJ_state_ownuse_elec" )
L106_out_EJ_state_td_elec <- readdata( "L106_out_EJ_state_td_elec" )
L106_in_EJ_state_td_elec <- readdata( "L106_in_EJ_state_td_elec" )
states_subregions <- readdata( "states_subregions" )

A_trn_tech_input <- readdata( "A_trn_tech_input" )
A_ind_tech_input <- readdata( "A_ind_tech_input" )
A_ind_tech_sec_out <- readdata( "A_ind_tech_sec_out" )
A_refliq_tech_2nd <- readdata( "A_refliq_tech_2nd" )
A_elecS_tech_backup <- readdata( "A_elecS_tech_backup" )
A_res_tech_backup <- readdata( "A_res_tech_backup" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
L212_subregions <- unique( L155_state_elec_supply$GCAM_subregion )

L212_sR_out_ownuse <- merge( states_subregions[, c( "state", "subregion_FERC" ) ], L103_out_EJ_state_ownuse_elec )
names( L212_sR_out_ownuse )[ names( L212_sR_out_ownuse ) %in% c( "subregion_FERC" ) ] <- "region"
L212_sR_out_ownuse <- aggregate( cbind( X1990, X2005 ) ~ region, data=L212_sR_out_ownuse, FUN=sum )
L212_sR_out_ownuse <- melt( L212_sR_out_ownuse, id="region", variable_name="year" )
L212_sR_out_ownuse$year <- as.integer( substring( L212_sR_out_ownuse$year, 2 ) )
names( L212_sR_out_ownuse )[ 3 ] <- "production"

L212_sR_in_ownuse <- merge( states_subregions[, c( "state", "subregion_FERC" ) ], L103_in_EJ_state_ownuse_elec )
names( L212_sR_in_ownuse )[ names( L212_sR_in_ownuse ) %in% c( "subregion_FERC" ) ] <- "region"
L212_sR_in_ownuse <- aggregate( cbind( X1990, X2005 ) ~ region, data=L212_sR_in_ownuse, FUN=sum )
L212_sR_in_ownuse <- melt( L212_sR_in_ownuse, id="region", variable_name="year" )
L212_sR_in_ownuse$year <- as.integer( substring( L212_sR_in_ownuse$year, 2 ) )
names( L212_sR_in_ownuse )[ 3 ] <- "input"

L212_sR_eff_ownuse <- merge( L212_sR_out_ownuse, L212_sR_in_ownuse )
L212_sR_eff_ownuse$efficiency <- L212_sR_eff_ownuse$production / L212_sR_eff_ownuse$input

L212_sR_out_td <- merge( states_subregions[, c( "state", "subregion_FERC" ) ], L106_out_EJ_state_td_elec )
names( L212_sR_out_td )[ names( L212_sR_out_td ) %in% c( "subregion_FERC" ) ] <- "region"
L212_sR_out_td <- aggregate( cbind( X1990, X2005 ) ~ region, data=L212_sR_out_td, FUN=sum )
L212_sR_out_td <- melt( L212_sR_out_td, id="region", variable_name="year" )
L212_sR_out_td$year <- as.integer( substring( L212_sR_out_td$year, 2 ) )
names( L212_sR_out_td )[ 3 ] <- "production"

L212_sR_eff_td <- merge( L212_sR_out_td, L212_sR_in_td )
L212_sR_eff_td$efficiency <- L212_sR_eff_td$production / L212_sR_eff_td$consumption

L212_sR_in_td <- merge( states_subregions[, c( "state", "subregion_FERC" ) ], L106_in_EJ_state_td_elec )
names( L212_sR_in_td )[ names( L212_sR_in_td ) %in% c( "subregion_FERC" ) ] <- "region"
L212_sR_in_td <- aggregate( cbind( X1990, X2005 ) ~ region, data=L212_sR_in_td, FUN=sum )
L212_sR_in_td <- melt( L212_sR_in_td, id="region", variable_name="year" )
L212_sR_in_td$year <- as.integer( substring( L212_sR_in_td$year, 2 ) )
names( L212_sR_in_td )[ 3 ] <- "consumption"

L212_sR_trade <- merge( L212_sR_out_ownuse, L212_sR_in_td )
L212_sR_trade$net_import <- L212_sR_trade$consumption - L212_sR_trade$production

printlog( "L212_ElecDelete: DELETE EXISTING US ELECT SECTOR" )
L212_ElecDelete <- A_elecD_Delete 
L212_ElecDelete <- rbind( L212_ElecDelete, data.frame( region="USA", supplysector=c(
    "electricity_net_ownuse", "elect_td_ind", "elect_td_trn", "elect_td_bld" ) ) )

printlog( "L212_ElecSector: SECTOR LEVEL PARAMS" )
# The sector table will include units strings as well as the logit exponent by region
L212_ElecSector <- A_elecD_misc[ c( "supplysector", "input_unit", "output_unit", "price_unit" ) ]
L212_ElecSector[ "logit_expontent" ] <- A_elecD_subs_logit$subs_logit
L212_ElecSector <- L212_ElecSector[ rep( 1:nrow( L212_ElecSector ), times = length( L212_subregions ) ), ]
L212_ElecSector <- data.frame( region=L212_subregions, L212_ElecSector )

# Create a table for subsector nest share-weights and logits
printlog( "L212_ElecSubsector: SUBSECTOR LEVEL PARAMS" )
L212_ElecSubsector <- A_elecD_tech_logit 
L212_ElecSubsector <- L212_ElecSubsector[ sort( rep( 1:nrow( L212_ElecSubsector ), times = length( L212_subregions ) ) ), ]
L212_ElecSubsector <- data.frame( region=L212_subregions, L212_ElecSubsector )

# Create a table for subsector interpolation rules
printlog( "L212_ElecSubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L212_ElecSubsInterpRule <- data.frame( A_elecD_subs_interp, apply_to="share-weight" )
L212_ElecSubsInterpRule <- L212_ElecSubsInterpRule[ rep( 1:nrow( L212_ElecSubsInterpRule ), times = length( L212_subregions ) ), ]
L212_ElecSubsInterpRule <- data.frame( region=L212_subregions, L212_ElecSubsInterpRule )

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L212_GlobalDBTechInputDemand: TECH INPUT" )
# The technologies that pull from states will not be placed in the global tech db
L212_GlobalDBTechInputDemand <- A_elecD_tech_input[ A_elecD_tech_input$market == "__SR__", names( A_elecD_tech_input ) %!in% c( "market" ) ]
L212_GlobalDBTechInputDemand$efficiency <- round( L212_GlobalDBTechInputDemand$efficiency, CalInput_digits )
# Create stubs to put these techs into the subregions
L212_TechStubs <- unique( L212_GlobalDBTechInputDemand[ , c( "supplysector", "subsector", "technology" ) ] )
L212_TechStubs <- L212_TechStubs[ rep( 1:nrow( L212_TechStubs ), times = length( L212_subregions ) ), ]
L212_TechStubs <- data.frame( region=L212_subregions, L212_TechStubs )

# Table for calibration of subregional generation by state and load segment
printlog( "L212_SubregionalCal: TECH SUBREGIONAL CAL" )
L212_ElecSubregionalCal <- data.frame( L155_state_elec_supply[ rep( 1:nrow( L155_state_elec_supply ), times = length( GCAM_base_years ) ),
                            c( "state", "GCAM_subregion", "segment" ) ],
                            year = sort( rep( GCAM_base_years, times = nrow( L155_state_elec_supply ) ) ),
                            share_weight=1 )
L212_ElecSubregionalCal$calibrated_value[ L212_ElecSubregionalCal$year == GCAM_base_years[1] ] <- L155_state_elec_supply$gen1990
L212_ElecSubregionalCal$calibrated_value[ L212_ElecSubregionalCal$year == GCAM_base_years[2] ] <- L155_state_elec_supply$gen2005
L212_ElecSubregionalCal$calibrated_value <- round( L212_ElecSubregionalCal$calibrated_value, CalInput_digits )
L212_ElecSubregionalCal <- aggregate( calibrated_value ~ state + GCAM_subregion + segment + year, data=L212_ElecSubregionalCal, FUN=sum )
L212_ElecSubregionalCal$subsector <- L212_ElecSubregionalCal$segment
L212_ElecSubregionalCal$technology <- paste( L212_ElecSubregionalCal$segment, "_", L212_ElecSubregionalCal$state, sep="" )
L212_ElecSubregionalCal$share_weight <- ifelse( L212_ElecSubregionalCal$calibrated_value > 0, 1, 0 )
L212_ElecSubregionalCal$minicam_energy_input <- L212_ElecSubregionalCal$segment
L212_ElecSubregionalCal$efficiency <- 1
# Rename and reorder columns for consistency
names( L212_ElecSubregionalCal )[ 1 ] <- "market"
names( L212_ElecSubregionalCal )[ 2 ] <- "region"
names( L212_ElecSubregionalCal )[ 3 ] <- "supplysector"
L212_ElecSubregionalCal <- L212_ElecSubregionalCal[ , c( 2, 3, 6, 7, 4, 8, 9, 5, 10, 1 ) ]
# Need to put something in for 75 to avoid errors
L212_ElecSubregionalCal.75 <- L212_ElecSubregionalCal[ L212_ElecSubregionalCal$year == GCAM_base_years[1], ]
L212_ElecSubregionalCal.75$year <- GCAM_model_period0
L212_ElecSubregionalCal.75$calibrated_value <- -1
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ElecSubregionalCal.75 )

# Need interpolation rules for technologies
printlog( "L212_TechInterpRule: TECH INTERP RULES" )
L212_TechInterpRule <- L212_ElecSubregionalCal[ L212_ElecSubregionalCal$year == GCAM_model_period0, 1:4 ]
L212_TechInterpRule <- data.frame( L212_TechInterpRule, A_elecD_tech_interp[
            match( L212_TechInterpRule$supplysector, A_elecD_tech_interp$supplysector ),
            c( "from_year", "to_year", "interpolation_function" ) ], apply_to = "share-weight" )

# We now need an subregional aggregating sector in USA
# To do this we will add to existing tables a row for USA
printlog( "L212_USATemp: USA AGG INPUT TABLE ADJUSTMENTS" )

# Create Sector data for net ownuse before and after trade
L212_ImpTemp <- L212_ElecSector[ L212_ElecSector$supplysector == "electricity", ]
L212_ImpTemp$supplysector <- "electricity_net_ownuse_before_trade"
L212_ElecSector <- rbind( L212_ElecSector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "electricity_net_ownuse"
L212_ElecSector <- rbind( L212_ElecSector, L212_ImpTemp )

# Create Sector data for inter market trade
L212_ImpTemp <- L212_ElecSector[ L212_ElecSector$supplysector == "electricity" & L212_ElecSector$region == L212_subregions[1], ]
L212_ImpTemp$region <- "USA"
L212_ImpTemp$supplysector <- "elect_inter_market_trade"
L212_ElecSector <- rbind( L212_ElecSector, L212_ImpTemp )

# Create Subsector info for net ownuse before and after trade
L212_ImpTemp <- L212_ElecSubsector[ L212_ElecSubsector$supplysector == "electricity", ]
L212_ImpTemp$supplysector <- "electricity_net_ownuse_before_trade"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "electricity_net_ownuse"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )
L212_ImpTemp$subsector <- paste( L212_ImpTemp$subsector, "imports" )
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )

# Create Subsector info for inter market trade
L212_ImpTemp <- L212_ElecSubsector[ L212_ElecSubsector$supplysector == "electricity" & L212_ElecSubsector$region == L212_subregions[1], ]
L212_ImpTemp$region <- "USA"
L212_ImpTemp$supplysector <- "elect_inter_market_trade"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )

# Create Subsector interp rules for net ownuse before and after trade
L212_ImpTemp <- L212_ElecSubsInterpRule[ L212_ElecSubsInterpRule$supplysector == "electricity", ]
L212_ImpTemp$supplysector <- "electricity_net_ownuse_before_trade"
L212_ElecSubsInterpRule<- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )
L212_ImpTemp$supplysector <- "electricity_net_ownuse"
L212_ElecSubsInterpRule<- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )
L212_ImpTemp$subsector <- paste( L212_ImpTemp$subsector, "imports" )
L212_ElecSubsInterpRule<- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )

# Create Subsector interp rules for inter market trade
L212_ImpTemp <- L212_ElecSubsInterpRule[ L212_ElecSubsInterpRule$supplysector == "electricity" & L212_ElecSubsInterpRule$region == L212_subregions[1], ]
L212_ImpTemp$region <- "USA"
L212_ImpTemp$supplysector <- "elect_inter_market_trade"
L212_ElecSubsInterpRule<- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )

# Create technology for net own use before trade which is where the net ownuse losses occur
L212_ImpTemp <- data.frame( region=L212_sR_eff_ownuse$region, supplysector="electricity_net_ownuse_before_trade",
    subsector="electricity", technology="electricity", year=L212_sR_eff_ownuse$year, share_weight=1,
    minicam_energy_input="electricity", calibrated_value=L212_sR_eff_ownuse$input, efficiency=L212_sR_eff_ownuse$efficiency,
    market=L212_sR_eff_ownuse$region )
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp <- L212_ImpTemp[ L212_ImpTemp$year == GCAM_base_years[1], ]
L212_ImpTemp$year <- GCAM_model_period0
L212_ImpTemp$calibrated_value <- -1
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp <- data.frame( L212_ImpTemp[, names( L212_ImpTemp ) %in% c( "region", "supplysector", "subsector", "technology" ) ],
    from_year="2005", to_year="9999", interpolation_function="fixed", apply_to="share-weight" )
L212_TechInterpRule <- rbind( L212_TechInterpRule, L212_ImpTemp )

# Create technology for net ownuse domestic consumption
L212_ImpTemp <- data.frame( region=L212_sR_trade$region, supplysector="electricity_net_ownuse",
    subsector="electricity", technology="electricity", year=L212_sR_trade$year, share_weight=1,
    minicam_energy_input="electricity_net_ownuse_before_trade", calibrated_value=ifelse(L212_sR_trade$net_import > 0, L212_sR_trade$production,
    L212_sR_trade$consumption ), efficiency=1, market=L212_sR_trade$region )
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp <- L212_ImpTemp[ L212_ImpTemp$year == GCAM_base_years[1], ]
L212_ImpTemp$year <- GCAM_model_period0
L212_ImpTemp$calibrated_value <- -1
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp <- data.frame( L212_ImpTemp[, names( L212_ImpTemp ) %in% c( "region", "supplysector", "subsector", "technology" ) ],
    from_year="2005", to_year="9999", interpolation_function="fixed", apply_to="share-weight" )
L212_TechInterpRule <- rbind( L212_TechInterpRule, L212_ImpTemp )

# Create technology for net ownuse imports consumption
L212_ImpTemp <- data.frame( region=L212_sR_trade$region, supplysector="electricity_net_ownuse",
    subsector="electricity imports", technology="electricity imports", year=L212_sR_trade$year, share_weight=1,
    minicam_energy_input="elect_inter_market_trade", calibrated_value=L212_sR_trade$net_import,
    efficiency=1, market="USA" )
L212_ImpTemp$share_weight <- ifelse( L212_ImpTemp$calibrated_value > 0, 1, 0 )
L212_ImpTemp$calibrated_value <- ifelse( L212_ImpTemp$calibrated_value > 0, L212_ImpTemp$calibrated_value, -1 )
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp <- L212_ImpTemp[ L212_ImpTemp$year == GCAM_base_years[1], ]
L212_ImpTemp$year <- GCAM_model_period0
L212_ImpTemp$calibrated_value <- -1
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp <- data.frame( L212_ImpTemp[, names( L212_ImpTemp ) %in% c( "region", "supplysector", "subsector", "technology" ) ],
    from_year="2005", to_year="9999", interpolation_function="fixed", apply_to="share-weight" )
L212_TechInterpRule <- rbind( L212_TechInterpRule, L212_ImpTemp )

# Create technologies for pulling traded electricity from subregional markets
L212_ImpTemp <- data.frame( region="USA", supplysector="elect_inter_market_trade",
    subsector="electricity", technology=paste( "electricity", L212_sR_trade$region ), year=L212_sR_trade$year, share_weight=1,
    minicam_energy_input="electricity_net_ownuse_before_trade", calibrated_value=L212_sR_trade$net_import * -1,
    efficiency=1, market=L212_sR_trade$region )
L212_ImpTemp$share_weight <- ifelse( L212_ImpTemp$calibrated_value > 0, 1, 0 )
L212_ImpTemp$calibrated_value <- ifelse( L212_ImpTemp$calibrated_value > 0, L212_ImpTemp$calibrated_value, -1 )
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp <- L212_ImpTemp[ L212_ImpTemp$year == GCAM_base_years[1], ]
L212_ImpTemp$year <- GCAM_model_period0
L212_ImpTemp$calibrated_value <- -1
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp <- data.frame( L212_ImpTemp[, names( L212_ImpTemp ) %in% c( "region", "supplysector", "subsector", "technology" ) ],
    from_year="2005", to_year="9999", interpolation_function="fixed", apply_to="share-weight" )
L212_TechInterpRule <- rbind( L212_TechInterpRule, L212_ImpTemp )

# TD sectors for subregions
L212_ImpTemp <- L212_ElecSector[ L212_ElecSector$supplysector == "electricity" & L212_ElecSector$region == L212_subregions[1], ]
L212_ImpTemp <- L212_ImpTemp[ rep( 1, times=length( L212_subregions ) ), ]
L212_ImpTemp$region <- L212_subregions
L212_ImpTemp$supplysector <- "elect_td_ind"
L212_ElecSector <- rbind( L212_ElecSector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_trn"
L212_ElecSector <- rbind( L212_ElecSector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_bld"
L212_ElecSector <- rbind( L212_ElecSector, L212_ImpTemp )

# TD sectors for states (note bld is handled in L241_RGCAM_rooftop_PV.R
L212_ImpTemp <- L212_ElecSector[ L212_ElecSector$supplysector == "electricity" & L212_ElecSector$region == L212_subregions[1], ]
L212_ImpTemp <- L212_ImpTemp[ rep( 1, times=length( states ) ), ]
L212_ImpTemp$region <- states
L212_ImpTemp$supplysector <- "elect_td_ind"
L212_ElecSector <- rbind( L212_ElecSector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_trn"
L212_ElecSector <- rbind( L212_ElecSector, L212_ImpTemp )

# TD subsectors for subregions
L212_ImpTemp <- L212_ElecSubsector[ L212_ElecSubsector$supplysector == "electricity" & L212_ElecSubsector$region == L212_subregions[1], ]
L212_ImpTemp <- L212_ImpTemp[ rep( 1, times=length( L212_subregions ) ), ]
L212_ImpTemp$region <- L212_subregions 
L212_ImpTemp$supplysector <- "elect_td_ind"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_trn"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_bld"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )

# TD subsectors for states 
L212_ImpTemp <- L212_ElecSubsector[ L212_ElecSubsector$supplysector == "electricity" & L212_ElecSubsector$region == L212_subregions[1], ]
L212_ImpTemp <- L212_ImpTemp[ rep( 1, times=length( states ) ), ]
L212_ImpTemp$region <- states
L212_ImpTemp$supplysector <- "elect_td_ind"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_trn"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_bld"
L212_ElecSubsector <- rbind( L212_ElecSubsector, L212_ImpTemp )

# TD subsector interp rules for subregions
L212_ImpTemp <- L212_ElecSubsInterpRule[ L212_ElecSubsInterpRule$supplysector == "electricity" & L212_ElecSubsInterpRule$region == L212_subregions[1], ]
L212_ImpTemp <- L212_ImpTemp[ rep( 1, times=length( L212_subregions ) ), ]
L212_ImpTemp$region <- L212_subregions
L212_ImpTemp$supplysector <- "elect_td_ind"
L212_ElecSubsInterpRule <- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_trn"
L212_ElecSubsInterpRule <- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_bld"
L212_ElecSubsInterpRule <- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )

# TD subsector interp rules for states 
L212_ImpTemp <- L212_ElecSubsInterpRule[ L212_ElecSubsInterpRule$supplysector == "electricity" & L212_ElecSubsInterpRule$region == L212_subregions[1], ]
L212_ImpTemp <- L212_ImpTemp[ rep( 1, times=length( states ) ), ]
L212_ImpTemp$region <- states
L212_ImpTemp$supplysector <- "elect_td_ind"
L212_ElecSubsInterpRule <- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_trn"
L212_ElecSubsInterpRule <- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_bld"
L212_ElecSubsInterpRule <- rbind( L212_ElecSubsInterpRule, L212_ImpTemp )

# TD technology in the subregions note this is where TD losses occur
L212_ImpTemp <- data.frame( region=L212_sR_eff_td$region, supplysector="elect_td_ind",
    subsector="electricity", technology="electricity", year=L212_sR_eff_td$year, share_weight=1,
    minicam_energy_input="electricity_net_ownuse", calibrated_value=-1, coefficient=1/L212_sR_eff_td$efficiency,
    market=L212_sR_eff_td$region )
L212_ElecTDTechInput <- L212_ImpTemp
L212_ImpTemp$supplysector <- "elect_td_trn"
L212_ElecTDTechInput <- rbind( L212_ElecTDTechInput, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_bld"
L212_ElecTDTechInput <- rbind( L212_ElecTDTechInput, L212_ImpTemp )
L212_additional_years <- c( GCAM_model_period0, GCAM_future_years )
L212_ImpTemp <- L212_ElecTDTechInput[ L212_ElecTDTechInput$year == final_cal_year, ]
L212_ImpTemp <- L212_ImpTemp[ rep( 1:nrow( L212_ImpTemp ), times=length( L212_additional_years ) ), ]
L212_ImpTemp$year <- sort( rep( L212_additional_years, time=nrow( L212_ElecTDTechInput[ L212_ElecTDTechInput$year == final_cal_year, ] ) ) )
L212_ImpTemp$calibrated_value <- -1
L212_ImpTemp$coefficient <- L212_ImpTemp$coefficient * ( 1- 0.0001 ) ^ ( L212_ImpTemp$year - final_cal_year )
L212_ElecTDTechInput <- rbind( L212_ElecTDTechInput, L212_ImpTemp )
L212_ElecTDTechInput <- merge( L212_ElecTDTechInput, A_elecNEMS_tech_input )
L212_ElecTDTechInput <- L212_ElecTDTechInput[, c( "region", "supplysector", "subsector", "technology", "minicam_energy_input",
    "year", "share_weight", "calibrated_value", "coefficient", "non_energy_cost", "market" ) ]

# TD technology in the states that is simply a pass through from the subregional TD sector
L212_ImpTemp <- data.frame( region=states, supplysector="elect_td_ind", subsector="electricity", technology="electricity",
    year=GCAM_model_period0, share_weight=1, minicam_energy_input="elect_td_ind", calibrated_value=-1, efficiency=1,
    market=states_subregions[ states_subregions$subregion_FERC != "na", "subregion_FERC" ] )
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_trn"
L212_ImpTemp$minicam_energy_input <- "elect_td_trn"
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )
L212_ImpTemp$supplysector <- "elect_td_bld"
L212_ImpTemp$minicam_energy_input <- "elect_td_bld"
L212_ElecSubregionalCal <- rbind( L212_ElecSubregionalCal, L212_ImpTemp )

# Get rid of the old elect_td_bld pass through from L241_RGCAM_rooftop_PV.R
printlog( "L212_DeleteSubsector: DELETE SUBSECTOR" )
L212_DeleteSubsector <- data.frame( region=states, supplysector="elect_td_bld", subsector="elect_td_bld" )

# Relink trn and ind elec demands to just use the td in it's own region (note bld is take care in L241_RGCAM_rooftop_PV.R)
printlog( "L212_ChangeInputMarket: CHANGE INPUT MARKET" )
L212_ChangeInputMarket <- A_trn_tech_input[ A_trn_tech_input$minicam_energy_input == "elect_td_trn", c(
    "supplysector", "subsector", "technology", "year", "minicam_energy_input" ) ]
L212_ChangeInputMarket <- rbind( L212_ChangeInputMarket, A_ind_tech_input[ A_ind_tech_input$minicam_energy_input == "elect_td_ind", c(
    "supplysector", "subsector", "technology", "year", "minicam_energy_input" ) ] )
L212_ChangeInputMarket <- rbind( L212_ChangeInputMarket, A_refliq_tech_2nd[ A_refliq_tech_2nd$minicam_energy_input == "elect_td_ind", c(
    "supplysector", "subsector", "technology", "year", "minicam_energy_input" ) ] )
L212_ChangeInputMarket$market <- ""

printlog( "L212_ChangeSecOutputMarket: CHANGE OUTPUT MARKET" )
L212_ChangeSecOutputMarket <- A_ind_tech_sec_out[ A_ind_tech_sec_out$secondary_output == "electricity", c(
    "supplysector", "subsector", "technology", "year", "secondary_output" ) ]
L212_ChangeSecOutputMarket <- data.frame( region=states, L212_ChangeSecOutputMarket[ sort( rep( 1:nrow( L212_ChangeSecOutputMarket ),
    times=length( states ) ) ), ] )
L212_ChangeSecOutputMarket$market <- states_subregions[ match( L212_ChangeSecOutputMarket$region, states_subregions$state ),
    "subregion_FERC" ]

printlog( "L212_ChangeIntermitElecMarket: CHANGE INTERMIT ELECT MARKET" )
L212_ChangeIntermitElecMarket <- rbind( A_elecS_tech_backup[, c( "supplysector", "subsector", "technology", "period" ) ],
    A_res_tech_backup[, c( "supplysector", "subsector", "technology", "period" ) ] )
L212_ChangeIntermitElecMarket <- data.frame( region=states, L212_ChangeIntermitElecMarket[ sort( rep( 1:nrow( L212_ChangeIntermitElecMarket ),
    times=length( states ) ) ), ] )
L212_ChangeIntermitElecMarket$electricity_sector_market <- states_subregions[ match( L212_ChangeIntermitElecMarket$region, states_subregions$state ),
    "subregion_FERC" ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L212_ElecDelete, "SectorDelete", "L212_ElecDelete", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_DeleteSubsector, "SubsectorDelete", "L212_DeleteSubsector", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_ElecSector, "Sector", "L212_ElecSector", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_ElecSubsector, "ElecSubsector_Logit", "L212_ElecSubsector", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_ElecSubsInterpRule, "ElecSubsInterpRule", "L212_ElecSubsInterpRule", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_GlobalDBTechInputDemand, "GlobalDBTechInputDemand", "L212_GlobalDBTechInputDemand", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_ElecSubregionalCal, "ElecSubgregionalCal", "L212_ElecSubregionalCal", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_TechStubs, "TechStubs", "L212_TechStubs", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_TechInterpRule, "TechInterpRule", "L212_TechInterpRule", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_ElecTDTechInput, "RefLiqTechInput", "L212_ElecTDTechInput", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_ChangeInputMarket, "ChangeInputMarketGlobalDB", "L212_ChangeInputMarket", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_ChangeSecOutputMarket, "ChangeSecOutputMarket", "L212_ChangeSecOutputMarket", "batch_rgcam_elec_demand_nems_market.xml" ) 
write_mi_data( L212_ChangeIntermitElecMarket, "ChangeIntermitElecMarket", "L212_ChangeIntermitElecMarket", "batch_rgcam_elec_demand_nems_market.xml" ) 


insert_file_into_batchxml( "batch_rgcam_elec_demand_nems_market.xml", "rgcam_elec_demand_nems_market.xml", "", xml_tag="outFile" )

logstop()
