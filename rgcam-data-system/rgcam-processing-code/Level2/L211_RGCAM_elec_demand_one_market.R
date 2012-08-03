# L211_RGCAM_elec_demand_one_market.R_
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L211_RGCAM_elec_demand_one_market.R" )
printlog( "RGCAM electricity demand input file for a single electricity market for the entire country" )

# -----------------------------------------------------------------------------
# 1. Read files

A_elecD_Delete <- readdata( "A_elecD_Delete" )
A_elecD_misc <- readdata( "A_elecD_misc" )
A_elecD_subs_logit <- readdata( "A_elecD_subs_logit" )
A_elecD_tech_logit <- readdata( "A_elecD_tech_logit" )
A_elecD_subs_interp <- readdata( "A_elecD_subs_interp" )

A_elecD_tech_interp <- readdata( "A_elecD_tech_interp" )
A_elecD_tech_input <- readdata( "A_elecD_tech_input" )

L155_state_elec_supply <- readdata( "L155_state_elec_supply" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
L211_subregions <- unique( L155_state_elec_supply$GCAM_subregion )

printlog( "L211_ElecDelete: DELETE EXISTING US ELECT SECTOR" )
L211_ElecDelete <- A_elecD_Delete 

printlog( "L211_ElecSector: SECTOR LEVEL PARAMS" )
# The sector table will include units strings as well as the logit exponent by region
L211_ElecSector <- A_elecD_misc[ c( "supplysector", "input_unit", "output_unit", "price_unit" ) ]
L211_ElecSector[ "logit_expontent" ] <- A_elecD_subs_logit$subs_logit
L211_ElecSector <- L211_ElecSector[ rep( 1:nrow( L211_ElecSector ), times = length( L211_subregions ) ), ]
L211_ElecSector <- data.frame( region=L211_subregions, L211_ElecSector )

# Create a table for subsector nest share-weights and logits
printlog( "L211_ElecSubsector: SUBSECTOR LEVEL PARAMS" )
L211_ElecSubsector <- A_elecD_tech_logit 
L211_ElecSubsector <- L211_ElecSubsector[ sort( rep( 1:nrow( L211_ElecSubsector ), times = length( L211_subregions ) ) ), ]
L211_ElecSubsector <- data.frame( region=L211_subregions, L211_ElecSubsector )

# Create a table for subsector interpolation rules
printlog( "L211_ElecSubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L211_ElecSubsInterpRule <- data.frame( A_elecD_subs_interp, apply_to="share-weight" )
L211_ElecSubsInterpRule <- L211_ElecSubsInterpRule[ rep( 1:nrow( L211_ElecSubsInterpRule ), times = length( L211_subregions ) ), ]
L211_ElecSubsInterpRule <- data.frame( region=L211_subregions, L211_ElecSubsInterpRule )

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L211_GlobalDBTechInputDemand: TECH INPUT" )
# The technologies that pull from states will not be placed in the global tech db
L211_GlobalDBTechInputDemand <- A_elecD_tech_input[ A_elecD_tech_input$market == "__SR__", names( A_elecD_tech_input ) %!in% c( "market" ) ]
L211_GlobalDBTechInputDemand$efficiency <- round( L211_GlobalDBTechInputDemand$efficiency, CalInput_digits )
# Create stubs to put these techs into the subregions
L211_TechStubs <- unique( L211_GlobalDBTechInputDemand[ , c( "supplysector", "subsector", "technology" ) ] )
L211_TechStubs <- L211_TechStubs[ rep( 1:nrow( L211_TechStubs ), times = length( L211_subregions ) ), ]
L211_TechStubs <- data.frame( region=L211_subregions, L211_TechStubs )

# Table for calibration of subregional generation by state and load segment
printlog( "L211_SubregionalCal: TECH SUBREGIONAL CAL" )
L211_ElecSubregionalCal <- data.frame( L155_state_elec_supply[ rep( 1:nrow( L155_state_elec_supply ), times = length( GCAM_base_years ) ),
                            c( "state", "GCAM_subregion", "segment" ) ],
                            year = sort( rep( GCAM_base_years, times = nrow( L155_state_elec_supply ) ) ),
                            share_weight=1 )
L211_ElecSubregionalCal$calibrated_value[ L211_ElecSubregionalCal$year == GCAM_base_years[1] ] <- L155_state_elec_supply$gen1990
L211_ElecSubregionalCal$calibrated_value[ L211_ElecSubregionalCal$year == GCAM_base_years[2] ] <- L155_state_elec_supply$gen2005
L211_ElecSubregionalCal$calibrated_value <- round( L211_ElecSubregionalCal$calibrated_value, CalInput_digits )
L211_ElecSubregionalCal <- aggregate( calibrated_value ~ state + GCAM_subregion + segment + year, data=L211_ElecSubregionalCal, FUN=sum )
L211_ElecSubregionalCal$subsector <- L211_ElecSubregionalCal$segment
L211_ElecSubregionalCal$technology <- paste( L211_ElecSubregionalCal$segment, "_", L211_ElecSubregionalCal$state, sep="" )
L211_ElecSubregionalCal$share_weight <- ifelse( L211_ElecSubregionalCal$calibrated_value > 0, 1, 0 )
L211_ElecSubregionalCal$minicam_energy_input <- L211_ElecSubregionalCal$segment
L211_ElecSubregionalCal$efficiency <- 1
# Rename and reorder columns for consistency
names( L211_ElecSubregionalCal )[ 1 ] <- "market"
names( L211_ElecSubregionalCal )[ 2 ] <- "region"
names( L211_ElecSubregionalCal )[ 3 ] <- "supplysector"
L211_ElecSubregionalCal <- L211_ElecSubregionalCal[ , c( 2, 3, 6, 7, 4, 8, 9, 5, 10, 1 ) ]
# Need to put something in for 75 to avoid errors
L211_ElecSubregionalCal.75 <- L211_ElecSubregionalCal[ L211_ElecSubregionalCal$year == GCAM_base_years[1], ]
L211_ElecSubregionalCal.75$year <- GCAM_model_period0
L211_ElecSubregionalCal.75$calibrated_value <- -1
L211_ElecSubregionalCal <- rbind( L211_ElecSubregionalCal, L211_ElecSubregionalCal.75 )

# Need interpolation rules for technologies
printlog( "L211_TechInterpRule: TECH INTERP RULES" )
L211_TechInterpRule <- L211_ElecSubregionalCal[ L211_ElecSubregionalCal$year == GCAM_model_period0, 1:4 ]
L211_TechInterpRule <- data.frame( L211_TechInterpRule, A_elecD_tech_interp[
            match( L211_TechInterpRule$supplysector, A_elecD_tech_interp$supplysector ),
            c( "from_year", "to_year", "interpolation_function" ) ], apply_to = "share-weight" )

# We now need an subregional aggregating sector in USA
# To do this we will add to existing tables a row for USA
printlog( "L211_USATemp: USA AGG INPUT TABLE ADJUSTMENTS" )
L211_USATemp <- L211_ElecSector[ L211_ElecSector$region == L211_subregions[1] & L211_ElecSector$supplysector == "electricity", ]
L211_USATemp$region <- "USA"
L211_ElecSector <- rbind( L211_ElecSector, L211_USATemp )

L211_USATemp <- L211_ElecSubsector[ L211_ElecSubsector$region == L211_subregions[1] & L211_ElecSubsector$supplysector == "electricity", ]
L211_USATemp$region <- "USA"
L211_ElecSubsector <- rbind( L211_ElecSubsector, L211_USATemp )

L211_USATemp <- L211_ElecSubsInterpRule[ L211_ElecSubsInterpRule$region == L211_subregions[1] & L211_ElecSubsInterpRule$supplysector == "electricity", ]
L211_USATemp$region <- "USA"
L211_ElecSubsInterpRule<- rbind( L211_ElecSubsInterpRule, L211_USATemp )

L211_USATemp <- aggregate( cbind( gen1990, gen2005 ) ~ GCAM_subregion, data=L155_state_elec_supply, FUN=sum )
L211_USATemp$gen1975 <- -1
L211_USATemp <- melt( L211_USATemp, id=1 )
L211_USATemp$variable <- substring( L211_USATemp$variable, 4 )
L211_USATemp$value <- round( L211_USATemp$value, CalInput_digits )
L211_USATemp <- data.frame( region="USA", supplysector="electricity", subsector="electricity", technology=
    paste( "electricity_", L211_USATemp$GCAM_subregion, sep="" ), year=L211_USATemp$variable, share_weight=1,
    minicam_energy_input="electricity", calibrated_value=L211_USATemp$value, efficiency=1, market=L211_USATemp$GCAM_subregion )
L211_ElecSubregionalCal <- rbind( L211_ElecSubregionalCal, L211_USATemp )

L211_USATemp <- L211_ElecSubregionalCal[ L211_ElecSubregionalCal$year == GCAM_model_period0 & L211_ElecSubregionalCal$region == "USA", 1:4 ]
L211_USATemp <- data.frame( L211_USATemp, A_elecD_tech_interp[
            match( L211_USATemp$supplysector, A_elecD_tech_interp$supplysector ),
            c( "from_year", "to_year", "interpolation_function" ) ], apply_to = "share-weight" )
L211_TechInterpRule <- rbind( L211_TechInterpRule, L211_USATemp )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L211_ElecDelete, "SectorDelete", "L211_ElecDelete", "batch_rgcam_elec_demand_single_market.xml" ) 
write_mi_data( L211_ElecSector, "Sector", "L211_ElecSector", "batch_rgcam_elec_demand_single_market.xml" ) 
write_mi_data( L211_ElecSubsector, "ElecSubsector_Logit", "L211_ElecSubsector", "batch_rgcam_elec_demand_single_market.xml" ) 
write_mi_data( L211_ElecSubsInterpRule, "ElecSubsInterpRule", "L211_ElecSubsInterpRule", "batch_rgcam_elec_demand_single_market.xml" ) 
write_mi_data( L211_GlobalDBTechInputDemand, "GlobalDBTechInputDemand", "L211_GlobalDBTechInputDemand", "batch_rgcam_elec_demand_single_market.xml" ) 
write_mi_data( L211_ElecSubregionalCal, "ElecSubgregionalCal", "L211_ElecSubregionalCal", "batch_rgcam_elec_demand_single_market.xml" ) 
write_mi_data( L211_TechStubs, "TechStubs", "L211_TechStubs", "batch_rgcam_elec_demand_single_market.xml" ) 
write_mi_data( L211_TechInterpRule, "TechInterpRule", "L211_TechInterpRule", "batch_rgcam_elec_demand_single_market.xml" ) 


insert_file_into_batchxml( "batch_rgcam_elec_demand_single_market.xml", "rgcam_elec_demand_single_market.xml", "", xml_tag="outFile" )

logstop()
