# L230_RGCAM_ind_input.R
# Generates state level aggregate industry input file
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L230_RGCAM_ind_input.R" )
printlog( "RGCAM aggregate industry input file" )

# -----------------------------------------------------------------------------
# 1. Read files

A_ind_fuel_map <- readdata( "A_ind_fuel_map" )
A_ind_Delete <- readdata( "A_ind_Delete" )
A_ind_sector <- readdata( "A_ind_sector" )
A_ind_subs <- readdata( "A_ind_subs" )
A_ind_subs_interp <- readdata( "A_ind_subs_interp" )

A_ind_tech_interp <- readdata( "A_ind_tech_interp" )
A_ind_tech_input <- readdata( "A_ind_tech_input" )
A_ind_tech_sec_out <- readdata( "A_ind_tech_sec_out" )
A_ind_tech_seq <- readdata( "A_ind_tech_seq" )

A_ind_finaldemand <- readdata( "A_ind_finaldemand" )

L102_in_EJ_state_indnochp_F <- readdata( "L102_in_EJ_state_indnochp_F" )
L102_in_EJ_state_indfeed_F <- readdata( "L102_in_EJ_state_indfeed_F" )
L102_in_EJ_state_indchp_F <- readdata( "L102_in_EJ_state_indchp_F" )
L102_out_EJ_state_indchp_F <- readdata( "L102_out_EJ_state_indchp_F" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
L230_subregions <- unique( L102_in_EJ_state_indnochp_F$state )

# We will want to all the subsector by the name used during processing however we will switch
# the fuel names to the actual GCAM fuel name
# TOOD: look up names instead?
L230_fuel_cal <- rbind( L102_in_EJ_state_indnochp_F, L102_in_EJ_state_indfeed_F, L102_in_EJ_state_indchp_F )
L230_fuel_cal <- data.frame( L102_in_EJ_state_indnochp_F, technology=L102_in_EJ_state_indnochp_F$GCAM_fuel )
L230_fuel_cal <- rbind( L230_fuel_cal, data.frame( L102_in_EJ_state_indfeed_F,
    technology=L102_in_EJ_state_indfeed_F$GCAM_fuel ) )
L230_fuel_cal <- rbind( L230_fuel_cal, data.frame( L102_in_EJ_state_indchp_F,
    technology=paste( L102_in_EJ_state_indchp_F$GCAM_fuel, "cogen" ) ) )
L230_fuel_cal$subsector <- L230_fuel_cal$GCAM_fuel
L230_fuel_cal <- L230_fuel_cal[ , c( "state", "GCAM_sector", "subsector", "technology",
    "GCAM_fuel", X_GCAM_base_years ) ]
L230_fuel_cal[, X_GCAM_base_years ] <- round( L230_fuel_cal[, X_GCAM_base_years ], CalInput_digits )
L230_fuel_cal <- L230_fuel_cal[ L230_fuel_cal$X1990 > 0 | L230_fuel_cal$X2005 > 0, ]
L230_fuel_cal$GCAM_fuel <- A_ind_fuel_map$GCAM_name[ match( L230_fuel_cal$GCAM_fuel, A_ind_fuel_map$Proc_name ) ]

printlog( "L230_Delete: DELETE EXISTING US TRN SECTOR" )
L230_DeleteSector <- A_ind_Delete[ A_ind_Delete$type == "supplysector", c( "region", "sector" ) ]
L230_DeleteFD <- A_ind_Delete[ A_ind_Delete$type == "energy-final-demand", c( "region", "sector" ) ]

printlog( "L230_Sector: SECTOR LEVEL PARAMS" )
# The sector table will include units strings as well as the logit exponent by region
L230_Sector <- data.frame( A_ind_sector, keyword="industry" )
L230_Sector <- L230_Sector[ sort( rep( 1:nrow( L230_Sector ), times = length( L230_subregions ) ) ), ]
L230_Sector <- data.frame( region=L230_subregions, L230_Sector )

# Create a table for subsector nest share-weights and logits
printlog( "L230_Subsector: SUBSECTOR LEVEL PARAMS" )
L230_Subsector <- A_ind_subs
L230_Subsector <- L230_Subsector[ sort( rep( 1:nrow( L230_Subsector ), times = length( L230_subregions ) ) ), ]
L230_Subsector <- data.frame( region=L230_subregions, L230_Subsector )
# The header expects the logit to be the last column
L230_Subsector$logit_exp <- L230_Subsector$logit
L230_Subsector$logit <- NULL

# Create a table for subsector interpolation rules
printlog( "L230_SubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L230_SubsInterpRule <- data.frame( A_ind_subs_interp, apply_to="share-weight" )
L230_SubsInterpRule <- L230_SubsInterpRule[ sort( rep( 1:nrow( L230_SubsInterpRule ), times = length( L230_subregions ) ) ), ]
L230_SubsInterpRule <- data.frame( region=L230_subregions, L230_SubsInterpRule )

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L230_GlobalDBTechInput: TECH INPUT" )
# The technologies that pull from states will not be placed in the global tech db
L230_GlobalDBTechInput <- A_ind_tech_input[ A_ind_tech_input$market != "*", names( A_ind_tech_input ) %!in% c( "tech_index" ) ]
# TODO: rounding?
L230_GlobalDBTechInterp <- data.frame( A_ind_tech_interp, apply_to="share-weight" )
L230_GlobalDBTechSecOutput <- data.frame( A_ind_tech_sec_out )
L230_GlobalDBTechSeq <- data.frame( A_ind_tech_seq )

# Create stubs to put in techs that did not exist at all in the base year.  The ones that did exist will get
# their stubs along with there calibration data.
printlog( "L230_TechStubs: TECH STUBS FOR FUTURE TECHS" )
# First come up with a list of future techs
L230_exist_techs <- aggregate( cbind( X1990, X2005 ) ~ GCAM_sector + subsector + technology,
    data=L230_fuel_cal, FUN=sum )
names( L230_exist_techs ) <- c( "supplysector", "subsector", "technology", X_GCAM_base_years )
L230_TechStubs <- unique( L230_GlobalDBTechInput[, c( "supplysector", "subsector", "technology" ) ] )
L230_TechStubs <- merge( L230_TechStubs, L230_exist_techs, all.x=TRUE )
L230_TechStubs <- L230_TechStubs[ is.na( L230_TechStubs$X1990 ), ]

# This is a bit awkward but I am going to zero out base year share weights for these future techs.
# I don't know why this wasn't done in the spreadsheet but it also means these will NEVER compete
# since the interpolation rule is just fixed, so what was the point?
L230_GlobalDBTechInput[
    paste( L230_GlobalDBTechInput$supplysector, L230_GlobalDBTechInput$subsector, L230_GlobalDBTechInput$technology )
    %in%
    paste( L230_TechStubs$supplysector, L230_TechStubs$subsector, L230_TechStubs$technology ) &
    L230_GlobalDBTechInput$year %in% GCAM_base_years, "share_weight" ] <- 0

# Now merge these future techs with regions
L230_TechStubs <- L230_TechStubs[ sort( rep( 1:nrow( L230_TechStubs ), times = length( L230_subregions ) ) ),
    c( "supplysector", "subsector", "technology" ) ]
L230_TechStubs <- data.frame( region=L230_subregions, L230_TechStubs )

# Table for calibration of transportation
printlog( "L230_TechCal: TECH CAL" )
# The calibration of technology by input is fairly easy as L230_fuel_cal pretty much gives us
# this however we need to compute services to calibrate final demand and the mix of energy-use vs
# feedstock
L230_ServiceCalc <- L230_fuel_cal
names( L230_ServiceCalc ) <- c( "region", "supplysector", "subsector", "technology",
    "minicam_energy_input", X_GCAM_base_years )
L230_ServiceCalc <- melt( L230_ServiceCalc, id=c( "region", "supplysector", "subsector", "technology",
    "minicam_energy_input" ), variable_name="year" )
L230_ServiceCalc$year <- substring( L230_ServiceCalc$year, 2 )
names( L230_ServiceCalc )[ ncol( L230_ServiceCalc ) ] <- "calibrated_value"
L230_ServiceCalc <- merge( L230_ServiceCalc, L230_GlobalDBTechInput[
    names( L230_GlobalDBTechInput ) %!in% c( "share_weight", "input_cost", "market" ) ] )

L230_TechCal <- L230_ServiceCalc
L230_TechCal$share_weight <- ifelse( L230_TechCal$calibrated_value > 0, 1, 0 )
L230_TechCal <- L230_TechCal[, c( "region", "supplysector", "subsector", "technology",
    "minicam_energy_input", "year", "share_weight", "calibrated_value", "efficiency" ) ]

printlog( "L230_IndustrySector: AGGREGATE ENERGY_USE AND FEEDSTOCK" )
L230_ServiceCalc$service <- L230_ServiceCalc$calibrated_value * L230_ServiceCalc$efficiency
# Now aggregate back to the sector level.  industry will need it's own table since
# it couldn't utilize the global tech db.  We can put it's calibration info in that table
# and the rest will be used for base service in the final demand
L230_ServiceCalc <- aggregate( service ~ region + supplysector + year, data=L230_ServiceCalc, FUN=sum )
L230_FDBaseService <- aggregate( service ~ region + year, data=L230_ServiceCalc, FUN=sum )
L230_IndustrySector <- data.frame( region=L230_ServiceCalc$region, minicam_energy_input=L230_ServiceCalc$supplysector,
    year=L230_ServiceCalc$year,
    coefficient=L230_ServiceCalc$service / L230_FDBaseService[
        match( paste( L230_ServiceCalc$region, L230_ServiceCalc$year ),
               paste( L230_FDBaseService$region, L230_FDBaseService$year ) ), "service" ] )
L230_IndustrySector$year <- as.numeric( levels( L230_IndustrySector$year )[ L230_IndustrySector$year ] )
L230_IndustrySector.75 <- L230_IndustrySector[ L230_IndustrySector$year == GCAM_base_years[1], ]
L230_IndustrySector.75$year <- GCAM_model_period0
L230_IndustrySector <- rbind( L230_IndustrySector, L230_IndustrySector.75 )
L230_IndustrySector <- merge( L230_IndustrySector, A_ind_tech_input[, c( "supplysector", "subsector",
    "technology", "year", "share_weight", "minicam_energy_input" ) ] )
L230_IndustrySector <- L230_IndustrySector[, c( "region", "supplysector", "subsector", "technology",
    "minicam_energy_input", "year", "coefficient", "share_weight" ) ]
    
# Create final demand input tables
printlog( "L230_FDBaseService" )
L230_FDBaseService <- data.frame( region=L230_FDBaseService$region, energy_final_demand="industry",
    L230_FDBaseService[, c( "year", "service" ) ] )
printlog( "L230_FDPercapitaBased" )
L230_FDPercapitaBased <- unique( A_ind_finaldemand[, c( "energy_final_demand", "perCapitaBased" ) ] )
L230_FDPercapitaBased <- L230_FDPercapitaBased[ sort( rep( 1:nrow( L230_FDPercapitaBased ), times = length( L230_subregions ) ) ), ]
L230_FDPercapitaBased <- data.frame( region=L230_subregions, L230_FDPercapitaBased )
printlog( "L230_FDElasticities" )
L230_FDElasticities <- A_ind_finaldemand[, names( A_ind_finaldemand ) %!in% c( "perCapitaBased" ) ]
L230_FDElasticities$income_year <- L230_FDElasticities$year
L230_FDElasticities$aeei_year <- L230_FDElasticities$year
L230_FDElasticities <- L230_FDElasticities[ sort( rep( 1:nrow( L230_FDElasticities ), times = length( L230_subregions ) ) ), ]
L230_FDElasticities <- data.frame( region=L230_subregions, L230_FDElasticities )
L230_FDElasticities <- L230_FDElasticities[, c( "region", "energy_final_demand", "year", "price_elast",
    "income_year", "income_elast", "aeei_year", "aeei" ) ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L230_DeleteSector, "SectorDelete", "L230_DeleteSector", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_DeleteFD, "FinalDemandDelete", "L230_DeleteFD", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_Sector, "SectorKeyword", "L230_Sector", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_Subsector, "ElecSubsector", "L230_Subsector", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_SubsInterpRule, "ElecSubsInterpRule", "L230_SubsInterpRule", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_GlobalDBTechInput, "GlobalDBTechInput", "L230_GlobalDBTechInput", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_GlobalDBTechInterp, "GlobalDBTechInterpRule", "L230_GlobalDBTechInterp", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_GlobalDBTechSecOutput, "GlobalDBTechSecOutput", "L230_GlobalDBTechSecOutput", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_GlobalDBTechSeq, "GlobalDBTechSeq", "L230_GlobalDBTechSeq", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_TechStubs, "TechStubs", "L230_TechStubs", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_IndustrySector, "IndustrySector", "L230_IndustrySector", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_TechCal, "TechCal", "L230_TechCal", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_FDBaseService, "FDBaseSerivce", "L230_FDBaseService", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_FDPercapitaBased, "FDPercapitaBased", "L230_FDPercapitaBased", "batch_rgcam_ind_base_input.xml" )
write_mi_data( L230_FDElasticities, "FDElasticitiesAEEI", "L230_FDElasticities", "batch_rgcam_ind_base_input.xml" )


insert_file_into_batchxml( "batch_rgcam_ind_base_input.xml", "rgcam_ind_base_input.xml", "", xml_tag="outFile" )

logstop()
