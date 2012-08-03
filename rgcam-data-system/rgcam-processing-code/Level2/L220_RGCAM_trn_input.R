# L220_RGCAM_trn_input.R
# Generates state level detailed transportation input file
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L220_RGCAM_trn_input.R" )
printlog( "RGCAM detailed transportation input file" )

# -----------------------------------------------------------------------------
# 1. Read files

A_trn_fuel_map <- readdata( "A_trn_fuel_map" )
A_trn_Delete <- readdata( "A_trn_Delete" )
A_trn_sector <- readdata( "A_trn_sector" )
A_trn_subs <- readdata( "A_trn_subs" )
A_trn_subs_interp <- readdata( "A_trn_subs_interp" )

A_trn_tech_interp <- readdata( "A_trn_tech_interp" )
A_trn_tech_input <- readdata( "A_trn_tech_input" )

A_trn_finaldemand <- readdata( "A_trn_finaldemand" )

L140_in_EJ_state_trn_det_F <- readdata( "L140_in_EJ_state_trn_det_F" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
L220_fuel_cal <- L140_in_EJ_state_trn_det_F[ , c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_GCAM_base_years ) ]
L220_fuel_cal[, X_GCAM_base_years ] <- round( L220_fuel_cal[, X_GCAM_base_years ], CalInput_digits )
L220_fuel_cal <- L220_fuel_cal[ L220_fuel_cal$X1990 > 0 | L220_fuel_cal$X2005 > 0, ]
L220_fuel_cal$GCAM_fuel <- A_trn_fuel_map$GCAM_name[ match( L220_fuel_cal$GCAM_fuel, A_trn_fuel_map$Proc_name ) ]

# Only sectors + modes that exist in the base year will exist in the future
L220_sector_mode_list <- unique( L220_fuel_cal[, c( "state", "GCAM_sector", "GCAM_mode" ) ] )

printlog( "L220_Delete: DELETE EXISTING US TRN SECTOR" )
L220_DeleteSector <- A_trn_Delete[ A_trn_Delete$type == "supplysector", c( "region", "sector" ) ]
L220_DeleteFD <- A_trn_Delete[ A_trn_Delete$type == "energy-final-demand", c( "region", "sector" ) ]

printlog( "L220_Sector: SECTOR LEVEL PARAMS" )
# The sector table will include units strings as well as the logit exponent by region
L220_Sector <- unique( L220_sector_mode_list[, c( "state", "GCAM_sector" ) ] )
L220_Sector <- data.frame( region=L220_Sector$state, A_trn_sector[ match( L220_Sector$GCAM_sector,
    A_trn_sector$supplysector ), ], keyword="transportation" )

# Create a table for subsector nest share-weights and logits
printlog( "L220_TrnSubsector: SUBSECTOR LEVEL PARAMS" )
L220_TrnSubsector <- L220_sector_mode_list
L220_TrnSubsector <- rbind( L220_TrnSubsector, data.frame( state=unique( L220_sector_mode_list$state ),
    GCAM_sector="trn_passenger", GCAM_mode="road" ) )
L220_TrnSubsector <- data.frame( region=L220_TrnSubsector$state, A_trn_subs[ match(
    paste( L220_TrnSubsector$GCAM_sector, L220_TrnSubsector$GCAM_mode ),
    paste( A_trn_subs$supplysector,  A_trn_subs$subsector ) ), ] )

# Create a table for subsector interpolation rules
printlog( "L220_SubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L220_SubsInterpRule <- L220_TrnSubsector[, c( "region", "supplysector", "subsector" ) ]
L220_SubsInterpRule <- merge( L220_SubsInterpRule, A_trn_subs_interp )
# the region column has gotten out of place, move it back to first
L220_SubsInterpRule <- L220_SubsInterpRule[, c( "region", names( L220_SubsInterpRule )[ which( names( L220_SubsInterpRule ) != "region" ) ] ) ]
L220_SubsInterpRule <- data.frame( L220_SubsInterpRule, apply_to="share-weight" )

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L220_GlobalDBTechInput: TECH INPUT" )
# The technologies that pull from states will not be placed in the global tech db
L220_GlobalDBTechInput <- A_trn_tech_input[ A_trn_tech_input$market != "*", names( A_trn_tech_input ) %!in% c( "tech_index" ) ]
# TODO: rounding?
L220_GlobalDBTechInterp <- data.frame( A_trn_tech_interp, apply_to="share-weight" )

# Create stubs to put in techs that did not exist at all in the base year.  The ones that did exist will get
# their stubs along with there calibration data.
printlog( "L220_TechStubs: TECH STUBS FOR FUTURE TECHS" )
# First come up with a list of future techs
L220_exist_techs <- aggregate( cbind( X1990, X2005 ) ~ GCAM_sector + GCAM_mode + GCAM_fuel, data=L220_fuel_cal, FUN=sum )
names( L220_exist_techs ) <- c( "supplysector", "subsector", "minicam_energy_input", X_GCAM_base_years )
L220_TechStubs <- unique( L220_GlobalDBTechInput[, c( "supplysector", "subsector", "technology", "minicam_energy_input" ) ] )
L220_TechStubs <- merge( L220_TechStubs, L220_exist_techs, all.x=TRUE )
L220_TechStubs <- L220_TechStubs[ is.na( L220_TechStubs$X1990 ), ]
# Now merge these future techs with regional subsectors being careful not to create sector/subsectors which did not exist
L220_TechStubs <- merge( L220_TrnSubsector[, c("region", "supplysector", "subsector" ) ],
    L220_TechStubs[, c("supplysector", "subsector", "technology" ) ] )
# the region column has gotten out of place, move it back to first
L220_TechStubs <- L220_TechStubs[, c( "region", names( L220_TechStubs )[ which( names( L220_TechStubs ) != "region" ) ] ) ]

# Table for calibration of transportation
printlog( "L220_TrnTechCal: TECH CAL" )
# The calibration of technology by input is fairly easy as L220_fuel_cal pretty much gives us
# this however we need to compute services to calibrate final demand and the trn_pass_road pass-through
L220_ServiceCalc <- L220_fuel_cal
names( L220_ServiceCalc ) <- c( "region", "supplysector", "subsector", "minicam_energy_input", X_GCAM_base_years )
L220_ServiceCalc <- merge( L220_ServiceCalc, L220_GlobalDBTechInput[ L220_GlobalDBTechInput$year == GCAM_model_period0,
    names( L220_GlobalDBTechInput ) %!in% c( "year", "share_weight", "input_cost", "market" ) ] )

L220_TrnTechCal <- L220_ServiceCalc[, c( "region", "supplysector", "subsector", "technology",
    "minicam_energy_input", X_GCAM_base_years ) ]
L220_TrnTechCal <- melt( L220_TrnTechCal, id=c( "region", "supplysector", "subsector", "technology",
    "minicam_energy_input" ), variable_name="year" )
L220_TrnTechCal$year <- substring( L220_TrnTechCal$year, 2 )
names( L220_TrnTechCal )[ ncol( L220_TrnTechCal ) ] <- "calibrated_value"
L220_TrnTechCal$share_weight <- ifelse( L220_TrnTechCal$calibrated_value > 0, 1, 0 )

# Formula is weird due to unit conversions, taken from Global_tran_input.xls!GCAM_trn_1990
L220_ServiceCalc$ser_1990 <- L220_ServiceCalc$X1990 * 1e9 * L220_ServiceCalc$loadFactor /
    ( L220_ServiceCalc$coefficient * 1.0551 )
L220_ServiceCalc$ser_2005 <- L220_ServiceCalc$X2005 * 1e9 * L220_ServiceCalc$loadFactor /
    ( L220_ServiceCalc$coefficient * 1.0551 )
# Now aggregate back to the sector level.  trn_pass_road will need it's own table since
# it couldn't utilize the global tech db.  We can put it's calibration info in that table
# and the rest will be used for base service in the final demand
L220_ServiceCalc <- aggregate( cbind( ser_1990, ser_2005 ) ~ region + supplysector, data=L220_ServiceCalc, FUN=sum )
L220_trn_pass_road <- L220_ServiceCalc[ L220_ServiceCalc$supplysector == "trn_pass_road", ]
L220_trn_pass_road$ser_1975 <- -1
L220_trn_pass_road <- melt( L220_trn_pass_road, id=c( "region", "supplysector" ) )
names( L220_trn_pass_road ) <- c( "region", "minicam_energy_input", "year", "calibrated_value" )
L220_trn_pass_road$year <- substring( L220_trn_pass_road$year, 5 )
L220_trn_pass_road <- merge( L220_trn_pass_road, A_trn_tech_input[ A_trn_tech_input$market == "*",
    names( A_trn_tech_input ) %!in% c( "input_cost", "loadFactor", "market", "tech_index" ) ] )
L220_trn_pass_road <- L220_trn_pass_road[, c( "region", "supplysector", "subsector", "technology", "minicam_energy_input",
    "year", "share_weight", "calibrated_value", "coefficient" ) ]

# Create final demand input tables
printlog( "L220_FDBaseService" )
L220_FDBaseService <- L220_ServiceCalc
L220_FDBaseService$supplysector[ L220_FDBaseService$supplysector == "trn_pass_road" ] <- "trn_passenger"
L220_FDBaseService <- aggregate( cbind( ser_1990, ser_2005 ) ~ region + supplysector, data=L220_FDBaseService, FUN=sum )
printlog( "L220_FDPercapitaBased" )
L220_FDPercapitaBased <- unique( A_trn_finaldemand[, c( "energy_final_demand", "perCapitaBased" ) ] )
L220_FDPercapitaBased <- data.frame( region=L220_FDBaseService$region, L220_FDPercapitaBased[
    match( L220_FDBaseService$supplysector, L220_FDPercapitaBased$energy_final_demand ), ] )
printlog( "L220_FDElasticities" )
L220_FDElasticities <- A_trn_finaldemand[, names( A_trn_finaldemand ) %!in% c( "perCapitaBased" ) ]
L220_FDElasticities$income_year <- L220_FDElasticities$year
L220_FDElasticities <- merge( L220_FDPercapitaBased[, c( "region", "energy_final_demand" ) ], L220_FDElasticities )
L220_FDElasticities <- L220_FDElasticities[, c( "region", "energy_final_demand", "year", "price_elast",
    "income_year", "income_elast" ) ]
L220_FDBaseService <- melt( L220_FDBaseService, id=c( "region", "supplysector" ), variable_name="year" )
L220_FDBaseService$year <- substring( L220_FDBaseService$year, 5 )
names( L220_FDBaseService ) <- c( "region", "energy_final_demand", "year", "base_service" )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L220_DeleteSector, "SectorDelete", "L220_DeleteSector", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_DeleteFD, "FinalDemandDelete", "L220_DeleteFD", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_Sector, "SectorKeyword", "L220_Sector", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_TrnSubsector, "TrnSubsector", "L220_TrnSubsector", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_SubsInterpRule, "TrnSubsInterpRule", "L220_SubsInterpRule", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_GlobalDBTechInput, "TrnGlobalDBTechInput", "L220_GlobalDBTechInput", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_GlobalDBTechInterp, "TrnGlobalDBTechInterpRule", "L220_GlobalDBTechInterp", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_TechStubs, "TrnTechStubs", "L220_TechStubs", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_trn_pass_road, "TrnTechInput", "L220_trn_pass_road", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_TrnTechCal, "TrnTechCal", "L220_TrnTechCal", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_FDBaseService, "FDBaseSerivce", "L220_FDBaseService", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_FDPercapitaBased, "FDPercapitaBased", "L220_FDPercapitaBased", "batch_rgcam_trn_base_input.xml" )
write_mi_data( L220_FDElasticities, "FDElasticities", "L220_FDElasticities", "batch_rgcam_trn_base_input.xml" )


insert_file_into_batchxml( "batch_rgcam_trn_base_input.xml", "rgcam_trn_base_input.xml", "", xml_tag="outFile" )

logstop()
