# L210_RGCAM_elec_supply.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L210_RGCAM_elec_supply.R" )
printlog( "RGCAM electricity generation sectors model input file" )

# -----------------------------------------------------------------------------
# 1. Read files

A_elecS_Delete <- readdata( "A_elecS_Delete" )
A_elecS_misc <- readdata( "A_elecS_misc" )
A_elecS_subs_logit <- readdata( "A_elecS_subs_logit" )
A_elecS_tech_logit <- readdata( "A_elecS_tech_logit" )
A_elecS_subs_interp <- readdata( "A_elecS_subs_interp" )
A_elecS_subs_sw <- readdata( "A_elecS_subs_sw" )

A_elecS_tech_avail <- readdata( "A_elecS_tech_avail" )
A_elecS_tech_input <- readdata( "A_elecS_tech_input" )
A_elecS_tech_exist <- readdata( "A_elecS_tech_exist" )
A_elecS_tech_CCS <- readdata( "A_elecS_tech_CCS" )
A_elecS_tech_backup <- readdata( "A_elecS_tech_backup" )

L155_state_elec_supply <- readdata( "L155_state_elec_supply" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
printlog( "L210_ElecDelete: DELETE EXISTING US ELECT GEN SECTOR" )
L210_ElecDelete <- A_elecS_Delete 

printlog( "L210_ElecSector: SECTOR LEVEL PARAMS" )
# We will have to put reserve margin and grid capacity factor in a seperate table
# since they need to be in rows
L210_SectorMetaInfo <- melt( A_elecS_misc[ c( "supplysector", "electricity_reserve_margin", "average_grid_capacity_factor" ) ], id="supplysector" )
L210_SectorMetaInfo$variable <- gsub( "_", "-", L210_SectorMetaInfo$variable )
# The sector table will include units strings as well as the logit exponent by region
L210_ElecSector <- A_elecS_misc[ c( "supplysector", "input_unit", "output_unit", "price_unit" ) ]
L210_ElecSector[ "logit_expontent" ] <- A_elecS_subs_logit$subs_logit
L210_ElecSector <- L210_ElecSector[ rep( 1:nrow( L210_ElecSector ), times = length( states ) ), ]
L210_ElecSector <- data.frame( region=states, L210_ElecSector )

L210_SectorMetaInfo <- L210_SectorMetaInfo[ rep( 1:nrow( L210_SectorMetaInfo ), times = length( states ) ), ]
L210_SectorMetaInfo <- data.frame( region=states, L210_SectorMetaInfo )

# Create a table for subsector nest share-weights and logits
printlog( "L210_ElecSubsector: SUBSECTOR LEVEL PARAMS" )
stopifnot( nrow( A_elecS_tech_logit ) == nrow( A_elecS_subs_sw ) )
L210_ElecSubsector <- data.frame( A_elecS_subs_sw[ order( A_elecS_subs_sw$supplysector, A_elecS_subs_sw$subsector ), ],
                                  A_elecS_tech_logit[ order( A_elecS_tech_logit$supplysector, A_elecS_tech_logit$subsector ), "tech_logit" ] )
L210_ElecSubsector <- L210_ElecSubsector[ sort( rep( 1:nrow( L210_ElecSubsector ), times = length( states ) ) ), ]
L210_ElecSubsector <- data.frame( region=states, L210_ElecSubsector )

# Create a table for subsector interpolation rules
# Note base year share-weights may be adjusted below for subsectors
# with no production will get a zero share-weight for that year
printlog( "L210_ElecSubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L210_ElecSubsInterpRule <- data.frame( A_elecS_subs_interp, apply_to="share-weight" )
L210_ElecSubsInterpRule <- L210_ElecSubsInterpRule[ sort( rep( 1:nrow( L210_ElecSubsInterpRule ), times = length( states ) ) ), ]
L210_ElecSubsInterpRule <- data.frame( region=states, L210_ElecSubsInterpRule )

# Create a table for technology availability years
printlog( "L210_GlobalDBTechAvail: TECH AVAIL" )
L210_GlobalDBTechAvail <- A_elecS_tech_avail[ A_elecS_tech_avail$initial_available_year != -1
                                              | A_elecS_tech_avail$final_available_year != -1, ]

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L210_GlobalDBTechInput: TECH INPUT" )
# First drop rows which are invalid
L210_GlobalDBTechInput <- A_elecS_tech_input[ A_elecS_tech_input$period != -1 & !is.na(A_elecS_tech_input$lifetime), ]

# Hydro is just a fixed output technology and doesn't have cost info so we will pull it out
# and treat it sperately
L210_GlobalDBTechInput <- L210_GlobalDBTechInput[ L210_GlobalDBTechInput$subsector != "hydro", ]

# Geothermal doesn't have a detailed cost break down so it too must be handled seperately
L210_GeothermalInput <- L210_GlobalDBTechInput[ L210_GlobalDBTechInput$subsector == "geothermal",c( "supplysector",
                                                "subsector", "technology", "period", "share_weight", "capital_cost",
                                                "lifetime", "efficiency", "minicam_energy_input", "market" ) ]
names( L210_GeothermalInput )[ names( L210_GeothermalInput ) == "capital_cost" ]  <- "non_energy_cost"
L210_GeothermalInput$non_energy_cost <- round( L210_GeothermalInput$non_energy_cost, cost_digits )
L210_GeothermalInput$efficiency <- round( L210_GeothermalInput$efficiency, CalInput_digits )
L210_GlobalDBTechInput <- L210_GlobalDBTechInput[ L210_GlobalDBTechInput$subsector != "geothermal", ]

# Round costs and efficiencies as appropriate
L210_cost_names <- c( "capital_cost", "fixed_om", "variable_om", "fcr", "capacity_factor", "capacity_factor_OM" )
L210_GlobalDBTechInput[ , L210_cost_names ] <- round( L210_GlobalDBTechInput[, L210_cost_names ], cost_digits )
L210_GlobalDBTechInput$efficiency <- round( L210_GlobalDBTechInput$efficiency, CalInput_digits )

# Intermittent technologies will need their own header so they must be split out.
# These will be found by the technologies that require backup and those parameters
# will just get mereged into one table for simplicity.
printlog( "L210_GlobalDBTechIntermit: TECH INTERMITTENT" )
L210_int_techs <- L210_GlobalDBTechInput$supplysector %in% A_elecS_tech_backup$supplysector &
                  L210_GlobalDBTechInput$subsector %in% A_elecS_tech_backup$subsector &
                  L210_GlobalDBTechInput$technology %in% A_elecS_tech_backup$technology &
                  L210_GlobalDBTechInput$period %in% A_elecS_tech_backup$period
L210_GlobalDBTechIntermit <- L210_GlobalDBTechInput[ L210_int_techs, ]
L210_GlobalDBTechIntermit <- data.frame( L210_GlobalDBTechIntermit[ order( L210_GlobalDBTechIntermit$supplysector,
                                                                           L210_GlobalDBTechIntermit$subsector,
                                                                           L210_GlobalDBTechIntermit$technology,
                                                                           L210_GlobalDBTechIntermit$period ), ],
                                         A_elecS_tech_backup[ order( A_elecS_tech_backup$supplysector,
                                                                     A_elecS_tech_backup$subsector,
                                                                     A_elecS_tech_backup$technology,
                                                                     A_elecS_tech_backup$period ), c(
                                                                     "capacity_limit", "trial_market_name",
                                                                     "electricity_sector", "electricity_sector_market",
                                                                     "backup_capital_cost", "backup_capacity_factor" ) ] )
L210_GlobalDBTechIntermit <- L210_GlobalDBTechIntermit[ , names( L210_GlobalDBTechIntermit ) %!in% c( "market" ) ]
L210_GlobalDBTechInput <- L210_GlobalDBTechInput[ !L210_int_techs, ]

# The availability information must also be special cased due to the header being different.
L210_int_techs <- L210_GlobalDBTechAvail$supplysector %in% A_elecS_tech_backup$supplysector &
                  L210_GlobalDBTechAvail$subsector %in% A_elecS_tech_backup$subsector &
                  L210_GlobalDBTechAvail$technology %in% A_elecS_tech_backup$technology

L210_GlobalDBTechIntermit[ c( "initial_available_year", "final_available_year" ) ] <- 
    L210_GlobalDBTechAvail[ match(
        paste( L210_GlobalDBTechIntermit$supplysector, L210_GlobalDBTechIntermit$subsector, L210_GlobalDBTechIntermit$technology ),
        paste( L210_GlobalDBTechAvail$supplysector, L210_GlobalDBTechAvail$subsector, L210_GlobalDBTechAvail$technology ) ),
    c( "initial_available_year", "final_available_year" ) ]
L210_GlobalDBTechIntermit$initial_available_year[ is.na( L210_GlobalDBTechIntermit$initial_available_year ) ] <- -1
L210_GlobalDBTechIntermit$final_available_year[ is.na( L210_GlobalDBTechIntermit$final_available_year ) ] <- -1
L210_GlobalDBTechAvail <- L210_GlobalDBTechAvail[ !L210_int_techs, ]

# Due to the way the tables are processed we have to put the backup energy
# for intermittent technologies into a seperate table
printlog( "L210_GlobalDBTechIntermitBackup: TECH BACKUP ENERGY" )
L210_GlobalDBTechIntermitBackup <- A_elecS_tech_backup[ , names( A_elecS_tech_backup ) %!in% c(
                                                                     "capacity_limit", "trial_market_name",
                                                                     "electricity_sector", "electricity_sector_market",
                                                                     "backup_capital_cost", "backup_capacity_factor" ) ]


# Create a table for technology shutdown parameters
printlog( "L210_GlobalDBTechShutdown: TECH SHUTDOWN" )
L210_GlobalDBTechShutdown <- data.frame( A_elecS_tech_exist, period=GCAM_base_years[ 2 ] )
L210_GlobalDBTechShutdown$steepness <- round( L210_GlobalDBTechShutdown$steepness, retirement_fn_digits )

# Create a table for CCS parameters
printlog( "L210_GlobalDBTechCCS: TECH CCS" )
L210_GlobalDBTechCCS <- A_elecS_tech_CCS

# Create a table for base year calibration data
printlog( "L210_TechCal: TECH CALIBRATION DATA" )
L210_TechCal <- data.frame( L155_state_elec_supply[ rep( 1:nrow( L155_state_elec_supply ), times = length( GCAM_base_years ) ),
                            c( "state", "technology" ) ],
                            year = sort( rep( GCAM_base_years, times = nrow( L155_state_elec_supply ) ) ),
                            share_weight=1 )
L210_TechCal$calibrated_value[ L210_TechCal$year == GCAM_base_years[1] ] <- L155_state_elec_supply$in1990
L210_TechCal$efficiency[ L210_TechCal$year == GCAM_base_years[1] ] <- L155_state_elec_supply$gen1990 / L155_state_elec_supply$in1990
L210_TechCal$calibrated_value[ L210_TechCal$year == GCAM_base_years[2] ] <- L155_state_elec_supply$in2005
L210_TechCal$efficiency[ L210_TechCal$year == GCAM_base_years[2] ] <- L155_state_elec_supply$gen2005 / L155_state_elec_supply$in2005
L210_TechCal$calibrated_value <- round( L210_TechCal$calibrated_value, CalInput_digits )
# Match in additional columns necessary to make the input table
L210_TechCal <- data.frame( L210_TechCal, A_elecS_tech_input[
            match( L210_TechCal$technology, A_elecS_tech_input$technology ),
            c( "supplysector", "subsector", "minicam_energy_input" ) ] )
# Reorder columns just matched in for aesthetics
L210_TechCal.numcols <- length( L210_TechCal )
L210_TechCal <- L210_TechCal[, c(1, L210_TechCal.numcols-2, L210_TechCal.numcols-1, 2, L210_TechCal.numcols, 3:(L210_TechCal.numcols-3) ) ]
# Copy subsectors with no production so we can zero out the subsector share-weight before
# we remove them from the data frame.
L210_empty_subs <- L210_TechCal[ L210_TechCal$calibrated_value <= 0, c( "state", "supplysector", "subsector", "year" ) ]
L210_TechCal <- L210_TechCal[ L210_TechCal$calibrated_value > 0, ]
L210_TechCal$efficiency <- round( L210_TechCal$efficiency, CalInput_digits )
# Special case the renewables since the code seems to override the efficiencies to 1 regardless
L210_TechCal[ L210_TechCal$subsector %in% c( "wind", "solar" ), c( "calibrated_value", "efficiency" ) ] <-
    cbind( L210_TechCal[ L210_TechCal$subsector %in% c( "wind", "solar" ), ]$calibrated_value *
           L210_TechCal[ L210_TechCal$subsector %in% c( "wind", "solar" ), ]$efficiency, 1 )
# Special case the geothermal efficiency since it seems to be very different than assumed
L210_TechCal[ L210_TechCal$subsector %in% c( "geothermal" ), c( "calibrated_value", "efficiency" ) ] <-
    cbind( L210_TechCal[ L210_TechCal$subsector %in% c( "geothermal" ), ]$calibrated_value *
           L210_TechCal[ L210_TechCal$subsector %in% c( "geothermal" ), ]$efficiency / 0.1, 0.1 )

# Reset subsector share-weights in the base year to zero where there is zero production
# and 1 otherwise
printlog( "Adjust base year sw in L210_ElecSubsInterpRule" )
L210_ElecSubsInterpRule[ do.call( "paste", L210_ElecSubsInterpRule[, c( "region", "supplysector", "subsector" ) ] ) %in%
                         do.call( "paste", L210_empty_subs[ L210_empty_subs$year == GCAM_base_years[1], c( "state", "supplysector", "subsector" ) ] ),
                         paste( "sw_", GCAM_base_years[1], sep="" ) ] <- 0
L210_ElecSubsInterpRule[ do.call( "paste", L210_ElecSubsInterpRule[, c( "region", "supplysector", "subsector" ) ] ) %in%
                         do.call( "paste", L210_empty_subs[ L210_empty_subs$year == GCAM_base_years[2], c( "state", "supplysector", "subsector" ) ] ),
                         paste( "sw_", GCAM_base_years[2], sep="" ) ] <- 0

# Create input table for Hydro
printlog( "L210_HydroInput: Hydro TECH INPUT" )
L210_HydroInput <- L210_TechCal[ L210_TechCal$subsector == "hydro", ]
# This is kind of goofy but we need to convert input back to output
L210_HydroInput$calibrated_value <- L210_HydroInput$calibrated_value * L210_HydroInput$efficiency
L210_HydroInput <- L210_HydroInput[ , names( L210_HydroInput ) %!in% c( "efficiency" ) ]
L210_HydroInput$share_weight <- 0
L210_HydroInputFuture <- L210_HydroInput[ L210_HydroInput$year == GCAM_base_years[2], names( L210_HydroInput ) %!in% c( "year" ) ]

L210_HydroInputFuture <- L210_HydroInputFuture[ sort( rep( 1:nrow( L210_HydroInputFuture ), times = length( GCAM_future_years ) ) ), ]
L210_HydroInputFuture <- data.frame( L210_HydroInputFuture, year=GCAM_future_years )
L210_HydroInput <- rbind( L210_HydroInput, L210_HydroInputFuture )
# Force a zero value where hydro is in one base period but not the other
# Use an "outer join" to find the mismatches
L210_HydroInputFuture <- merge( L210_HydroInput[ L210_HydroInput$year == GCAM_base_years[1], names( L210_HydroInput ) %!in% c( "calibrated_value" ) ],
       L210_HydroInput[ L210_HydroInput$year == GCAM_base_years[2], names( L210_HydroInput ) %!in% c( "year" ) ],
       all=TRUE)
# If the year is na then there was a value in 2005 but not 1990
# If the calibrated_value is na then there was a value in 1990 but not 2005
L210_HydroInputFuture <- L210_HydroInputFuture[ is.na( L210_HydroInputFuture$year ) |
                                                is.na( L210_HydroInputFuture$calibrated_value ), ]
L210_HydroInputFuture$year <- ifelse( is.na( L210_HydroInputFuture$year ), GCAM_base_years[1], GCAM_base_years[2] )
L210_HydroInputFuture$calibrated_value <- 0
L210_HydroInput <- rbind( L210_HydroInput, L210_HydroInputFuture )
L210_HydroInput$initial_available_year <- GCAM_base_years[1]
# Remove hydro from tech cal table
L210_TechCal <- L210_TechCal[ L210_TechCal$subsector != "hydro", ]
# Reset hydro subsector share weights to zero
#L210_ElecSubsector[ L210_ElecSubsector$subsector == "hydro", c( paste( "sw_", GCAM_base_years, sep="" ) ) ] <- 0

# Nuclear is fixed output so much be treated seperately
printlog( "L210_NucFixedOutput: Nuclear FIXED OUTPUT" )
L210_NucFixedOutput <- L210_TechCal[ L210_TechCal$subsector == "nuclear", names( L210_TechCal ) %!in% c( "minicam_energy_input" ) ]
# This is kind of goofy but we need to convert input back to output
L210_NucFixedOutput$calibrated_value <- L210_NucFixedOutput$calibrated_value * L210_NucFixedOutput$efficiency
L210_NucFixedOutput <- L210_NucFixedOutput[ , names( L210_NucFixedOutput ) %!in% c( "efficiency" ) ]
L210_NucFixedOutput$share_weight <- 0
# Force a zero value where nuclear is in one base period but not the other
# Use an "outer join" to find the mismatches
L210_NucFixedOutput.missing <- merge( L210_NucFixedOutput[ L210_NucFixedOutput$year == GCAM_base_years[1], names( L210_NucFixedOutput) %!in% c( "calibrated_value" ) ],
       L210_NucFixedOutput[ L210_NucFixedOutput$year == GCAM_base_years[2], names( L210_NucFixedOutput) %!in% c( "year" ) ],
       all=TRUE)
# If the year is na then there was a value in 2005 but not 1990
# If the calibrated_value is na then there was a value in 1990 but not 2005
L210_NucFixedOutput.missing<- L210_NucFixedOutput.missing[ is.na( L210_NucFixedOutput.missing$year ) |
                                                           is.na( L210_NucFixedOutput.missing$calibrated_value ), ]
L210_NucFixedOutput.missing$year <- ifelse( is.na( L210_NucFixedOutput.missing$year ), GCAM_base_years[1], GCAM_base_years[2] )
L210_NucFixedOutput.missing$calibrated_value <- 0
L210_NucFixedOutput<- rbind( L210_NucFixedOutput, L210_NucFixedOutput.missing )
L210_NucFixedOutput$initial_available_year <- GCAM_base_years[1]
# Remove nuclear from tech cal table
L210_TechCal <- L210_TechCal[ L210_TechCal$subsector != "nuclear", ]
# Reset hydro subsector share weights to zero
#L210_ElecSubsector[ L210_ElecSubsector$subsector == "nuclear", c( paste( "sw_", GCAM_base_years, sep="" ) ) ] <- 0

# Read stubs for future technologies, existing ones get taken care of by the
# calibration tables
printlog( "L210_TechStubs: TECH STUBS" )
L210_TechStubs <- A_elecS_tech_avail[ A_elecS_tech_avail$initial_available_year > GCAM_base_years[2], c( "supplysector", "subsector", "technology" ) ]
# Allow geothermal in all regions, they will be take out by the resource
# processing if a state has no geothermal potential
L210_TechStubs <- rbind( L210_TechStubs, unique( L210_GeothermalInput[,
    c( "supplysector", "subsector", "technology" ) ] ) )
# TODO: some consideration for excluding some technologies in some regions?
L210_TechStubs <- L210_TechStubs[ rep( 1:nrow( L210_TechStubs ), times = length( states ) ), ]
L210_TechStubs <- data.frame( region=states, L210_TechStubs )
# Add stubs for regions that did not have wind in the base year so that they may in the future
L210_BaseWind <- unique( subset( L210_TechCal, subsector == "wind", select=
    c("state", "supplysector", "subsector", "technology" ) ) )
L210_TechStubs <- rbind( L210_TechStubs, data.frame( region = states[ states %!in% L210_BaseWind$state ],
    L210_BaseWind[ 1, c("supplysector", "subsector", "technology" ) ] ) )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L210_ElecDelete, "SectorDelete", "L210_ElecDelete", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_ElecSector, "Sector", "L210_ElecSector", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_SectorMetaInfo, "SectorMetaInfo", "L210_SectorMetaInfo", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_ElecSubsector, "ElecSubsector", "L210_ElecSubsector", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_ElecSubsInterpRule, "ElecSubsInterpRule", "L210_ElecSubsInterpRule", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_GlobalDBTechAvail, "GlobalDBTechAvail", "L210_GlobalDBTechAvail", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_GlobalDBTechInput, "ElecGlobalDBTechInput", "L210_GlobalDBTechInput", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_GeothermalInput, "GeothermalInput", "L210_GeothermalInput", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_GlobalDBTechShutdown, "GlobalDBTechShutdown", "L210_GlobalDBTechShutdown", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_GlobalDBTechCCS, "GlobalDBTechCCS", "L210_GlobalDBTechCCS", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_GlobalDBTechIntermitBackup, "GlobalDBTechIntermitBackup", "L210_GlobalDBTechIntermitBackup", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_GlobalDBTechIntermit, "GlobalDBTechIntermit", "L210_GlobalDBTechIntermit", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_TechCal, "TechCal", "L210_TechCal", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_HydroInput, "HydroInput", "L210_HydroInput", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_NucFixedOutput, "NucFixedOutput", "L210_NucFixedOutput", "batch_rgcam_elec_supply_input_base.xml" ) 
write_mi_data( L210_TechStubs, "TechStubs", "L210_TechStubs", "batch_rgcam_elec_supply_input_base.xml" ) 


insert_file_into_batchxml( "batch_rgcam_elec_supply_input_base.xml", "rgcam_elec_supply_input_base.xml", "", xml_tag="outFile" )
#insert_file_into_batchxml( "batch_rgcam_bld_adv.xml", "rgcam_bld_adv.xml", "", xml_tag="outFile" )
#insert_file_into_batchxml( "batch_rgcam_bld_frz.xml", "rgcam_bld_frz.xml", "", xml_tag="outFile" )

logstop()
