# L231_RGCAM_refliq_input.R
# Generates state level refined liquids input file where production is by
# fuel + state; then aggregated by state then aggregated by fuel.
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L231_RGCAM_refliq_input.R" )
printlog( "RGCAM refined liquids input file" )

# -----------------------------------------------------------------------------
# 1. Read files

A_refliq_Delete <- readdata( "A_refliq_Delete" )
A_refliq_sector <- readdata( "A_refliq_sector" )
A_refliq_co2_coef <- readdata( "A_refliq_co2_coef" )
A_refliq_subs <- readdata( "A_refliq_subs" )
A_refliq_subs_interp <- readdata( "A_refliq_subs_interp" )

A_refliq_tech_avail <- readdata( "A_refliq_tech_avail" )
A_refliq_tech_input <- readdata( "A_refliq_tech_input" )
A_refliq_tech_2nd <- readdata( "A_refliq_tech_2nd" )
A_refliq_tech_CCS <- readdata( "A_refliq_tech_CCS" )

L102_in_EJ_state_refining_F <- readdata( "L102_in_EJ_state_refining_F" )
L102_out_EJ_state_refining_F <- readdata( "L102_out_EJ_state_refining_F" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
L231_subregions <- unique( L102_out_EJ_state_refining_F$state )

# Fix some names, probably should do this upstream somewhere?
L231_fuel_cal <- L102_in_EJ_state_refining_F[, c( "state", "GCAM_sector", "GCAM_fuel", X_GCAM_base_years ) ]
L231_fuel_cal[ L231_fuel_cal$GCAM_fuel == "regional crude oil", "GCAM_fuel" ] <- "regional oil"
L231_fuel_cal[ L231_fuel_cal$GCAM_sector == "crude oil refining", "GCAM_sector" ] <- "oil refining"
L231_fuel_cal <- melt( L231_fuel_cal, id=c( "state", "GCAM_sector", "GCAM_fuel" ), variable_name="year" )
L231_fuel_cal$year <- substring( L231_fuel_cal$year, 2 )
names( L231_fuel_cal ) <- c( "region", "technology", "minicam_energy_input", "year", "calibrated_value" )

L231_fuel_output <- L102_out_EJ_state_refining_F[, c( "state", "GCAM_sector", X_GCAM_base_years ) ]
L231_fuel_output[ L231_fuel_output$GCAM_sector == "crude oil refining", "GCAM_sector" ] <- "oil refining"
L231_fuel_output <- melt( L231_fuel_output, id=c( "state", "GCAM_sector" ), variable_name="year" )
L231_fuel_output$year <- substring( L231_fuel_output$year, 2 )
names( L231_fuel_output ) <- c( "region", "technology", "year", "output" )

printlog( "L231_Delete: DELETE EXISTING US REFINERIES" )
L231_DeleteSector <- A_refliq_Delete

printlog( "L231_Sector: SECTOR LEVEL PARAMS" )
# The sector table will include units strings as well as the logit exponent by region
L231_sectors_in_states <- A_refliq_sector[ A_refliq_sector$by_state, "supplysector" ]
L231_Sector <- A_refliq_sector[ A_refliq_sector$by_state, names( A_refliq_sector ) %!in% c( "by_state" ) ]
L231_Sector <- L231_Sector[ sort( rep( 1:nrow( L231_Sector ), times = length( L231_subregions ) ) ), ]
L231_Sector <- data.frame( region=L231_subregions, L231_Sector )
L231_Sector <- rbind( L231_Sector, data.frame( region="USA", A_refliq_sector[ !A_refliq_sector$by_state,
    names( A_refliq_sector ) %!in% c( "by_state" ) ] ) )

# Specify CO2 coefficients for the new sectors we are creating
printlog( "L231_CO2Coefs: CO2 COEFS FOR NEW SECTORS" )
L231_CO2Coefs <- A_refliq_co2_coef[ A_refliq_co2_coef$by_state, names( A_refliq_co2_coef ) %!in% c( "by_state" ) ]
L231_CO2Coefs <- L231_CO2Coefs[ sort( rep( 1:nrow( L231_CO2Coefs ), times = length( L231_subregions ) ) ), ]
L231_CO2Coefs <- data.frame( region=L231_subregions, L231_CO2Coefs )
L231_CO2Coefs <- rbind( L231_CO2Coefs, data.frame( region="USA", A_refliq_co2_coef[ !A_refliq_co2_coef$by_state,
    names( A_refliq_co2_coef ) %!in% c( "by_state" ) ] ) )

# Create a table for subsector nest share-weights and logits
printlog( "L231_Subsector: SUBSECTOR LEVEL PARAMS" )
L231_Subsector <- A_refliq_subs[ A_refliq_subs$supplysector %in% L231_sectors_in_states, ]
L231_Subsector <- L231_Subsector[ sort( rep( 1:nrow( L231_Subsector ), times = length( L231_subregions ) ) ), ]
L231_Subsector <- data.frame( region=L231_subregions, L231_Subsector )
L231_Subsector <- rbind( L231_Subsector, data.frame( region="USA", A_refliq_subs[
    A_refliq_subs$supplysector %!in% L231_sectors_in_states, ] ) )

# Create a table for subsector interpolation rules
printlog( "L231_SubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L231_SubsInterpRule <- A_refliq_subs_interp[ A_refliq_subs_interp$supplysector %in% L231_sectors_in_states, ]
L231_SubsInterpRule <- data.frame( L231_SubsInterpRule, apply_to="share-weight" )
L231_SubsInterpRule <- L231_SubsInterpRule[ sort( rep( 1:nrow( L231_SubsInterpRule ), times = length( L231_subregions ) ) ), ]
L231_SubsInterpRule <- data.frame( region=L231_subregions, L231_SubsInterpRule )
L231_SubsInterpRule <- rbind( L231_SubsInterpRule, data.frame( region="USA", A_refliq_subs_interp[
    A_refliq_subs_interp$supplysector %!in% L231_sectors_in_states, ], apply_to="share-weight" ) )

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L231_GlobalDBTechInput: TECH INPUT" )
# The technologies that pull from states will not be placed in the global tech db
L231_GlobalDBTechInput <- A_refliq_tech_input[ A_refliq_tech_input$market != "*",
    names( A_refliq_tech_input ) %!in% c( "tech_index", "input_index" ) ]
# TODO: rounding?
L231_GlobalDBTechInputSecInput <- A_refliq_tech_2nd
L231_GlobalDBTechInputCCS <- A_refliq_tech_CCS[, names( A_refliq_tech_CCS ) %!in% c( "tech_index" ) ]
L231_GlobalDBTechAvail <- data.frame( A_refliq_tech_avail, final_available_year=-1 )

# Create stubs to put in techs that did not exist at all in the base year.  The ones that did exist will get
# their stubs along with there calibration data.
printlog( "L231_TechStubs: TECH STUBS FOR FUTURE TECHS" )
L231_TechStubs <- unique( L231_GlobalDBTechInput[ L231_GlobalDBTechInput$technology %!in%
    unique( L231_fuel_output$technology ), c( "supplysector", "subsector", "technology" ) ] )
L231_TechStubs <- L231_TechStubs[ sort( rep( 1:nrow( L231_TechStubs), times = length( L231_subregions ) ) ), ]
L231_TechStubs <- data.frame( region=L231_subregions, L231_TechStubs )

# This is a bit awkward but I am going to zero out base year share weights for these future techs.
# I don't know why this wasn't done in the spreadsheet but it also means these will NEVER compete
# since the interpolation rule is just fixed, so what was the point?
#L230_GlobalDBTechInput[
    #paste( L230_GlobalDBTechInput$supplysector, L230_GlobalDBTechInput$subsector, L230_GlobalDBTechInput$technology )
    #%in%
    #paste( L230_TechStubs$supplysector, L230_TechStubs$subsector, L230_TechStubs$technology ) &
    #L230_GlobalDBTechInput$year %in% GCAM_base_years, "share_weight" ] <- 0

# Table for calibration
printlog( "L231_CalOutput: TECH CAL" )
# For calibration we will just use CalDataOutput which is given in L231_fuel_output
# We will have to come up with coefficients and place those in a seperate table
L231_CalOutput <- data.frame( L231_fuel_output, L231_GlobalDBTechInput[ match(
    L231_fuel_output$technology, L231_GlobalDBTechInput$technology ), c( "supplysector", "subsector" ) ] )
L231_CalOutput$output <- round( L231_CalOutput$output, CalInput_digits )
L231_CalOutput <- L231_CalOutput[, c( "region", "supplysector", "subsector", "technology", "year", "output" ) ]
L231_CalOutput$share_weight <- ifelse( L231_CalOutput$output > 0, 1, 0 )

L231_InputCoef <- merge( L231_fuel_cal, L231_fuel_output )
L231_InputCoef$output <- round( L231_InputCoef$output, CalInput_digits )
L231_InputCoef <- L231_InputCoef[ L231_InputCoef$output > 0, ]
L231_InputCoef$coefficient <- L231_InputCoef$calibrated_value / L231_InputCoef$output
L231_InputCoef <- L231_InputCoef[, names( L231_InputCoef ) %!in% c( "calibrated_value", "output" ) ]
L231_InputCoef <- merge( L231_InputCoef, L231_CalOutput[, c( "region", "supplysector", "subsector",
    "technology", "year", "share_weight" ) ] )
L231_InputCoef <- L231_InputCoef[, c( "region", "supplysector", "subsector",
    "technology", "minicam_energy_input", "year", "coefficient", "share_weight" ) ]

# Now we need to calibrate/create tech inputs for the aggregation of refined liquids
# on state then fuel
L231_agg_temp <- L231_Sector[ L231_Sector$region == L231_subregions[1], ]
L231_agg_temp$region <- "USA"
L231_Sector <- rbind( L231_Sector, L231_agg_temp )

L231_agg_temp <- L231_CO2Coefs[ L231_CO2Coefs$region == L231_subregions[1], ]
L231_agg_temp$region <- "USA"
L231_CO2Coefs <- rbind( L231_CO2Coefs, L231_agg_temp )

L231_agg_temp <- L231_Subsector[ L231_Subsector$region == L231_subregions[1], ]
L231_agg_temp$region <- "USA"
# TODO: how to do assumptions for these
L231_agg_temp$logit <- -3
L231_Subsector <- rbind( L231_Subsector, L231_agg_temp )

L231_agg_temp <- L231_SubsInterpRule[ L231_SubsInterpRule$region == L231_subregions[1], ]
L231_agg_temp$region <- "USA"
L231_SubsInterpRule <- rbind( L231_SubsInterpRule, L231_agg_temp )

L231_TechInput <- L231_agg_temp[, c( "region", "supplysector", "subsector" ) ]
L231_TechInput <- L231_TechInput[ sort( rep( 1:nrow( L231_TechInput ), times = length( L231_subregions ) ) ), ]
L231_TechInput <- data.frame( L231_TechInput, technology=paste( L231_TechInput$supplysector, L231_subregions, sep="_" ) )
L231_TechInput$minicam_energy_input <- L231_TechInput$supplysector
L231_base_years = c( GCAM_base_years, 1975 )
L231_TechInput <- L231_TechInput[ sort( rep( 1:nrow( L231_TechInput ), times = length( L231_base_years ) ) ), ]
L231_TechInput <- data.frame( L231_TechInput, year=L231_base_years )
L231_TechInput$share_weight <- 0
L231_TechInput$calibrated_value <- -1
L231_agg_temp <- aggregate( output ~ region + supplysector + year, data=L231_CalOutput, FUN=sum )
L231_agg_temp$technology <- paste( L231_agg_temp$supplysector, L231_agg_temp$region, sep="_" )
L231_agg_temp$output <- round( L231_agg_temp$output, CalInput_digits )
L231_agg_temp <- L231_agg_temp[ L231_agg_temp$output > 0, c( "technology", "year", "output" ) ]
L231_agg_temp <- L231_agg_temp[ order( paste( L231_agg_temp$technology, L231_agg_temp$year ) ), ]
L231_TechInput <- L231_TechInput[ order( paste( L231_TechInput$technology, L231_TechInput$year ) ), ]
L231_TechInput[ which( !is.na( match( paste( L231_TechInput$technology, L231_TechInput$year ),
    paste( L231_agg_temp$technology, L231_agg_temp$year )
    ) ) ), c( "share_weight", "calibrated_value" ) ] <- data.frame( share_weight=1, calibrated_value=L231_agg_temp$output )
L231_TechInput$coefficient <- 1
L231_TechInput$input_cost <- 0
L231_TechInput$market <- gsub( ".*_", "", L231_TechInput$technology )
# TODO: into assumptions somewhere?
L231_TechInput[ L231_TechInput$supplysector %in% c( "coal refined liquids", "gas refined liquids" ), "share_weight" ] <- 1

# Now aggregate on fuel
L231_agg_temp <- data.frame( region="USA", A_refliq_tech_input[ A_refliq_tech_input$market == "*",
    c( "supplysector", "subsector", "technology", "minicam_energy_input", "year", "share_weight" ) ], calibrated_value=-1,
    A_refliq_tech_input[ A_refliq_tech_input$market == "*", c( "coefficient", "input_cost" ) ], market="USA" )
L231_agg_temp2 <- aggregate( output ~ supplysector + year, data=L231_CalOutput[ L231_CalOutput$output > 0, ], FUN=sum )
L231_agg_temp <- L231_agg_temp[ order( paste( L231_agg_temp$minicam_energy_input, L231_agg_temp$year ) ), ]
L231_agg_temp2 <- L231_agg_temp2[ order( paste( L231_agg_temp2$supplysector, L231_agg_temp2$year ) ), ]
L231_agg_temp[ which( !is.na( match( paste( L231_agg_temp$minicam_energy_input, L231_agg_temp$year ),
    paste( L231_agg_temp2$supplysector, L231_agg_temp2$year ) ) ) ), c( "share_weight", "calibrated_value" ) ] <-
    data.frame( share_weight=1, calibrated_value=L231_agg_temp2$output )
L231_TechInput <- rbind( L231_TechInput, L231_agg_temp )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L231_DeleteSector, "SectorDelete", "L231_DeleteSector", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_Sector, "PassThroughSector", "L231_Sector", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_CO2Coefs, "CO2Coefs", "L231_CO2Coefs", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_Subsector, "ElecSubsector", "L231_Subsector", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_SubsInterpRule, "ElecSubsInterpRule", "L231_SubsInterpRule", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_GlobalDBTechAvail, "GlobalDBTechAvail", "L231_GlobalDBTechAvail", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_GlobalDBTechInput, "GlobalDBTechInputRefLiq", "L231_GlobalDBTechInput", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_GlobalDBTechInputSecInput, "GlobalDBTechInputSecInput", "L231_GlobalDBTechInputSecInput", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_GlobalDBTechInputCCS, "GlobalDBTechCCS", "L231_GlobalDBTechInputCCS", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_TechStubs, "TechStubs", "L231_TechStubs", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_CalOutput, "CalOutput", "L231_CalOutput", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_InputCoef, "IndustrySector", "L231_InputCoef", "batch_rgcam_refined_liquids_input.xml" )
write_mi_data( L231_TechInput, "PassThroughTechInput", "L231_TechInput", "batch_rgcam_refined_liquids_input.xml" )

insert_file_into_batchxml( "batch_rgcam_refined_liquids_input.xml", "rgcam_refined_liquids_input.xml", "", xml_tag="outFile" )

logstop()
