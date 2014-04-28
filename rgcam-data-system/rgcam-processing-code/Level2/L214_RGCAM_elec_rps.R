# L214_RGCAM_elec_rps.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L214_RGCAM_elec_rps.R" )
printlog( "RGCAM electricity state level renewable portfolio standards" )

# -----------------------------------------------------------------------------
# 1. Read files

state_region_ind <- readdata( "state_region_ind" )

A_elecS_tech_avail <- readdata( "A_elecS_tech_avail" )
A_elecS_tech_input <- readdata( "A_elecS_tech_input" )
A_res_tech_input <- readdata( "A_res_tech_input" )

DSIRE_RPS_inclusions <- readdata( "DSIRE_RPS_inclusions" )
DSIRE_RPS_share <- readdata( "DSIRE_RPS_share" )
DSIRE_RPS_capacity <- readdata( "DSIRE_RPS_capacity" )

L155_state_elec_supply <- readdata( "L155_state_elec_supply" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files

L214_elecS_tech_input <- unique( rbind( A_elecS_tech_input[, c( "supplysector", "subsector", "technology" ) ],
    A_res_tech_input[ A_res_tech_input$technology == "rooftop_pv", c( "supplysector", "subsector", "technology" ) ] ) )

# Collapse DSIRE RPS definitions into a single RPS by state
printlog( "L214_DSIRE_inclusions: Collapse tiers into a single definition" )
L214_DSIRE_inclusions <- melt( DSIRE_RPS_inclusions, id.vars=c( "state", "tier" ) )
L214_DSIRE_inclusions <- aggregate( value ~ state + variable, L214_DSIRE_inclusions, FUN=max )
L214_renew_cat_names <- unique( L214_DSIRE_inclusions$variable )
L214_DSIRE_inclusions <- cast( L214_DSIRE_inclusions, state ~ variable )
L214_inclusion_misc <- L214_DSIRE_inclusions[, c( "state", "load_covered", "existing_allowed") ]
L214_DSIRE_inclusions <- L214_DSIRE_inclusions[, names( L214_DSIRE_inclusions )[
    names( L214_DSIRE_inclusions ) %!in% c( "load_covered", "existing_allowed" ) ] ]
L214_DSIRE_inclusions <- data.frame( L214_DSIRE_inclusions )
L214_DSIRE_inclusions <- melt( L214_DSIRE_inclusions, id.vars=c( "state" ) )

# convert percentages to fractions
L214_inclusion_misc$load_covered <- L214_inclusion_misc$load_covered / 100
L214_inclusion_misc$existing_allowed <- L214_inclusion_misc$existing_allowed / 100

printlog( "L214_DSIRE_inclusions: map in GCAM tech names" )
L214_tech_names <- data.frame( technology=unique( L214_elecS_tech_input$technology ) )
for( renew_name in L214_renew_cat_names ) {
    L214_tech_names[ grepl( renew_name, L214_tech_names$technology ), "variable" ] <- renew_name
}
L214_tech_names <- subset( L214_tech_names, !is.na( variable ) )

# Map in technology names
L214_DSIRE_inclusions <- merge( L214_DSIRE_inclusions, L214_tech_names )

# Remove technologies that are not included
L214_DSIRE_inclusions <- merge( L214_DSIRE_inclusions, L214_inclusion_misc )
L214_DSIRE_inclusions <- subset( L214_DSIRE_inclusions, value > 0 )
L214_DSIRE_inclusions <- subset( L214_DSIRE_inclusions, !grepl( 'exist', technology ) | existing_allowed > 0 )
# WARNING: exluding hydro all to gether if existing is not allowed since we are
# currently assuming no hydro growth
L214_DSIRE_inclusions <- subset( L214_DSIRE_inclusions, !grepl( 'hydro', technology ) | existing_allowed > 0 )

cleanup_year_profile <- function(d) {
    d <- melt( d, id.vars=c( "state" ), variable_name="year" )
    d$year <- as.integer( sub( '^X', '', d$year ) )

    # Collapse tiers
    d <- aggregate( value ~ state + year, d, FUN=sum )

    # We are assuming any policy in the calibration years are taken care of by share-weight
    # so adjust these to be relative to that
    d.final_cal <- subset( d, year == final_cal_year, select=c( "state", "value" ) )
    names( d.final_cal )[ 2 ] <- "final_cal_value"
    d <- subset( d, value > 0 )
    d <- merge( d, d.final_cal )
    d <- subset( d, year > final_cal_year )
    d$value <- d$value - d$final_cal_value
    d$final_cal_value <- NULL

    # Remove states where the RPS is not specified via shares/capacity
    d.rownsum <- aggregate( value ~ state, d, FUN=sum )
    d.rownsum <- subset( d.rownsum, value > 0, select=c( "state" ) )
    d <- merge( d.rownsum, d )
    max_year <- max( d$year )

    # fillout the values outside to the specified range
    # TODO: better way than looping?
    d <- cast( d, state ~ year, fill=0 )
    for( c in 3:ncol( d ) ) {
        for( r in 1:nrow( d ) ) {
            if( d[ r, c ] == 0 ) {
                d[ r, c ] = d[ r, c - 1 ]
            }
        }
    }

    # Extend to include the GCAM future years as well
    d[, as.character( GCAM_future_years[ GCAM_future_years > max_year ] ) ] <- d[, as.character( max_year ) ]
    d[, "2100" ] <- d[, as.character( max_year ) ]
    d <- data.frame( d )
    d <- melt( d, id.vars=c( "state" ), variable_name="year" )
    d$year <- as.integer( sub( '^X', '', d$year ) )

    # only include data at model years
    # TODO hard coding year logic since we want to allow the 5 year data here
    d <- subset( d, year %% 5 == 0 )
    return(d)
}

printlog( "L214_DSIRE_share: Collapse tiers into a single share definition" )
L214_DSIRE_share <- cleanup_year_profile( DSIRE_RPS_share )

printlog( "L214_DSIRE_capacity: Collapse tiers into a single capacity definition" )
L214_DSIRE_capacity <- cleanup_year_profile( DSIRE_RPS_capacity )

printlog( "convert state names to state abbreviations" )
convert_state_abbrv <- function(d) {
    names( d )[ names( d ) == "state" ] <- "state_name"
    d <- merge( d, state_region_ind[, c( "state", "state_name" ) ] )
    d$state_name <- NULL
    return(d)
}
L214_DSIRE_inclusions <- convert_state_abbrv( L214_DSIRE_inclusions )
L214_inclusion_misc <- convert_state_abbrv( L214_inclusion_misc )
L214_DSIRE_share <- convert_state_abbrv( L214_DSIRE_share )
L214_DSIRE_capacity <- convert_state_abbrv( L214_DSIRE_capacity )

# For share based RPS states we will need to create an RES market
# The applicable renewables will get a input-subsidy which will
# set the supply.  All generation technologies will get a new
# energy input that demands a pass-through sector RPS_demand
# which will set the demand for the RES market and contain the share
# share as well as adjust for load_covered.
L214_share_states <- unique( L214_DSIRE_share$state )
L214_subsidy_years <- seq( 2005, 2100, 5 )

printlog( "Create RPS policy" )
L214_RPSPolicy <- data.frame( region=L214_share_states, policy_portfolio_standard="RPS_policy",
    market=L214_share_states, policyType="RES" )

L214_RPSPolicyConstraint <- data.frame( region=L214_share_states, policy_portfolio_standard="RPS_policy" )
L214_RPSPolicyConstraint <- L214_RPSPolicyConstraint[ sort( rep( 1:nrow( L214_RPSPolicyConstraint ),
    times=length( L214_subsidy_years ) ) ), ]
L214_RPSPolicyConstraint <- data.frame( L214_RPSPolicyConstraint, year=L214_subsidy_years )
# For RES markets where the supply and demand is dynamic we just set the constraint value to zero
L214_RPSPolicyConstraint$constraint <- 0
L214_RPSPolicyConstraint <- subset( L214_RPSPolicyConstraint, year > final_cal_year )
# "Trun off" the market where the constraint is zero to avoid having to try to solve it
L214_RPSPolicyConstraint[ paste( L214_RPSPolicyConstraint$region, L214_RPSPolicyConstraint$year ) %in%
    with( L214_DSIRE_share[ L214_DSIRE_share$value == 0, ], paste( state, year ) ), "constraint" ] <- -1

printlog( "Create RPS_demand sector/subsector" )
L214_Sector <- data.frame( region=L214_share_states, supplysector="RPS_demand",
    output_unit="EJ", input_unit="EJ", price_unit="1975$/GJ", logit="-3" )

L214_Subsector <- data.frame( region=L214_share_states, supplysector="RPS_demand",
    subsector="RPS_demand", sw_2020=1, sw_2035=1, sw_2050=1, sw_2065=1, sw_2080=1, sw_2095=1, logit="-3" )

L214_SubsInterp <- data.frame( region=L214_share_states, supplysector="RPS_demand",
    subsector="RPS_demand", sw_1975=0, sw_1990=0, sw_2005=1, from_year=2005, to_year=2020, interpolation_function="linear",
    apply_to="share-weight" )

L214_TechAvail <- data.frame( region=L214_share_states, supplysector="RPS_demand", subsector="RPS_demand",
    technology="RPS_demand", initial_available_year=2005, final_available_year=-1 )

L214_TechInput <- data.frame( region=L214_share_states, supplysector="RPS_demand", subsector="RPS_demand",
    technology="RPS_demand", minicam_energy_input="RPS_policy", share_weight=1 )

# Merge in the shares and adjust for load convered
L214_TechInput <- merge( L214_TechInput, L214_DSIRE_share, by.x="region", by.y="state" )
L214_TechInput <- merge( L214_TechInput, L214_inclusion_misc[, c( "state", "load_covered" ) ], by.x="region", by.y="state" )
L214_TechInput$value <- L214_TechInput$value * L214_TechInput$load_covered
# Add a 2005 year even though the policy won't start until 2010 to avoid
# having demand for the RPS_demand sector but no supply options in 2005.
L214_TechInput.2005 <- subset( L214_TechInput, year == 2010 )
L214_TechInput.2005$year <- 2005
L214_TechInput.2005$value <- 0
L214_TechInput <- rbind( L214_TechInput.2005, L214_TechInput )

# Reorder names for header consistency
L214_TechInput <- L214_TechInput[, c( "region", "supplysector", "subsector", "technology", "minicam_energy_input",
    "year", "value", "share_weight" ) ]

remove_invalid_tech_years <- function(d) {
    d <- merge( d, A_elecS_tech_avail )
    d <- subset( d, year >= initial_available_year & ( final_available_year == -1 | year <= final_available_year ) )
    invalid_hydro <- subset( L155_state_elec_supply, GCAM_fuel == "hydro" & (gen1990 > 0 & gen2005 == 0),
        select=c( "state", "technology" ) )
    d <- d[ paste( d$state, d$technology ) %!in% paste( invalid_hydro$state, invalid_hydro$technology ), ]
    # TODO: problematic for 2010 hydro?
    d <- subset(d, subsector != "hydro" | year %in% GCAM_years )
    return(d)
}

printlog( "L214_SubsidyInput: Create subsidy input tags for the techs that should be included in the RPS" )
L214_SubsidyInput <- L214_DSIRE_inclusions[ sort( rep( 1:nrow( L214_DSIRE_inclusions ), times=length( L214_subsidy_years ) ) ), ]
L214_SubsidyInput <- subset( L214_SubsidyInput, state %in% L214_share_states )
L214_SubsidyInput <- data.frame( L214_SubsidyInput, year=L214_subsidy_years )
L214_SubsidyInput[ L214_SubsidyInput$year == 2005, "value" ] <- L214_SubsidyInput[ L214_SubsidyInput$year == 2005, "value" ] *
    L214_SubsidyInput[ L214_SubsidyInput$year == 2005, "existing_allowed" ]
L214_SubsidyInput <- subset( L214_SubsidyInput, value > 0 )
L214_SubsidyInput <- merge( L214_SubsidyInput, L214_elecS_tech_input[, c( "supplysector", "subsector", "technology" ) ] )
L214_SubsidyInput$input_subsidy <- "RPS_policy"
L214_SubsidyInput <- remove_invalid_tech_years( L214_SubsidyInput )
# Reorder names for header consistency
L214_SubsidyInput <- L214_SubsidyInput[, c( "state", "supplysector", "subsector", "technology", "year", 
    "input_subsidy", "value" ) ]

printlog( "L214_TaxInput: Create input for elec techs that should add to the 'demand' of the policy" )
L214_TaxInput <- L214_elecS_tech_input[ sort( rep( 1:nrow( L214_elecS_tech_input ), times=length( L214_subsidy_years ) ) ), ]
L214_TaxInput <- data.frame( L214_TaxInput, year=L214_subsidy_years )
L214_TaxInput <- L214_TaxInput[ sort( rep( 1:nrow( L214_TaxInput ), times=length( L214_share_states ) ) ), ]
L214_TaxInput <- data.frame( state=L214_share_states, L214_TaxInput )
L214_TaxInput$coeficient <- 1
L214_TaxInput$p_mult <- 0
L214_TaxInput$minicam_energy_input <- "RPS_demand"
L214_TaxInput <- remove_invalid_tech_years( L214_TaxInput )
# Reorder names for header consistency
L214_TaxInput <- L214_TaxInput[, c( "state", "supplysector", "subsector", "technology", "year",
    "minicam_energy_input", "coeficient", "p_mult" ) ]

# For capacity based RPS states we will create a subsidy market
# This means we set the constraint into a policy-portfolio-standard
# which is set as the demand.  We will then need to have the appropriate
# renewables set the supply using an input-subsidy

# WARNING: I've decided not to implement the capacity constraint since as it stands now 
# the only state with a valid capacity constraint is Texas and even there they have
# already exceeded the constraint without the explicit policy.

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L214_RPSPolicy, "RPSPolicy", "L214_RPSPolicy", "batch_rgcam_elec_rps.xml" ) 
write_mi_data( L214_RPSPolicyConstraint, "RPSPolicyConstraint", "L214_RPSPolicyConstraint", "batch_rgcam_elec_rps.xml" ) 
write_mi_data( L214_Sector, "Sector", "L214_Sector", "batch_rgcam_elec_rps.xml" ) 
write_mi_data( L214_Subsector, "ElecSubsector", "L214_Subsector", "batch_rgcam_elec_rps.xml" ) 
write_mi_data( L214_SubsInterp, "ElecSubsInterpRule", "L214_SubsInterp", "batch_rgcam_elec_rps.xml" ) 
write_mi_data( L214_TechAvail, "TechAvail", "L214_TechAvail", "batch_rgcam_elec_rps.xml" ) 
write_mi_data( L214_TechInput, "IndustrySector", "L214_TechInput", "batch_rgcam_elec_rps.xml" ) 
write_mi_data( L214_SubsidyInput, "SubsidyInput", "L214_SubsidyInput", "batch_rgcam_elec_rps.xml" ) 
write_mi_data( L214_TaxInput, "TaxInput", "L214_TaxInput", "batch_rgcam_elec_rps.xml" ) 

insert_file_into_batchxml( "batch_rgcam_elec_rps.xml", "rgcam_elec_rps.xml", "", xml_tag="outFile" )
logstop()
