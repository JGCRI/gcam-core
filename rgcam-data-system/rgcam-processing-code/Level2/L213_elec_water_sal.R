# L213_RGCAM_elec_water.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L213_elec_water_sal.R" )
printlog( "RGCAM electricity generation water demands model input file" )

# -----------------------------------------------------------------------------
# 1. Read files

states_subregions <- readdata( "states_subregions" )

L155_state_elec_supply <- readdata( "L155_state_elec_supply" )

A_elecS_tech_input <- readdata( "A_elecS_tech_input" )

elec_water_coefficients <- readdata( "elec_water_coefficients" )
elec_water_coefficients_FA <- readdata( "elec_water_coefficients_FA" )
UCS_elec_cooling_fresh <- readdata( "UCS_elec_cooling_fresh" )
UCS_elec_cooling_saline <- readdata( "UCS_elec_cooling_saline" )
UCS_elec_cooling_fresh_refs <- readdata( "UCS_elec_cooling_fresh_refs" )
UCS_elec_cooling_saline_refs <- readdata( "UCS_elec_cooling_saline_refs" )
UCS_tech_names <- readdata( "UCS_tech_names" )
UCS_fuel_names <- readdata( "UCS_fuel_names" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files

L213_states_subregions <- subset( states_subregions, state != "US", select=c( "state", "subregion_FERC" ) )

# Calculate cooling technology shares.  This is done twice, first shares by fuel for existing
# plants since they do not differentiate generation technology.  Second by generation technology
# to use as a bassis for future shares.  We will also calculate total region shares by generation
# technology to fill missing data.
printlog( "Calculate cooling technology shares." )
L213_UCS_elec_cooling <- melt( UCS_elec_cooling_fresh, variable_name="cooling_tech" )
L213_UCS_elec_cooling$water_type <- "fresh"
L213_UCS_elec_cooling <- rbind( L213_UCS_elec_cooling, data.frame(
    melt( UCS_elec_cooling_saline, variable_name="cooling_tech" ), water_type="saline" ) )
L213_UCS_elec_cooling <- subset( L213_UCS_elec_cooling, UCS_technology != "" )
#Append saline to the cooling tech name
L213_UCS_elec_cooling$cooling_tech <- as.character( L213_UCS_elec_cooling$cooling_tech )
L213_UCS_elec_cooling$cooling_tech[ L213_UCS_elec_cooling$water_type == "saline" ] <- paste(
      L213_UCS_elec_cooling$cooling_tech[ L213_UCS_elec_cooling$water_type == "saline" ], "saline" )

L213_UCS_elec_cooling.refs <- melt( UCS_elec_cooling_fresh_refs, variable_name="cooling_tech" )
L213_UCS_elec_cooling.refs$water_type <- "fresh"
L213_UCS_elec_cooling.refs <- rbind( L213_UCS_elec_cooling.refs, data.frame(
    melt( UCS_elec_cooling_saline_refs, variable_name="cooling_tech" ), water_type="saline" ) )
L213_UCS_elec_cooling.refs <- subset( L213_UCS_elec_cooling.refs, UCS_technology != "" )
L213_UCS_elec_cooling.refs$cooling_tech <- as.character( L213_UCS_elec_cooling.refs$cooling_tech )
L213_UCS_elec_cooling.refs$cooling_tech[ L213_UCS_elec_cooling.refs$water_type == "saline" ] <- paste(
      L213_UCS_elec_cooling.refs$cooling_tech[ L213_UCS_elec_cooling.refs$water_type == "saline" ], "saline" )
L213_refs_error_flags <- subset( UCS_elec_cooling_fresh_refs, error_flag != "", select=c("region", "fuel", "UCS_technology", "error_flag"))

calc_shares <- function(data, aggr_formula) {
    agg <- aggregate( aggr_formula, data, FUN=sum )
    names( agg )[ length( names( agg ) ) ] <- "total"
    data <- merge( data, agg )
    data$share <- data$value / data$total
    return( data )
}

#L213_UCS_elec_cooling_by_fuel <- aggregate( value ~ region + fuel + UCS_technology + cooling_tech + water_type, L213_UCS_elec_cooling, FUN=sum )
L213_UCS_elec_cooling_by_fuel <- L213_UCS_elec_cooling
L213_UCS_elec_cooling_by_fuel.refs <- L213_UCS_elec_cooling.refs
L213_UCS_elec_cooling_by_fuel_region <- aggregate( value ~ fuel + UCS_technology + cooling_tech + water_type, subset( L213_UCS_elec_cooling, region != "USA" ), FUN=sum )
L213_UCS_elec_cooling_by_region <- aggregate( value ~ UCS_technology + cooling_tech + water_type, subset( L213_UCS_elec_cooling, region != "USA" ), FUN=sum )
L213_UCS_elec_cooling_by_region.refs <- aggregate( value ~ UCS_technology + cooling_tech + water_type, subset( L213_UCS_elec_cooling.refs, region != "USA" ), FUN=sum )
L213_UCS_elec_cooling_by_fuel_region.refs <- aggregate( value ~ fuel + UCS_technology + cooling_tech + water_type, subset( L213_UCS_elec_cooling.refs, region != "USA" ), FUN=sum )
names( L213_states_subregions ) <- c( "region", "subregion" )
L213_UCS_elec_cooling_by_subregion <- merge( L213_UCS_elec_cooling, L213_states_subregions )
L213_UCS_elec_cooling_by_subregion <- aggregate( value ~ subregion + UCS_technology + cooling_tech + water_type, L213_UCS_elec_cooling_by_subregion, FUN=sum )

L213_UCS_elec_cooling <- calc_shares( L213_UCS_elec_cooling, value ~ region + UCS_technology )
L213_UCS_elec_cooling.refs <- calc_shares( L213_UCS_elec_cooling.refs, value ~ region + UCS_technology )
L213_UCS_elec_cooling_by_fuel <- subset( L213_UCS_elec_cooling_by_fuel, UCS_technology != "Pumped Storage" )
L213_UCS_elec_cooling_by_fuel <- calc_shares( L213_UCS_elec_cooling_by_fuel, value ~ region + fuel )
L213_UCS_elec_cooling_by_fuel.refs <- subset( L213_UCS_elec_cooling_by_fuel.refs, UCS_technology != "Pumped Storage" )
L213_UCS_elec_cooling_by_fuel.refs <- calc_shares( L213_UCS_elec_cooling_by_fuel.refs, value ~ region + fuel )
L213_geo_filter <- unique( paste( L213_UCS_elec_cooling_by_fuel[ grepl( "Geothermal", L213_UCS_elec_cooling_by_fuel$UCS_technology ) & !is.na( L213_UCS_elec_cooling_by_fuel$share ), "region" ],
                       L213_UCS_elec_cooling_by_fuel[ grepl( "Geothermal", L213_UCS_elec_cooling_by_fuel$UCS_technology ) & !is.na( L213_UCS_elec_cooling_by_fuel$share ), "UCS_technology" ] ) )
L213_UCS_elec_cooling <- L213_UCS_elec_cooling[ paste( L213_UCS_elec_cooling$region, L213_UCS_elec_cooling$UCS_technology ) %!in% L213_geo_filter, ]
L213_UCS_elec_cooling <- rbind( L213_UCS_elec_cooling,
	L213_UCS_elec_cooling_by_fuel[ grepl( "Geothermal", L213_UCS_elec_cooling_by_fuel$UCS_technology ) & !is.na( L213_UCS_elec_cooling_by_fuel$share ), names( L213_UCS_elec_cooling ) ] )
L213_geo_filter <- unique( paste( L213_UCS_elec_cooling_by_fuel.refs[ grepl( "Geothermal", L213_UCS_elec_cooling_by_fuel.refs$UCS_technology ) & !is.na( L213_UCS_elec_cooling_by_fuel.refs$share ), "region" ],
                       L213_UCS_elec_cooling_by_fuel.refs[ grepl( "Geothermal", L213_UCS_elec_cooling_by_fuel.refs$UCS_technology ) & !is.na( L213_UCS_elec_cooling_by_fuel.refs$share ), "UCS_technology" ] ) )
L213_UCS_elec_cooling.refs <- L213_UCS_elec_cooling.refs[ paste( L213_UCS_elec_cooling.refs$region, L213_UCS_elec_cooling.refs$UCS_technology ) %!in% L213_geo_filter, ]
L213_UCS_elec_cooling.refs <- rbind( L213_UCS_elec_cooling.refs,
	L213_UCS_elec_cooling_by_fuel.refs[ grepl( "Geothermal", L213_UCS_elec_cooling_by_fuel.refs$UCS_technology ) & !is.na( L213_UCS_elec_cooling_by_fuel.refs$share ), names( L213_UCS_elec_cooling.refs ) ] )

L213_UCS_elec_cooling_by_fuel_region <- calc_shares( L213_UCS_elec_cooling_by_fuel_region, value ~ fuel )
L213_UCS_elec_cooling_by_fuel_region.refs <- calc_shares( L213_UCS_elec_cooling_by_fuel_region.refs, value ~ fuel )
L213_UCS_elec_cooling_by_region <- calc_shares( L213_UCS_elec_cooling_by_region, value ~ UCS_technology )
L213_UCS_elec_cooling_by_region[  grepl( 'Geothermal', L213_UCS_elec_cooling_by_region$UCS_technology ), ] <- L213_UCS_elec_cooling_by_fuel_region[
	grepl( 'Geothermal', L213_UCS_elec_cooling_by_fuel_region$UCS_technology ), names( L213_UCS_elec_cooling_by_region ) ]
L213_UCS_elec_cooling_by_region.refs <- calc_shares( L213_UCS_elec_cooling_by_region.refs, value ~ UCS_technology )
L213_UCS_elec_cooling_by_region.refs[  grepl( 'Geothermal', L213_UCS_elec_cooling_by_region.refs$UCS_technology ), ] <- L213_UCS_elec_cooling_by_fuel_region.refs[
	grepl( 'Geothermal', L213_UCS_elec_cooling_by_fuel_region.refs$UCS_technology ), names( L213_UCS_elec_cooling_by_region.refs ) ]
L213_UCS_elec_cooling_by_subregion <- calc_shares( L213_UCS_elec_cooling_by_subregion, value ~ subregion + UCS_technology )

# We no longer care to keep track of saline
## Yeah we do!
#L213_UCS_elec_cooling <- subset( L213_UCS_elec_cooling, water_type != "saline" )
#L213_UCS_elec_cooling.refs <- subset( L213_UCS_elec_cooling.refs, water_type != "saline" )
#test <- subset( L213_UCS_elec_cooling_by_fuel, !is.na( share ) & share > 0 )
#L213_UCS_elec_cooling_by_fuel <- subset( L213_UCS_elec_cooling_by_fuel, water_type != "saline" )

# Figure out what to do about missing data
printlog( "Fill missing base year share data for use with future technologies" )
# In the reference scenario nuclear does not have any data so instead we will use Generic steam plant (coal)
L213_UCS_cooling_by_region_nuclear = L213_UCS_elec_cooling_by_region.refs$UCS_technology == "Generic steam plant (nuclear)"
L213_UCS_cooling_by_region_coal = L213_UCS_elec_cooling_by_region.refs$UCS_technology == "Generic steam plant (coal)"
L213_UCS_elec_cooling_by_region.refs[  L213_UCS_cooling_by_region_nuclear, "share" ] <- L213_UCS_elec_cooling_by_region.refs[ L213_UCS_cooling_by_region_coal, ][
    match( paste( L213_UCS_elec_cooling_by_region.refs[ L213_UCS_cooling_by_region_nuclear, "cooling_tech" ],
				  L213_UCS_elec_cooling_by_region.refs[ L213_UCS_cooling_by_region_nuclear, "water_type" ] ),
           paste( L213_UCS_elec_cooling_by_region.refs[ L213_UCS_cooling_by_region_coal, "cooling_tech" ],
				  L213_UCS_elec_cooling_by_region.refs[ L213_UCS_cooling_by_region_coal, "water_type" ] ) ), "share" ]
stopifnot( nrow( subset( L213_UCS_elec_cooling_by_region.refs, is.na( share ) ) ) == 0 )

L213_UCS_elec_cooling[ is.na( L213_UCS_elec_cooling$share ), "share" ] <- L213_UCS_elec_cooling_by_region[
    match( paste( L213_UCS_elec_cooling[ is.na( L213_UCS_elec_cooling$share ), "UCS_technology" ],
                  L213_UCS_elec_cooling[ is.na( L213_UCS_elec_cooling$share ), "cooling_tech" ],
				  L213_UCS_elec_cooling[ is.na( L213_UCS_elec_cooling$share ), "water_type" ] ),
           paste( L213_UCS_elec_cooling_by_region$UCS_technology, L213_UCS_elec_cooling_by_region$cooling_tech, L213_UCS_elec_cooling_by_region$water_type ) ), "share" ]

# In the reference scenario nuclear does not have any data so instead we will use Generic steam plant (coal) by state
L213_UCS_cooling_nuclear = L213_UCS_elec_cooling.refs$UCS_technology == "Generic steam plant (nuclear)"
L213_UCS_cooling_coal = L213_UCS_elec_cooling.refs$UCS_technology == "Generic steam plant (coal)"
L213_UCS_elec_cooling.refs[  L213_UCS_cooling_nuclear, "share" ] <- L213_UCS_elec_cooling.refs[ L213_UCS_cooling_coal, ][
    match( paste( L213_UCS_elec_cooling.refs[ L213_UCS_cooling_nuclear, "region" ],
	              L213_UCS_elec_cooling.refs[ L213_UCS_cooling_nuclear, "cooling_tech" ],
				  L213_UCS_elec_cooling.refs[ L213_UCS_cooling_nuclear, "water_type" ] ),
           paste( L213_UCS_elec_cooling.refs[ L213_UCS_cooling_coal, "region" ],
		          L213_UCS_elec_cooling.refs[ L213_UCS_cooling_coal, "cooling_tech" ],
				  L213_UCS_elec_cooling.refs[ L213_UCS_cooling_coal, "water_type" ] ) ), "share" ]

# Adjust reference shares based on error flags to which guard against too few data points
# First find region/technologies that should be replaced with the reference share national averages
L213_error_filter <- paste( L213_UCS_elec_cooling.refs$region, L213_UCS_elec_cooling.refs$UCS_technology ) %in%
    paste( subset( L213_refs_error_flags, error_flag == "National average")$region, subset( L213_refs_error_flags, error_flag == "National average" )$UCS_technology )
L213_UCS_elec_cooling.refs[ L213_error_filter, "share" ] <- L213_UCS_elec_cooling_by_region.refs[
    match( paste( L213_UCS_elec_cooling.refs[ L213_error_filter, "UCS_technology" ],
                  L213_UCS_elec_cooling.refs[ L213_error_filter, "cooling_tech" ],
				  L213_UCS_elec_cooling.refs[ L213_error_filter, "water_type" ] ),
           paste( L213_UCS_elec_cooling_by_region.refs$UCS_technology, L213_UCS_elec_cooling_by_region.refs$cooling_tech, L213_UCS_elec_cooling_by_region.refs$water_type ) ), "share" ]

# Next find region/technologies that should be replace with the historical subregional averages
L213_error_filter <- paste( L213_UCS_elec_cooling.refs$region, L213_UCS_elec_cooling.refs$UCS_technology ) %in%
    paste( subset( L213_refs_error_flags, error_flag == "Historical")$region, subset( L213_refs_error_flags, error_flag == "Historical" )$UCS_technology )
L213_UCS_elec_cooling.refs[ L213_error_filter, "share" ] <- L213_UCS_elec_cooling_by_subregion[
    match( paste( L213_UCS_elec_cooling.refs[ L213_error_filter, "region" ],
                  L213_UCS_elec_cooling.refs[ L213_error_filter, "UCS_technology" ],
                  L213_UCS_elec_cooling.refs[ L213_error_filter, "cooling_tech" ],
				  L213_UCS_elec_cooling.refs[ L213_error_filter, "water_type" ] ),
           paste( L213_UCS_elec_cooling_by_subregion$subregion,
                  L213_UCS_elec_cooling_by_subregion$UCS_technology,
                  L213_UCS_elec_cooling_by_subregion$cooling_tech,
                  L213_UCS_elec_cooling_by_subregion$water_type ) ), "share" ]

# For the rest of the missing values just use national average
L213_UCS_elec_cooling.refs[ is.na( L213_UCS_elec_cooling.refs$share ), "share" ] <- L213_UCS_elec_cooling_by_region.refs[
    match( paste( L213_UCS_elec_cooling.refs[ is.na( L213_UCS_elec_cooling.refs$share ), "UCS_technology" ],
                  L213_UCS_elec_cooling.refs[ is.na( L213_UCS_elec_cooling.refs$share ), "cooling_tech" ],
				  L213_UCS_elec_cooling.refs[ is.na( L213_UCS_elec_cooling.refs$share ), "water_type" ] ),
           paste( L213_UCS_elec_cooling_by_region.refs$UCS_technology, L213_UCS_elec_cooling_by_region.refs$cooling_tech, L213_UCS_elec_cooling_by_region.refs$water_type ) ), "share" ]

# Adjust nuclear in the reference scenario once again since it may not have dry cooling
L213_UCS_elec_cooling.refs <- cast( L213_UCS_elec_cooling.refs[, c("region", "UCS_technology", "fuel", "cooling_tech", "share")], region + UCS_technology + fuel ~ cooling_tech )
L213_UCS_cooling_nuclear = L213_UCS_elec_cooling.refs$UCS_technology == "Generic steam plant (nuclear)"
L213_UCS_elec_cooling.refs[ L213_UCS_cooling_nuclear, "Recirculating" ] <- L213_UCS_elec_cooling.refs[ L213_UCS_cooling_nuclear, "Recirculating" ] +
	L213_UCS_elec_cooling.refs[ L213_UCS_cooling_nuclear, "Dry_Cooled" ]
L213_UCS_elec_cooling.refs[ L213_UCS_cooling_nuclear, "Dry_Cooled" ] <- 0
L213_UCS_elec_cooling.refs <- melt( L213_UCS_elec_cooling.refs, variable_name="cooling_tech" )
names( L213_UCS_elec_cooling.refs )[ names( L213_UCS_elec_cooling.refs ) == "value" ] <- "share"

# IGCC and CCS technologies will not have base year shares and we only have coefficients for recirculating
# (with the exception of natural gas). Therefore we will just set the shares to 100% to recirculating
# for these technologies under the region USA.  We will copy this to all states.
L213_CC_shares <- subset( L213_UCS_elec_cooling, region == "USA" & share > 0, 
	select=names( L213_UCS_elec_cooling )[ names( L213_UCS_elec_cooling ) != "region" ] )
L213_CC_shares <- merge( data.frame( region=unique( L213_UCS_elec_cooling$region ) ), L213_CC_shares )
L213_UCS_elec_cooling <- rbind( L213_UCS_elec_cooling, L213_CC_shares )
L213_UCS_elec_cooling <- subset( L213_UCS_elec_cooling, region != "USA" )
L213_CC_shares <- subset( L213_UCS_elec_cooling.refs, region == "USA" & share > 0, 
	select=names( L213_UCS_elec_cooling.refs )[ names( L213_UCS_elec_cooling.refs ) != "region" ] )
L213_CC_shares <- merge( data.frame( region=unique( L213_UCS_elec_cooling.refs$region ) ), L213_CC_shares )
L213_UCS_elec_cooling.refs <- rbind( L213_UCS_elec_cooling.refs, L213_CC_shares )
L213_UCS_elec_cooling.refs <- subset( L213_UCS_elec_cooling.refs, region != "USA" )

# The reference scenarios are by regions map them back to states
names( L213_states_subregions ) <- c( "state", "region" )
L213_UCS_elec_cooling.refs <- merge( L213_UCS_elec_cooling.refs, L213_states_subregions )
L213_UCS_elec_cooling.refs$region <- L213_UCS_elec_cooling.refs$state
L213_UCS_elec_cooling.refs$state <- NULL

# Create a dry cooled scenario where dry regions use a certain percent dry cooling
L213_dry_states <- c( "NV", "TX", "UT", "CA", "AZ", "NM" )
L213_target_dry_share <- 0.30
L213_UCS_elec_cooling.dry <- L213_UCS_elec_cooling.refs
#L213_UCS_elec_cooling.dry$change <- 0
#L213_UCS_elec_cooling.dry <- cast( L213_UCS_elec_cooling.dry[, c("region", "UCS_technology", "fuel", "cooling_tech", "share")], region + UCS_technology + fuel ~ cooling_tech )
L213_UCS_elec_cooling.dry <- cast( L213_UCS_elec_cooling.dry[, c("region", "UCS_technology", "fuel", "cooling_tech", "share")], region + UCS_technology + fuel ~ cooling_tech, fun.aggregate=sum )
L213_dry_adj_techs <- L213_UCS_elec_cooling.dry$region %in% L213_dry_states & L213_UCS_elec_cooling.dry$fuel != "Nuclear" &
	L213_UCS_elec_cooling.dry$No_Cooling_Needed != 1 & !grepl( "Combustion [Tt]urbine", L213_UCS_elec_cooling.dry$UCS_technology ) &
	L213_UCS_elec_cooling.dry$UCS_technology %!in% c( "PV", "Wind", "CSP Trough", "Hydropower", "Pumped Storage" ) &
	!grepl( "Geothermal", L213_UCS_elec_cooling.dry$UCS_technology )
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ] <- pmax( L213_target_dry_share - L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ], 0 )
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ] <-L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ] +  L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ]
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Recirculating" ] <-L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Recirculating" ] -  L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ]
L213_UCS_elec_cooling.dry[ L213_UCS_elec_cooling.dry$Recirculating < 0, "Once_Through" ] <-L213_UCS_elec_cooling.dry[ L213_UCS_elec_cooling.dry$Recirculating < 0, "Once_Through" ] + L213_UCS_elec_cooling.dry[ L213_UCS_elec_cooling.dry$Recirculating < 0, "Recirculating" ]
L213_UCS_elec_cooling.dry[ L213_UCS_elec_cooling.dry$Recirculating < 0, "Recirculating" ] <- 0
# Need to special case geothermal since it is really done by fuel
# We will just adjust the dominante geothermal technology
# EGS is the dominate technology in all regions except NV and UT where it is binary
L213_dry_adj_techs <- L213_UCS_elec_cooling.dry$UCS_technology == "Geothermal EGS" & L213_UCS_elec_cooling.dry$region %in% L213_dry_states & 
	L213_UCS_elec_cooling.dry$region %!in% c( "NV", "UT" )
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ] <- pmax( L213_target_dry_share - L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ], 0 )
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ] <-L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ] +  L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ]
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Recirculating" ] <-L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Recirculating" ] -  L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ]
L213_dry_adj_techs <- L213_UCS_elec_cooling.dry$UCS_technology == "Geothermal Binary" & L213_UCS_elec_cooling.dry$region %in% c( "NV", "UT" )
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ] <- pmax( L213_target_dry_share - L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ], 0 )
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ] <-L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Dry_Cooled" ] +  L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ]
L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Recirculating" ] <-L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "Recirculating" ] -  L213_UCS_elec_cooling.dry[ L213_dry_adj_techs, "change" ]
L213_UCS_elec_cooling.dry <- melt( L213_UCS_elec_cooling.dry, variable_name="cooling_tech" )
names( L213_UCS_elec_cooling.dry )[ names( L213_UCS_elec_cooling.dry ) == "value" ] <- "share"

L213_cooling_techs <- c( "Once_Through", "Cooling_Pond", "Recirculating", "No_Cooling_Needed" )
L213_cooling_techs <- c( L213_cooling_techs, paste( L213_cooling_techs, "saline" ) )

# To keep the structure in the model simple we will set the actual coefficients for water use
# as the share of each cooling technology * the actual cooling technology water use coefficient
# the draw back is it is not apparent at first glance what the coefficient means however the
# benefit is the correct amount of water demanded is reported with in the technology
printlog( "Calculate the base share weighted water use coefficients." )

L213_elec_water_coefs <- melt( elec_water_coefficients, variable_name="cooling_tech" )
names( L213_elec_water_coefs )[ length( names( L213_elec_water_coefs ) ) ] <- "unweighted_coef"
L213_elec_water_coefs <- subset( L213_elec_water_coefs, cooling_tech %in% L213_cooling_techs )
L213_elec_water_coefs_saline <- L213_elec_water_coefs
L213_elec_water_coefs_saline$cooling_tech <- paste( L213_elec_water_coefs$cooling_tech, "saline" )
L213_elec_water_coefs <- rbind( L213_elec_water_coefs, L213_elec_water_coefs_saline )

L213_elec_water_coefs_FA <- melt( elec_water_coefficients_FA, variable_name="period" )
names( L213_elec_water_coefs_FA )[ length( names( L213_elec_water_coefs_FA ) ) ] <- "unweighted_coef"
L213_elec_water_coefs_FA <- subset( L213_elec_water_coefs_FA, cooling_tech %in% L213_cooling_techs )
L213_elec_water_coefs_FA_saline <- L213_elec_water_coefs_FA
L213_elec_water_coefs_FA_saline$cooling_tech <- paste( L213_elec_water_coefs_FA$cooling_tech, "saline" )
L213_elec_water_coefs_FA <- rbind( L213_elec_water_coefs_FA, L213_elec_water_coefs_FA_saline )
L213_elec_water_coefs_FA$period <- as.integer( sub( 'X', '', L213_elec_water_coefs_FA$period ) )

# The shares in the base year are by fuel since that is how the generation technologies will be represented
# TODO: some way to make sure the missing technologies are consistent with energy calibration data
L213_UCS_elec_cooling_by_fuel <- subset( L213_UCS_elec_cooling_by_fuel, cooling_tech %in% L213_cooling_techs & !is.na( share ) & share > 0 )
#L213_elec_coolingwater_coefs_by_fuel <- merge( L213_UCS_elec_cooling_by_fuel, L213_elec_water_coefs.fuel, all.x=TRUE )
L213_elec_coolingwater_coefs_by_fuel <- merge( L213_UCS_elec_cooling_by_fuel, L213_elec_water_coefs, all.x=TRUE )
L213_elec_coolingwater_coefs_by_fuel$coefficient <- L213_elec_coolingwater_coefs_by_fuel$share * L213_elec_coolingwater_coefs_by_fuel$unweighted_coef
stopifnot( nrow( subset( L213_elec_coolingwater_coefs_by_fuel, is.na( coefficient ) ) ) == 0 )
L213_elec_coolingwater_coefs_by_fuel <- subset( L213_elec_coolingwater_coefs_by_fuel, coefficient > 0 )

# Also calculate the coefficients by technology for future technologies
L213_UCS_elec_cooling <- subset( L213_UCS_elec_cooling, cooling_tech %in% L213_cooling_techs & share > 0 )
L213_elec_coolingwater_coefs <- merge( L213_UCS_elec_cooling, L213_elec_water_coefs, all.x=TRUE )
L213_elec_coolingwater_coefs$coefficient <- L213_elec_coolingwater_coefs$share * L213_elec_coolingwater_coefs$unweighted_coef
stopifnot( nrow( subset( L213_elec_coolingwater_coefs, is.na( coefficient ) ) ) == 0 )
L213_elec_coolingwater_coefs <- subset( L213_elec_coolingwater_coefs, coefficient > 0 )
L213_elec_coolingwater_coefs.geo <- subset( L213_elec_coolingwater_coefs, grepl( "Geothermal", L213_elec_coolingwater_coefs$UCS_technology ) )
L213_elec_coolingwater_coefs.geo$UCS_technology <- "Geothermal Binary"
L213_elec_coolingwater_coefs.geo <- aggregate( coefficient ~ region + fuel + UCS_technology + cooling_tech + withdrawal_type,
    L213_elec_coolingwater_coefs.geo, FUN=sum )
L213_elec_coolingwater_coefs <- subset( L213_elec_coolingwater_coefs, !grepl( "Geothermal", L213_elec_coolingwater_coefs$UCS_technology ) )
L213_elec_coolingwater_coefs <- rbind( L213_elec_coolingwater_coefs[, names( L213_elec_coolingwater_coefs.geo ) ], L213_elec_coolingwater_coefs.geo )

# Also calculate the coefficients by technology for future technologies in the reference scenario
L213_UCS_elec_cooling.refs <- subset( L213_UCS_elec_cooling.refs, cooling_tech %in% L213_cooling_techs & share > 0 )
L213_elec_coolingwater_coefs.refs <- merge( L213_UCS_elec_cooling.refs, L213_elec_water_coefs, all.x=TRUE )
L213_elec_coolingwater_coefs.refs$coefficient <- L213_elec_coolingwater_coefs.refs$share * L213_elec_coolingwater_coefs.refs$unweighted_coef
stopifnot( nrow( subset( L213_elec_coolingwater_coefs.refs, is.na( coefficient ) ) ) == 0 )
L213_elec_coolingwater_coefs.refs <- subset( L213_elec_coolingwater_coefs.refs, coefficient > 0 )
L213_elec_coolingwater_coefs.geo <- subset( L213_elec_coolingwater_coefs.refs, grepl( "Geothermal", L213_elec_coolingwater_coefs.refs$UCS_technology ) )
L213_elec_coolingwater_coefs.geo$UCS_technology <- "Geothermal Binary"
L213_elec_coolingwater_coefs.geo <- aggregate( coefficient ~ region + fuel + UCS_technology + cooling_tech + withdrawal_type,
    L213_elec_coolingwater_coefs.geo, FUN=sum )
L213_elec_coolingwater_coefs.geo$share <- 1
L213_elec_coolingwater_coefs.geo$unweighted_coef <- L213_elec_coolingwater_coefs.geo$coefficient
L213_elec_coolingwater_coefs.refs <- subset( L213_elec_coolingwater_coefs.refs, !grepl( "Geothermal", L213_elec_coolingwater_coefs.refs$UCS_technology ) )
L213_elec_coolingwater_coefs.refs <- rbind( L213_elec_coolingwater_coefs.refs[, names( L213_elec_coolingwater_coefs.geo ) ], L213_elec_coolingwater_coefs.geo )

# Calculate coefficients by technology in the reference scenario with advanced water saving technologies
L213_UCS_elec_cooling.refs_FA <- subset( L213_UCS_elec_cooling.refs, cooling_tech %in% L213_cooling_techs & share > 0 )
L213_elec_coolingwater_coefs.refs_FA <- merge( L213_UCS_elec_cooling.refs_FA, L213_elec_water_coefs_FA, all.x=TRUE )
L213_elec_coolingwater_coefs.refs_FA$coefficient <- L213_elec_coolingwater_coefs.refs_FA$share * L213_elec_coolingwater_coefs.refs_FA$unweighted_coef
stopifnot( nrow( subset( L213_elec_coolingwater_coefs.refs_FA, is.na( coefficient ) ) ) == 0 )
L213_elec_coolingwater_coefs.refs_FA <- subset( L213_elec_coolingwater_coefs.refs_FA, coefficient > 0 )
L213_elec_coolingwater_coefs.geo <- subset( L213_elec_coolingwater_coefs.refs_FA, grepl( "Geothermal", L213_elec_coolingwater_coefs.refs_FA$UCS_technology ) )
L213_elec_coolingwater_coefs.geo$UCS_technology <- "Geothermal Binary"
L213_elec_coolingwater_coefs.geo <- aggregate( coefficient ~ region + fuel + UCS_technology + cooling_tech + withdrawal_type + period,
    L213_elec_coolingwater_coefs.geo, FUN=sum )
L213_elec_coolingwater_coefs.refs_FA <- subset( L213_elec_coolingwater_coefs.refs_FA, !grepl( "Geothermal", L213_elec_coolingwater_coefs.refs_FA$UCS_technology ) )
L213_elec_coolingwater_coefs.refs_FA <- rbind( L213_elec_coolingwater_coefs.refs_FA[, names( L213_elec_coolingwater_coefs.geo ) ], L213_elec_coolingwater_coefs.geo )

# Calculate the coefficients by technology for future technologies in the reference scenario with optimistic dry cooling
L213_UCS_elec_cooling.dry <- subset( L213_UCS_elec_cooling.dry, cooling_tech %in% L213_cooling_techs & share > 0 )
L213_elec_coolingwater_coefs.dry <- merge( L213_UCS_elec_cooling.dry, L213_elec_water_coefs_FA, all.x=TRUE )
L213_elec_coolingwater_coefs.dry$coefficient <- L213_elec_coolingwater_coefs.dry$share * L213_elec_coolingwater_coefs.dry$unweighted_coef
stopifnot( nrow( subset( L213_elec_coolingwater_coefs.dry, is.na( coefficient ) ) ) == 0 )
L213_elec_coolingwater_coefs.dry <- subset( L213_elec_coolingwater_coefs.dry, coefficient > 0 )
L213_elec_coolingwater_coefs.geo <- subset( L213_elec_coolingwater_coefs.dry, grepl( "Geothermal", L213_elec_coolingwater_coefs.dry$UCS_technology ) )
L213_elec_coolingwater_coefs.geo$UCS_technology <- "Geothermal Binary"
L213_elec_coolingwater_coefs.geo <- aggregate( coefficient ~ region + fuel + UCS_technology + cooling_tech + withdrawal_type + period,
    L213_elec_coolingwater_coefs.geo, FUN=sum )
L213_elec_coolingwater_coefs.dry <- subset( L213_elec_coolingwater_coefs.dry, !grepl( "Geothermal", L213_elec_coolingwater_coefs.dry$UCS_technology ) )
L213_elec_coolingwater_coefs.dry <- rbind( L213_elec_coolingwater_coefs.dry[, names( L213_elec_coolingwater_coefs.geo ) ], L213_elec_coolingwater_coefs.geo )

# Map in GCAM technology names
#test <- merge( test, UCS_fuel_names )
#test <- aggregate( share ~ region + technology + cooling_tech, test, FUN=sum )
L213_elec_coolingwater_coefs_by_fuel <- merge( L213_elec_coolingwater_coefs_by_fuel, UCS_fuel_names )
L213_elec_coolingwater_coefs_by_fuel <- aggregate( coefficient ~ region + technology + cooling_tech + withdrawal_type,
    L213_elec_coolingwater_coefs_by_fuel, FUN=sum )
L213_elec_coolingwater_coefs <- merge( L213_elec_coolingwater_coefs, UCS_tech_names )
L213_elec_coolingwater_coefs.refs <- merge( L213_elec_coolingwater_coefs.refs, UCS_tech_names )
L213_elec_coolingwater_coefs.refs_FA <- merge( L213_elec_coolingwater_coefs.refs_FA, UCS_tech_names )
L213_elec_coolingwater_coefs.dry <- merge( L213_elec_coolingwater_coefs.dry, UCS_tech_names )

# It might be easier to wait to merge the base and future if we have to compute different future share
# scenarios than fixed.
#L213_elec_coolingwater_coefs <- rbind( L213_elec_coolingwater_coefs[, names( L213_elec_coolingwater_coefs_by_fuel ) ],
    #L213_elec_coolingwater_coefs_by_fuel )
#L213_elec_coolingwater_coefs.refs <- rbind( L213_elec_coolingwater_coefs.refs[, names( L213_elec_coolingwater_coefs_by_fuel ) ],
    #L213_elec_coolingwater_coefs_by_fuel )

names(L155_state_elec_supply)[1] <- "region"
#test <- merge(test, subset(L155_state_elec_supply, gen1990 > 0 | gen2005 > 0, select=c("region", "technology", "gen1990", "gen2005")), all=TRUE)

L213_invalid_hydro_coefs <- subset( L155_state_elec_supply, GCAM_fuel == "hydro" & (gen1990 > 0 & gen2005 == 0), select=c( "region", "technology" ) )
L213_tech_input <- subset( A_elecS_tech_input, grepl( "hydro", technology ) )
L213_hydro_years <- c( GCAM_base_years, GCAM_future_years )
L213_tech_input <- L213_tech_input[ sort( rep( 1:nrow( L213_tech_input ), times = length( L213_hydro_years ) ) ), ]
L213_tech_input$period <- L213_hydro_years
L213_tech_input <- rbind ( subset( A_elecS_tech_input, !grepl( "hydro", technology ) ), L213_tech_input )

L213_ElecCoolingInput.baseyear <- data.frame( region=L213_elec_coolingwater_coefs_by_fuel$region,
                                           technology=L213_elec_coolingwater_coefs_by_fuel$technology,
                                           minicam_energy_input=paste( L213_elec_coolingwater_coefs_by_fuel$cooling_tech, L213_elec_coolingwater_coefs_by_fuel$withdrawal_type ),
                                           coefficient=L213_elec_coolingwater_coefs_by_fuel$coefficient )
L213_ElecCoolingInput.baseyear <- merge( L213_ElecCoolingInput.baseyear, L213_tech_input[, c( "supplysector", "subsector", "technology", "period" ) ] )
L213_ElecCoolingInput.baseyear <- subset( L213_ElecCoolingInput.baseyear, period < 2020 )

# Compute the base scenario where we hold the base year shares of cooling technology constant
# for the entire scenario
printlog( "L213_ElecCoolingInput_Fixed: Create water inputs for electricity where base year shares are held constant." )
L213_ElecCoolingInput_Fixed <- data.frame( region=L213_elec_coolingwater_coefs$region,
                                           technology=L213_elec_coolingwater_coefs$technology,
                                           minicam_energy_input=paste( L213_elec_coolingwater_coefs$cooling_tech, L213_elec_coolingwater_coefs$withdrawal_type ),
                                           coefficient=L213_elec_coolingwater_coefs$coefficient )

L213_ElecCoolingInput_Fixed <- merge( L213_ElecCoolingInput_Fixed, L213_tech_input[, c( "supplysector", "subsector", "technology", "period" ) ] )
L213_ElecCoolingInput_Fixed <- L213_ElecCoolingInput_Fixed[ paste( L213_ElecCoolingInput_Fixed$technology, L213_ElecCoolingInput_Fixed$period ) %!in%
	paste( L213_ElecCoolingInput.baseyear$technology, L213_ElecCoolingInput.baseyear$period ), ]
L213_ElecCoolingInput_Fixed <- rbind( L213_ElecCoolingInput_Fixed, L213_ElecCoolingInput.baseyear )
L213_ElecCoolingInput_Fixed$market <- "USA"
L213_ElecCoolingInput_Fixed <- subset( L213_ElecCoolingInput_Fixed, period > 0 )
# Reorder names to match header
L213_ElecCoolingInput_Fixed <- L213_ElecCoolingInput_Fixed[, c( "region", "supplysector", "subsector", "technology", "period",
    "minicam_energy_input", "coefficient", "market" ) ]
L213_ElecCoolingInput_Fixed <- L213_ElecCoolingInput_Fixed[ paste( L213_ElecCoolingInput_Fixed$region, L213_ElecCoolingInput_Fixed$technology) %!in%
                                                            paste( L213_invalid_hydro_coefs$region, L213_invalid_hydro_coefs$technology ) |
															L213_ElecCoolingInput_Fixed$period < 2005, ]

# Compute the reference scenario where we hold the recent shares of cooling technology constant
# for the entire scenario
printlog( "L213_ElecCoolingInput_Refs: Create water inputs for electricity where recent shares are held constant." )
L213_ElecCoolingInput_Refs <- data.frame( region=L213_elec_coolingwater_coefs.refs$region,
                                           technology=L213_elec_coolingwater_coefs.refs$technology,
                                           minicam_energy_input=paste( L213_elec_coolingwater_coefs.refs$cooling_tech, L213_elec_coolingwater_coefs.refs$withdrawal_type ),
                                           coefficient=L213_elec_coolingwater_coefs.refs$coefficient )
L213_ElecCoolingInput_Refs <- merge( L213_ElecCoolingInput_Refs, L213_tech_input[, c( "supplysector", "subsector", "technology", "period" ) ] )
L213_ElecCoolingInput_Refs <- L213_ElecCoolingInput_Refs[ paste( L213_ElecCoolingInput_Refs$technology, L213_ElecCoolingInput_Refs$period ) %!in%
	paste( L213_ElecCoolingInput.baseyear$technology, L213_ElecCoolingInput.baseyear$period ), ]
L213_ElecCoolingInput_Refs <- rbind( L213_ElecCoolingInput_Refs, L213_ElecCoolingInput.baseyear )
L213_ElecCoolingInput_Refs$market <- "USA"
L213_ElecCoolingInput_Refs <- subset( L213_ElecCoolingInput_Refs, period > 0 )
# Reorder names to match header
L213_ElecCoolingInput_Refs <- L213_ElecCoolingInput_Refs[, c( "region", "supplysector", "subsector", "technology", "period",
    "minicam_energy_input", "coefficient", "market" ) ]
L213_ElecCoolingInput_Refs <- L213_ElecCoolingInput_Refs[ paste( L213_ElecCoolingInput_Refs$region, L213_ElecCoolingInput_Refs$technology) %!in%
                                                          paste( L213_invalid_hydro_coefs$region, L213_invalid_hydro_coefs$technology ) |
														  L213_ElecCoolingInput_Refs$period < 2005, ]

# Compute the reference scenario plus advanced water saving technologies
printlog( "L213_ElecCoolingInput_Refs: Create water inputs for electricity where recent shares are held constant." )
L213_ElecCoolingInput_Refs_FA <- data.frame( region=L213_elec_coolingwater_coefs.refs_FA$region,
                                           technology=L213_elec_coolingwater_coefs.refs_FA$technology,
                                           minicam_energy_input=paste( L213_elec_coolingwater_coefs.refs_FA$cooling_tech, L213_elec_coolingwater_coefs.refs_FA$withdrawal_type ),
                                           coefficient=L213_elec_coolingwater_coefs.refs_FA$coefficient,
										   period=L213_elec_coolingwater_coefs.refs_FA$period )
L213_ElecCoolingInput_Refs_FA <- merge( L213_ElecCoolingInput_Refs_FA, L213_tech_input[, c( "supplysector", "subsector", "technology", "period" ) ] )
L213_ElecCoolingInput_Refs_FA <- L213_ElecCoolingInput_Refs_FA[ paste( L213_ElecCoolingInput_Refs_FA$technology, L213_ElecCoolingInput_Refs_FA$period ) %!in%
	paste( L213_ElecCoolingInput.baseyear$technology, L213_ElecCoolingInput.baseyear$period ), ]
L213_ElecCoolingInput_Refs_FA <- rbind( L213_ElecCoolingInput_Refs_FA, L213_ElecCoolingInput.baseyear )
L213_ElecCoolingInput_Refs_FA$market <- "USA"
L213_ElecCoolingInput_Refs_FA <- subset( L213_ElecCoolingInput_Refs_FA, period > 0 )
# Reorder names to match header
L213_ElecCoolingInput_Refs_FA <- L213_ElecCoolingInput_Refs_FA[, c( "region", "supplysector", "subsector", "technology", "period",
    "minicam_energy_input", "coefficient", "market" ) ]
L213_ElecCoolingInput_Refs_FA <- L213_ElecCoolingInput_Refs_FA[ paste( L213_ElecCoolingInput_Refs_FA$region, L213_ElecCoolingInput_Refs_FA$technology) %!in%
                                                          paste( L213_invalid_hydro_coefs$region, L213_invalid_hydro_coefs$technology ) |
														  L213_ElecCoolingInput_Refs_FA$period < 2005, ]

# Compute the reference scenario plus advanced water saving technologies and change the share of dry cooling in dry states
printlog( "L213_ElecCoolingInput_Dry: Create water inputs for electricity where recent shares are held constant." )
L213_ElecCoolingInput_Dry <- data.frame( region=L213_elec_coolingwater_coefs.dry$region,
                                           technology=L213_elec_coolingwater_coefs.dry$technology,
                                           minicam_energy_input=paste( L213_elec_coolingwater_coefs.dry$cooling_tech, L213_elec_coolingwater_coefs.dry$withdrawal_type ),
                                           coefficient=L213_elec_coolingwater_coefs.dry$coefficient,
										   period=L213_elec_coolingwater_coefs.dry$period )
L213_ElecCoolingInput_Dry <- merge( L213_ElecCoolingInput_Dry, L213_tech_input[, c( "supplysector", "subsector", "technology", "period" ) ] )
L213_ElecCoolingInput_Dry <- L213_ElecCoolingInput_Dry[ paste( L213_ElecCoolingInput_Dry$technology, L213_ElecCoolingInput_Dry$period ) %!in%
	paste( L213_ElecCoolingInput.baseyear$technology, L213_ElecCoolingInput.baseyear$period ), ]
L213_ElecCoolingInput_Dry <- rbind( L213_ElecCoolingInput_Dry, L213_ElecCoolingInput.baseyear )
L213_ElecCoolingInput_Dry$market <- "USA"
L213_ElecCoolingInput_Dry <- subset( L213_ElecCoolingInput_Dry, period > 0 )
# Reorder names to match header
L213_ElecCoolingInput_Dry <- L213_ElecCoolingInput_Dry[, c( "region", "supplysector", "subsector", "technology", "period",
    "minicam_energy_input", "coefficient", "market" ) ]
L213_ElecCoolingInput_Dry <- L213_ElecCoolingInput_Dry[ paste( L213_ElecCoolingInput_Dry$region, L213_ElecCoolingInput_Dry$technology) %!in%
                                                          paste( L213_invalid_hydro_coefs$region, L213_invalid_hydro_coefs$technology ) |
														  L213_ElecCoolingInput_Dry$period < 2005, ]
L213_ElecCoolingInput_Dry <- subset( L213_ElecCoolingInput_Dry, period >= 2020 )
L213_ElecCoolingInput_Dry <- rbind( L213_ElecCoolingInput_Dry, subset( L213_ElecCoolingInput_Refs, period < 2020 ) )

# Create a pass through for each cooling tech+withdrawal/consumption to just withdrawal/consumption
printlog( "Water cooling tech pass throughs to water withdrawal_type" )
L213_Sector <- data.frame( region="USA", supplysector=unique( L213_ElecCoolingInput_Fixed$minicam_energy_input ),
    input_unit="km^3", output_unit="km^3", price_unit="1975$/m^3", logit=-3 )

L213_Subsector <- L213_Sector[, c( "region", "supplysector" ) ]
L213_Subsector$subsector <- L213_Subsector$supplysector
L213_Subsector[, paste( "sw", c( GCAM_model_period0, GCAM_base_years ), sep="_" ) ] <- 1
L213_Subsector$from_year <- final_cal_year
L213_Subsector$to_year <- 9999
L213_Subsector$rule <- "fixed"
L213_Subsector$apply_to <- "share-weight"

L213_Tech <- L213_Subsector[, c( "region", "supplysector", "subsector" ) ]
L213_Tech$technology <- L213_Tech$subsector
L213_Tech$period <- GCAM_model_period0
L213_Tech$share_weight <- 1
L213_withdrawal_type <- unique( elec_water_coefficients$withdrawal_type )
L213_Tech$minicam_energy_input <- ifelse( grepl( L213_withdrawal_type[1], L213_Tech$technology ), L213_withdrawal_type[1], L213_withdrawal_type[2] )
L213_Tech$calibrated_value <- -1
L213_Tech$efficiency <- 1
L213_Tech$market <- " "
# Need and extra column to ensure the blank market does not get wipped out
L213_Tech$extra <- "Junk"

#Write out some extra info for the PRIMA SITE group
Table1_CoolingSystemShares.melt <- subset(L213_elec_coolingwater_coefs.refs, withdrawal_type == "water consumption" & !grepl( "No_Cooling", cooling_tech ) )
Table1_CoolingSystemShares <- cast( Table1_CoolingSystemShares.melt, region + technology ~ cooling_tech, value = "share" )
Table1_CoolingSystemShares[ is.na( Table1_CoolingSystemShares ) ] <- 0
CoolingSystemTypes <- sort( unique( Table1_CoolingSystemShares.melt$cooling_tech ) )
Table1_CoolingSystemShares$DryCooling <- 1 - rowSums( Table1_CoolingSystemShares[ CoolingSystemTypes ] )
Table1_CoolingSystemShares[ names( Table1_CoolingSystemShares ) %in% CoolingSystemTypes ] <- Table1_CoolingSystemShares[ CoolingSystemTypes ]

#Drop the techs that aren't actually in the model
nonexistent_techs <- c( "geo_int", "geo_subpeak", "geo_peak", "nuc_int_gen2", "nuc_int_gen3", "nuc_subpeak_gen2", "nuc_subpeak_gen3", "nuc_peak_gen2", "nuc_peak_gen3" )
Table1_CoolingSystemShares <- subset( Table1_CoolingSystemShares, !technology %in% nonexistent_techs )

Table2_ElecTechCoefs <- merge( elec_water_coefficients, UCS_tech_names )
Table2_ElecTechCoefs <- Table2_ElecTechCoefs[ order( Table2_ElecTechCoefs$technology ),
      c( "technology", "withdrawal_type", names( Table2_ElecTechCoefs )[ names( Table2_ElecTechCoefs ) %in% CoolingSystemTypes ] ) ]
Table2_ElecTechCoefs.melt <- melt( Table2_ElecTechCoefs, id.vars = c( "technology", "withdrawal_type" ), variable_name = "cooling_system" )
Table2_ElecTechCoefs <- cast( Table2_ElecTechCoefs.melt, technology + cooling_system ~ withdrawal_type )
Table2_ElecTechCoefs <- na.omit( Table2_ElecTechCoefs )
Table2_ElecTechCoefs <- subset( Table2_ElecTechCoefs, !technology %in% nonexistent_techs )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L213_ElecCoolingInput_Fixed, "WaterInputs", "L213_ElecCoolingInput_Fixed", "batch_rgcam_elec_water_input_fixed.xml" ) 
write_mi_data( L213_Sector, "Sector", "L213_Sector", "batch_rgcam_elec_water_input_fixed.xml" ) 
write_mi_data( L213_Subsector, "ElecSubsInterpRule", "L213_Subsector", "batch_rgcam_elec_water_input_fixed.xml" ) 
write_mi_data( L213_Tech, "ElecSubgregionalCal", "L213_Tech", "batch_rgcam_elec_water_input_fixed.xml" ) 

write_mi_data( L213_ElecCoolingInput_Refs, "WaterInputs", "L213_ElecCoolingInput_Refs", "batch_rgcam_elec_water_input_ref.xml" ) 
write_mi_data( L213_Sector, "Sector", "L213_Sector", "batch_rgcam_elec_water_input_ref.xml" ) 
write_mi_data( L213_Subsector, "ElecSubsInterpRule", "L213_Subsector", "batch_rgcam_elec_water_input_ref.xml" ) 
write_mi_data( L213_Tech, "ElecSubgregionalCal", "L213_Tech", "batch_rgcam_elec_water_input_ref.xml" ) 

write_mi_data( L213_ElecCoolingInput_Refs_FA, "WaterInputs", "L213_ElecCoolingInput_Refs_FA", "batch_rgcam_elec_water_input_ref_FA.xml" ) 
write_mi_data( L213_Sector, "Sector", "L213_Sector", "batch_rgcam_elec_water_input_ref_FA.xml" ) 
write_mi_data( L213_Subsector, "ElecSubsInterpRule", "L213_Subsector", "batch_rgcam_elec_water_input_ref_FA.xml" ) 
write_mi_data( L213_Tech, "ElecSubgregionalCal", "L213_Tech", "batch_rgcam_elec_water_input_ref_FA.xml" ) 

write_mi_data( L213_ElecCoolingInput_Dry, "WaterInputs", "L213_ElecCoolingInput_Dry", "batch_rgcam_elec_water_input_dry.xml" ) 
write_mi_data( L213_Sector, "Sector", "L213_Sector", "batch_rgcam_elec_water_input_dry.xml" ) 
write_mi_data( L213_Subsector, "ElecSubsInterpRule", "L213_Subsector", "batch_rgcam_elec_water_input_dry.xml" ) 
write_mi_data( L213_Tech, "ElecSubgregionalCal", "L213_Tech", "batch_rgcam_elec_water_input_dry.xml" ) 

insert_file_into_batchxml( "batch_rgcam_elec_water_input_fixed.xml", "rgcam_elec_water_input_fixed.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "batch_rgcam_elec_water_input_ref.xml", "rgcam_elec_water_input_ref.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "batch_rgcam_elec_water_input_ref_FA.xml", "rgcam_elec_water_input_ref_FA.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "batch_rgcam_elec_water_input_dry.xml", "rgcam_elec_water_input_dry.xml", "", xml_tag="outFile" )

comments.Table1_CoolingSystemShares <- c( "Cooling system shares by state and technology", "Unit = unitless share" )
comments.Table2_ElecTechCoefs <- c( "Water demand coefficients by technology and cooling system type", "Unit = m3/GJ" )
writedata( Table1_CoolingSystemShares, "Table1_CoolingSystemShares", comments = comments.Table1_CoolingSystemShares )
writedata( Table2_ElecTechCoefs, "Table2_ElecTechCoefs", comments = comments.Table2_ElecTechCoefs )

logstop()
