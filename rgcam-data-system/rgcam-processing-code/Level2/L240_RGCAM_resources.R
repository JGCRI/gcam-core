# L240_RGCAM_resources.R
# Generates state level resources including solar, wind, geothermal, CCS
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L240_RGCAM_resources.R" )
printlog( "RGCAM resources input file" )

# -----------------------------------------------------------------------------
# 1. Read files

A_elecS_tech_input <- readdata( "A_elecS_tech_input" )
state_region_ind <- readdata( "state_region_ind" )

NREL_us_re_capacity_factors <- readdata( "NREL_us_re_capacity_factors" )
NREL_us_re_technical_potential <- readdata( "NREL_us_re_technical_potential" )

us_state_wind <- readdata( "us_state_wind" )

A_res_delete <- readdata( "A_res_delete" )
A_res_names <- readdata( "A_res_names" )
A_res_other <- readdata( "A_res_other" )
A_res_techChange <- readdata( "A_res_techChange" )
A_res_renew_costs <- readdata( "A_res_renew_costs" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
# Central solar processing
# We will use NREL capacity factors to adjust state level central solar production
printlog( "L240_SolarCF: SOLAR CAPACITY FACTORS" )
L240_capacity_factors <- merge( state_region_ind[,c( "state", "state_name" ) ], NREL_us_re_capacity_factors,
    by.x="state_name", by.y="State" )
# Compute a scalar indicating how far the state is from the average.  This will be used
# for adjusting storage capacity factors
L240_capacity_factors$pv_scalar <- L240_capacity_factors$Urban_Utility_scale_PV /
    NREL_us_re_capacity_factors[ NREL_us_re_capacity_factors$State == "Average", "Urban_Utility_scale_PV" ]
L240_capacity_factors$csp_scalar <- L240_capacity_factors$CSP /
    NREL_us_re_capacity_factors[ NREL_us_re_capacity_factors$State == "Average", "CSP" ]

L240_SolarCF <- A_elecS_tech_input[ A_elecS_tech_input$subsector == "solar" & A_elecS_tech_input$period != -1,
    c( "supplysector", "subsector", "technology", "period", "capacity_factor" ) ]
L240_SolarCF <- L240_SolarCF[ sort( rep( 1:nrow( L240_SolarCF ), times = nrow( L240_capacity_factors ) ) ), ]
L240_SolarCF <- data.frame( region=L240_capacity_factors$state, L240_SolarCF )

# First set PV capacity factors, for storage we will have to scale the original
# capacity factor to adjust for state variability
with_subsXX <- function(x,expr) eval(parse(text=gsub("XX",deparse(substitute(x),width=500),deparse(substitute(expr),width=500))))
with_subsXX( L240_SolarCF[ grep( "^pv((?!storage).)*$", L240_SolarCF$technology, perl=TRUE ), ], XX$capacity_factor <<-
    L240_capacity_factors[ match( XX$region, L240_capacity_factors$state ), "Urban_Utility_scale_PV" ] )
with_subsXX( L240_SolarCF[ grep( "^pv.*storage$", L240_SolarCF$technology, perl=TRUE ), ], XX$capacity_factor <<-
    XX$capacity_factor * L240_capacity_factors[ match( XX$region, L240_capacity_factors$state ), "pv_scalar" ] )
# Now set CSP capacity factors
with_subsXX( L240_SolarCF[ grep( "^csp((?!storage).)*$", L240_SolarCF$technology, perl=TRUE ), ], XX$capacity_factor <<-
    L240_capacity_factors[ match( XX$region, L240_capacity_factors$state ), "CSP" ] )
with_subsXX( L240_SolarCF[ grep( "^csp.*storage$", L240_SolarCF$technology, perl=TRUE ), ], XX$capacity_factor <<-
    XX$capacity_factor * L240_capacity_factors[ match( XX$region, L240_capacity_factors$state ), "csp_scalar" ] )

L240_SolarCF$capacity_factor <- round( L240_SolarCF$capacity_factor, capacity_factor_digits )

# There will be some technologies that have zero capacity factor, remove those
# and delete them as technology options in the model
L240_DeleteTech <- unique( L240_SolarCF[ L240_SolarCF$capacity_factor == 0, c( "region", "supplysector",
    "subsector", "technology" ) ] )
L240_SolarCF <- L240_SolarCF[ L240_SolarCF$capacity_factor > 0, ]

# The model will need a capacity factor for O&M as well which is the same number
L240_SolarCF$capacity_factor_OM <- L240_SolarCF$capacity_factor

# Geothermal processing
printlog( "L240_DeleteResource: DELETE US LEVEL RESOURCES" )
L240_DeleteResource <- subset( A_res_delete, name == "geothermal" )

# We will use NREL technical potential for state geothermal/hydrothermal resources
printlog( "L240_GeoResource: GEOTHERMAL - HYDROTHERMAL RESOURCE" )
L240_geo_potential <- merge( state_region_ind[,c( "state", "state_name" ) ],
    NREL_us_re_technical_potential[, c( "State", "Geothermal_Hydrothermal_GWh" ) ],
    by.x="state_name", by.y="State" )

# Note NREL data is GWh electric, we need it in EJ primary resource for which we
# assume 10% conversion efficiency
L240_geo_potential <- with( L240_geo_potential, data.frame( region=state,
    max_resource_EJ=round( Geothermal_Hydrothermal_GWh * 0.0000036 * 10, CalInput_digits ) ) )

L240_DeleteTech.geo <- subset( L240_geo_potential, max_resource_EJ == 0 )
L240_geo_potential <- subset( L240_geo_potential, max_resource_EJ > 0 )
L240_GeoResource <- subset( A_res_names, resource == "geothermal" )
L240_GeoResource <- data.frame( region=L240_geo_potential$region, L240_GeoResource[ rep( 1:nrow( L240_GeoResource ), times=
    nrow( L240_geo_potential ) ), ] )
L240_GeoResource <- data.frame( L240_GeoResource, market=L240_GeoResource$region,
    pri_fuel_name=L240_GeoResource$resource, PrimaryFuelCO2Coef=0 )

printlog( "L240_GeoTechChange: GEOTHERMAL RESOURCE TECH CHANGE" )
L240_GeoTechChange <- subset( A_res_techChange, resource == "geothermal" )
L240_GeoTechChange <- data.frame( region=L240_geo_potential$region, L240_GeoTechChange[
    rep( 1:nrow( L240_GeoTechChange ), times=nrow( L240_geo_potential ) ), ] )
L240_GeoTechChange <- melt( L240_GeoTechChange, id=c( "region", "resource", "subresource" ), variable_name="year" )
L240_GeoTechChange$year <- sub( 'tc_', '', L240_GeoTechChange$year )

printlog( "L240_GeoRenewSubResGrades: GEOTHERMAL SUBRESOURCE GRADES" )
L240_GeoRenewSubResGrades <- data.frame( region=L240_geo_potential$region, A_res_renew_costs[
    sort( rep( 1:nrow( A_res_renew_costs ), times=nrow( L240_geo_potential ) ) ), ] )

printlog( "L240_GeoMaxSubResource: GEOTHERMAL MAX SUBRESOURCE" )
L240_GeoMaxSubResource <- data.frame( region=L240_geo_potential$region, unique(
    L240_GeoTechChange[, c( "resource", "subresource" ) ] ), maxSubResource=L240_geo_potential$max_resource_EJ )

printlog( "L240_DeleteTech.geo: DELETE ZERO GEOTHERMAL TECHNOLOGIES" )
L240_geo_tech_name <- unique( subset( A_elecS_tech_input, period != -1 & subsector == "geothermal",
    select=c( "supplysector", "subsector", "technology" ) ) )
L240_DeleteTech.geo <- data.frame( region=L240_DeleteTech.geo$region, L240_geo_tech_name )
L240_DeleteTech <- rbind( L240_DeleteTech, L240_DeleteTech.geo )

# TODO: just fix this in the electricity assumptions?  That would means we always run with the
# regional resource which I think we will do anyways.
printlog( "L240_ChangeMarket: CHANGE GEO TECH MARKET TO REGIONAL" )
L240_geo_tech_name <- unique( subset( A_elecS_tech_input, period != -1 & subsector == "geothermal",
    select=c( "supplysector", "subsector", "technology", "period", "minicam_energy_input" ) ) )
L240_ChangeMarket <- data.frame( region=L240_geo_potential$region, L240_geo_tech_name[
    sort( rep( 1:nrow( L240_geo_tech_name ), times=nrow( L240_geo_potential ) ) ), ],
    market=L240_geo_potential$region )

# Wind processing
# Create a resource for wind by states
printlog( "L240_WindResource: WIND RESOURCE" )
L240_WindResource <- subset( A_res_names, resource == "large onshore windresource" )
L240_WindResource <- data.frame( region=states, L240_WindResource[ rep( 1:nrow( L240_WindResource ), times=
    nrow( L240_WindResource ) ), ] )
L240_WindResource <- data.frame( L240_WindResource, market=L240_WindResource$region,
    pri_fuel_name=L240_WindResource$resource, PrimaryFuelCO2Coef=0 )

# Create a table for wind smooth renewable subresource parameters
printlog( "L240_SmoothRenewSubRes: SMOOTH RENEW SUBRESOURCE" )
L240_wind_func_params <- us_state_wind[, names( us_state_wind ) %!in% c( "base_cost" ) ]
L240_wind_func_params$mid_price <- L240_wind_func_params$mid_price * conv_2007_1975_USD
L240_SmoothRenewSubRes <- subset( A_res_other, resource == "large onshore windresource" )
L240_SmoothRenewSubRes <- L240_SmoothRenewSubRes[ rep( 1:nrow( L240_SmoothRenewSubRes ),
    times = nrow( L240_wind_func_params ) ), ]
L240_SmoothRenewSubRes <- data.frame( region=L240_wind_func_params$region, L240_SmoothRenewSubRes,
    maxSubResource=L240_wind_func_params$maxResource, mid_price=L240_wind_func_params$mid_price,
    curve_exponent=L240_wind_func_params$curve_exponent )

# TODO: just fix this in the electricity assumptions?  That would means we always run with the
# regional resource which I think we will do anyways.
# Note we are also adding an additional cost to account for base_price variation in the resource
# since I am not sure how much this base price has already been taken into account for the
# non energy costs of these technologies I am just going to add the difference from the average
printlog( "L240_ChangeMarketAddCost: CHANGE WIND TECH MARKET TO REGIONAL" )
L240_wind_tech_name <- unique( subset( A_elecS_tech_input, period != -1 & subsector == "wind",
    select=c( "supplysector", "subsector", "technology", "period", "minicam_energy_input" ) ) )
L240_ChangeMarketAddCost <- data.frame( region=us_state_wind$region, L240_wind_tech_name[
    sort( rep( 1:nrow( L240_wind_tech_name ), times=nrow( us_state_wind ) ) ), ],
    market=us_state_wind$region, base_cost_adj=us_state_wind$base_cost )
L240_ChangeMarketAddCost$base_cost_adj <- L240_ChangeMarketAddCost$base_cost_adj - mean( us_state_wind$base_cost )
L240_ChangeMarketAddCost$base_cost_adj <- L240_ChangeMarketAddCost$base_cost_adj * conv_2007_1975_USD

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L240_SolarCF, "ElecCapacityFactor", "L240_SolarCF", "batch_rgcam_resources.xml" )
write_mi_data( L240_DeleteTech, "DeleteTech", "L240_DeleteTech", "batch_rgcam_resources.xml" )
write_mi_data( L240_DeleteResource, "DeleteResource", "L240_DeleteResource", "batch_rgcam_resources.xml" )
write_mi_data( L240_GeoResource, "RenewResource", "L240_GeoResource", "batch_rgcam_resources.xml" )
write_mi_data( L240_GeoTechChange, "RenewResTechChange", "L240_GeoTechChange", "batch_rgcam_resources.xml" )
write_mi_data( L240_GeoRenewSubResGrades, "RenewSubResGrades", "L240_GeoRenewSubResGrades", "batch_rgcam_resources.xml" )
write_mi_data( L240_GeoMaxSubResource, "MaxSubResource", "L240_GeoMaxSubResource", "batch_rgcam_resources.xml" )
write_mi_data( L240_ChangeMarket, "ChangeInputMarket", "L240_ChangeMarket", "batch_rgcam_resources.xml" )
write_mi_data( L240_WindResource, "RenewResource", "L240_WindResource", "batch_rgcam_resources.xml" )
write_mi_data( L240_SmoothRenewSubRes, "SmoothRenewRes", "L240_SmoothRenewSubRes", "batch_rgcam_resources.xml" )
write_mi_data( L240_ChangeMarketAddCost, "ChangeInputMarketAddCost", "L240_ChangeMarketAddCost", "batch_rgcam_resources.xml" )

insert_file_into_batchxml( "batch_rgcam_resources.xml", "rgcam_resources.xml", "", xml_tag="outFile" )

logstop()
