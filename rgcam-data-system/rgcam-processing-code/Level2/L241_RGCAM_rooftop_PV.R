# L241_RGCAM_rooftop_PV.R
# Generates state level rooftop PV resources and technologies
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L241_RGCAM_rooftop_PV.R" )
printlog( "RGCAM rooftop PV input file" )

# -----------------------------------------------------------------------------
# 1. Read files

state_region_ind <- readdata( "state_region_ind" )

NREL_Com_PV_supply_curve <- readdata( "NREL_Com_PV_supply_curve" )
NREL_Res_PV_supply_curve <- readdata( "NREL_Res_PV_supply_curve" )
A_res_delete <- readdata( "A_res_delete" )
A_res_names <- readdata( "A_res_names" )
A_res_techChange <- readdata( "A_res_techChange" )
A_res_other <- readdata( "A_res_other" )

A_res_sector <- readdata( "A_res_sector" )
A_res_subsector <- readdata( "A_res_subsector" )
A_res_subs_interp <- readdata( "A_res_subs_interp" )
A_res_tech_input <- readdata( "A_res_tech_input" )
A_res_tech_backup <- readdata( "A_res_tech_backup" )

A_bld_technology <- readdata( "A_bld_technology" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files

# First create the resource supply curves for each region.
printlog( "L241_DeleteResource: DELETE US LEVEL RESOURCES" )
L241_DeleteResource <- subset( A_res_delete, name == "distributed_solar" )

# Create table for the resource level parameters
printlog( "L241_PVResource: RESOURCE CHARACTORISTICS" )
L241_PVResource <- subset( A_res_names, resource == "distributed_solar" )
L241_PVResource <- data.frame( region=states, L241_PVResource[ rep( 1:nrow( L241_PVResource ), times=
    nrow( L241_PVResource ) ), ] )
L241_PVResource <- data.frame( L241_PVResource, market=L241_PVResource$region,
    pri_fuel_name=L241_PVResource$resource, PrimaryFuelCO2Coef=0 )

# Create a table for technical change
printlog( "L241_PVTechChange: GEOTHERMAL RESOURCE TECH CHANGE" )
L241_PVTechChange <- subset( A_res_techChange, resource == "distributed_solar" )
L241_PVTechChange <- data.frame( region=states, L241_PVTechChange[
    rep( 1:nrow( L241_PVTechChange ), times=length( states ) ), ] )
L241_PVTechChange <- melt( L241_PVTechChange, id=c( "region", "resource", "subresource" ), variable_name="year" )
L241_PVTechChange$year <- sub( 'tc_', '', L241_PVTechChange$year )

# Define the function which computes the smooth renewable resource function:
# % of max = price ^ b / ( mid-price + price ^ b )
smooth_res_curve_approx <- function( b, midp, p ) {
    p_pow_b <- p ^ b
    f_p <- p_pow_b / ( midp ^ b + p_pow_b )
}

# Computes the smooth renewable resource function with the given parameters
# at the price points from the actual supply curve.  Then return the
# error between the computed values and the actual values
smooth_res_curve_approx_error <- function( b, midp, supply_curve ) {
    f_p <- smooth_res_curve_approx( b, midp, supply_curve$p )
    error <- f_p - supply_curve$percent_cumul
    crossprod( error, error )
}

printlog( "Compute supply curves" )
L241_pv_sc <- with( NREL_Res_PV_supply_curve, data.frame( state_name=State, p=Relative_Cost * 63.14, generation=MWh ) )
# Change Res generation to GWh to be consistent with Com
L241_pv_sc$generation <- L241_pv_sc$generation / 1000
L241_pv_sc <- with( NREL_Com_PV_supply_curve, rbind( L241_pv_sc,
    data.frame( state_name=State, p=Rel_Cost * 64.72, generation=GWH ) ) )
L241_pv_sc <- merge( state_region_ind[,c( "state", "state_name" ) ], L241_pv_sc )
L241_pv_sc <- L241_pv_sc[ order( L241_pv_sc$p ), ]
L241_pv_sc$p <- L241_pv_sc$p - min( L241_pv_sc$p )

#states <- c( "*" )
#states <- c( "*", "AR", "CA", "MD", "DC" )
L241_renew_func_params <- data.frame()
for(L241_state in states ) {
# We want prices relative to the smallest cost
if( L241_state == "*" ) {
L241_pv_sc_State <- L241_pv_sc
} else {
L241_pv_sc_State <- L241_pv_sc[ L241_pv_sc$state == L241_state, ]
}
L241_pv_sc_State <- rbind( L241_pv_sc_State[ 1, ], L241_pv_sc_State )
L241_pv_sc_State[ 1, c( "p", "generation" ) ] <- cbind( 0, 0 )
#L241_base_price <- min( L241_pv_sc_State$p )
#L241_pv_sc_State$p <- L241_pv_sc_State$p - L241_base_price
L241_pv_sc_State$cumul <- 0
for (i in 1:nrow(L241_pv_sc_State)) {
    if (i == 1) {
        L241_pv_sc_State[ i, "cumul" ] <- L241_pv_sc_State[ i, "generation" ]
    } else {
        L241_pv_sc_State[ i, "cumul" ] <- L241_pv_sc_State[ i-1, "cumul" ] + L241_pv_sc_State[ i, "generation" ]
    }
}
L241_pv_sc_State$percent_cumul <- L241_pv_sc_State$cumul / sum( L241_pv_sc_State$generation )
# Find the mid price.  We do this by taking the first % that is greater
# than or equal to %50 which biases us slightly to the high side
# with(L241_pv_sc_State,which(abs(percent_cumul-0.5)==min(abs(percent_cumul-0.5)))
L241_mid_p <- L241_pv_sc_State[ L241_pv_sc_State$percent_cumul >= 0.5, "p" ][1]

#L241_error_min_b <- nlm( smooth_res_curve_approx_error, 4.24, L241_mid_p, L241_pv_sc_State )
# A return code greater than 2 indicates a failure to solve
#stopifnot( L241_error_min_b$code <= 2 )
L241_error_min_b <- optimize(f=smooth_res_curve_approx_error, interval=c(1.0,15.0), L241_mid_p, L241_pv_sc_State )
L241_renew_func_params <- rbind( L241_renew_func_params,
    data.frame( state=L241_state, mid_p=L241_mid_p, b_exp=L241_error_min_b$minimum ) )

#L241_plot <- rbind( data.frame( p=L241_pv_sc_State$p, fract=L241_pv_sc_State$percent_cumul, type="data" ),
#                    data.frame( p=L241_pv_sc_State$p, fract=smooth_res_curve_approx( 4.24, L241_mid_p, L241_pv_sc_State$p ), type="guess" ),
#                    data.frame( p=L241_pv_sc_State$p, fract=smooth_res_curve_approx( L241_error_min_b$minimum, L241_mid_p, L241_pv_sc_State$p ), type="solved" ) )
#print(
#ggplot() + layer(geom="point", data=L241_plot[L241_plot$type == "data",],mapping=aes(x=fract,y=p),color="black") +
#    layer(geom="line", data=L241_plot[L241_plot$type == "guess",],mapping=aes(x=fract,y=p),color="green") +
#    layer(geom="line", data=L241_plot[L241_plot$type == "solved",],mapping=aes(x=fract,y=p),color="red")
#)
#readline( prompt="Check it out" )
}

L241_max_gen <- aggregate( generation ~ state, data=L241_pv_sc, FUN=sum )
# convert to from GWh to EJ
L241_max_gen$generation <- L241_max_gen$generation * 0.0000036;
L241_renew_func_params <- merge( L241_renew_func_params, L241_max_gen )

# Create a table for smooth renewable subresource parameters
printlog( "L241_SmoothRenewSubRes: SMOOTH RENEW SUBRESOURCE" )
L241_SmoothRenewSubRes <- A_res_other
L241_SmoothRenewSubRes <- L241_SmoothRenewSubRes[ rep( 1:nrow( L241_SmoothRenewSubRes ),
    times = nrow( L241_renew_func_params ) ), ]
L241_SmoothRenewSubRes <- data.frame( region=L241_renew_func_params$state, L241_SmoothRenewSubRes,
    maxSubResource=L241_renew_func_params$generation, mid_price=L241_renew_func_params$mid_p,
    curve_exponent=L241_renew_func_params$b_exp )

# Now recreate elect_td_bld to allow for state level rooftop pv.
printlog( "L241_DeleteSector: DELETE SECTOR" )
L241_DeleteSector <- subset( A_res_delete, type == "sector", select=c( "region", "name" ) )

printlog( "L241_ElecSector: SECTOR LEVEL PARAMS" )
# We will have to put reserve margin and grid capacity factor in a seperate table
# since they need to be in rows
L241_SectorMetaInfo <- melt( A_res_sector[ c( "supplysector", "electricity_reserve_margin", "average_grid_capacity_factor" ) ], id="supplysector" )
L241_SectorMetaInfo$variable <- gsub( "_", "-", L241_SectorMetaInfo$variable )
# The sector table will include units strings as well as the logit exponent by region
L241_ElecSector <- A_res_sector[, c( "supplysector", "input_unit", "output_unit", "price_unit", "logit" ) ]
L241_ElecSector <- L241_ElecSector[ rep( 1:nrow( L241_ElecSector ), times = length( states ) ), ]
L241_ElecSector <- data.frame( region=states, L241_ElecSector )

L241_SectorMetaInfo <- L241_SectorMetaInfo[ sort (rep( 1:nrow( L241_SectorMetaInfo ), times = length( states ) ) ), ]
L241_SectorMetaInfo <- data.frame( region=states, L241_SectorMetaInfo )

# Create a table for subsector nest share-weights and logits
printlog( "L241_ElecSubsector: SUBSECTOR LEVEL PARAMS" )
L241_ElecSubsector <- A_res_subsector[ sort( rep( 1:nrow( A_res_subsector ), times=length( states ) ) ), ]
L241_ElecSubsector <- data.frame( region=states, L241_ElecSubsector )

# Create a table for subsector interpolation rules
printlog( "L241_ElecSubsInterpRule: SUBSECTOR LEVEL INTERP RULES" )
L241_ElecSubsInterpRule <- data.frame( A_res_subs_interp, apply_to="share-weight" )
L241_ElecSubsInterpRule <- L241_ElecSubsInterpRule[ sort( rep( 1:nrow( L241_ElecSubsInterpRule ), times = length( states ) ) ), ]
L241_ElecSubsInterpRule <- data.frame( region=states, L241_ElecSubsInterpRule )

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L241_GlobalDBTechInput: TECH INPUT" )
L241_GlobalDBTechInput <- subset( A_res_tech_input, market != "*" )
# The header wants an efficiency instead of coefficient
L241_GlobalDBTechInput$coefficient <- 1.0 / L241_GlobalDBTechInput$coefficient
names( L241_GlobalDBTechInput )[ names( L241_GlobalDBTechInput ) %in% c( "coefficient" ) ] <- "efficiency"

# Intermittent technologies will need their own header so they must be split out.
# These will be found by the technologies that require backup and those parameters
# will just get mereged into one table for simplicity.
printlog( "L241_GlobalDBTechIntermit: TECH INTERMITTENT" )
L241_GlobalDBTechIntermit <- A_res_tech_input[ A_res_tech_input$market == "*", names( A_res_tech_input ) %!in% c( "market" ) ]
L241_GlobalDBTechIntermit <- merge( L241_GlobalDBTechIntermit, A_res_tech_backup )
L241_GlobalDBTechIntermitBackup <- L241_GlobalDBTechIntermit[, names( L241_GlobalDBTechIntermit ) %in%
    c( "supplysector", "subsector", "technology", "period", "backup_energy_input", "backup_efficiency", "backup_market" ) ]
L241_GlobalDBTechIntermit <- L241_GlobalDBTechIntermit[, names( L241_GlobalDBTechIntermit ) %!in%
    c( "backup_energy_input", "backup_efficiency", "backup_market" ) ]

# Create stubs to pull the techs from the global tech db
printlog( "L241_TechStubs: TECH INPUT" )
L241_TechStubs <- unique( A_res_tech_input[, c( "supplysector", "subsector", "technology" ) ] )
L241_TechStubs <- L241_TechStubs[ sort( rep( 1:nrow( L241_TechStubs ), times=length( states ) ) ), ]
L241_TechStubs <- data.frame( region=states, L241_TechStubs )

# We need to relink the bld technologies to use the regional elect_td_bld instead of the
# US level
printlog( "L241_ChangeMarket: CHANGE ELECT BLD TECH MARKET TO REGIONAL" )
L241_ChangeMarket <- unique( subset( A_bld_technology, minicam_energy_input== A_res_sector$supplysector,
    select=c( "supplysector", "subsector", "technology", "minicam_energy_input" ) ) )
L241_gcam_years <- c( GCAM_model_period0, GCAM_years )
L241_ChangeMarket <- L241_ChangeMarket[ sort( rep( 1:nrow( L241_ChangeMarket ), times=length( L241_gcam_years ) ) ), ]
L241_ChangeMarket <- data.frame( L241_ChangeMarket[, names( L241_ChangeMarket ) %!in% c( "minicam_energy_input" ) ],
    period=L241_gcam_years, L241_ChangeMarket[, "minicam_energy_input" ] )
L241_ChangeMarket <- data.frame( region=states, L241_ChangeMarket[
    sort( rep( 1:nrow( L241_ChangeMarket ), times=length( states ) ) ), ],
    market=states )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L241_DeleteResource, "DeleteResource", "L241_DeleteResource", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_PVResource, "RenewResource", "L241_PVResource", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_PVTechChange, "SmoothRenewResTechChange", "L241_PVTechChange", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_SmoothRenewSubRes, "SmoothRenewRes", "L241_SmoothRenewSubRes", "batch_rgcam_rooftop_pv.xml" )

write_mi_data( L241_DeleteSector, "SectorDelete", "L241_DeleteSector", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_ElecSector, "Sector", "L241_ElecSector", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_SectorMetaInfo, "SectorMetaInfo", "L241_SectorMetaInfo", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_ElecSubsector, "ElecSubsector", "L241_ElecSubsector", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_ElecSubsInterpRule, "ElecSubsInterpRule", "L241_ElecSubsInterpRule", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_GlobalDBTechInput, "GlobalDBTechInput", "L241_GlobalDBTechInput", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_GlobalDBTechIntermit, "GlobalDBTechIntermitSingle", "L241_GlobalDBTechIntermit", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_GlobalDBTechIntermitBackup, "GlobalDBTechIntermitBackup", "L241_GlobalDBTechIntermitBackup", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_TechStubs, "TechStubs", "L241_TechStubs", "batch_rgcam_rooftop_pv.xml" )
write_mi_data( L241_ChangeMarket, "ChangeInputMarket", "L241_ChangeMarket", "batch_rgcam_rooftop_pv.xml" )

insert_file_into_batchxml( "batch_rgcam_rooftop_pv.xml", "rgcam_rooftop_pv.xml", "", xml_tag="outFile" )

logstop()
