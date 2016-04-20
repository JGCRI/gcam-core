if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "L244.building_USA.R" )
printlog( "GCAM-USA buildings sector model inputs" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_bld_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
A44.gcam_consumer_usa <- readdata( "ENERGY_ASSUMPTIONS", "A44.gcam_consumer" )
A44.sector_usa <- readdata( "ENERGY_ASSUMPTIONS", "A44.sector" )
calibrated_techs_bld_usa <- readdata( "GCAMUSA_MAPPINGS", "calibrated_techs_bld_usa" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
A44.bld_shell_conductance <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.bld_shell_conductance" )
A44.demandFn_flsp <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.demandFn_flsp" )
A44.demandFn_serv <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.demandFn_serv" )
A44.gcam_consumer <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.gcam_consumer" )
A44.satiation_flsp <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.satiation_flsp" )
A44.sector <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.sector" )
A44.subsector_interp <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.subsector_interp" )
A44.subsector_logit <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.subsector_logit" )
A44.subsector_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.subsector_shrwt" )
A44.globaltech_cost <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.globaltech_cost" )
A44.globaltech_eff <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.globaltech_eff" )
A44.globaltech_eff_avg <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.globaltech_eff_avg" )
A44.globaltech_shares <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.globaltech_shares" )
A44.globaltech_intgains <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.globaltech_intgains" )
A44.globaltech_retirement <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.globaltech_retirement" )
A44.globaltech_shrwt <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.globaltech_shrwt" )
A44.globaltech_interp <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.globaltech_interp" )
A44.demand_satiation_mult <- readdata( "GCAMUSA_ASSUMPTIONS", "A44.demand_satiation_mult" )
L144.flsp_bm2_state_res <- readdata( "GCAMUSA_LEVEL1_DATA", "L144.flsp_bm2_state_res" )
L144.flsp_bm2_state_comm <- readdata( "GCAMUSA_LEVEL1_DATA", "L144.flsp_bm2_state_comm" )
L144.in_EJ_state_comm_F_U_Y <- readdata("GCAMUSA_LEVEL1_DATA","L144.in_EJ_state_comm_F_U_Y")
L144.in_EJ_state_res_F_U_Y <- readdata("GCAMUSA_LEVEL1_DATA","L144.in_EJ_state_res_F_U_Y")
L143.HDDCDD_scen_state <- readdata( "GCAMUSA_LEVEL1_DATA", "L143.HDDCDD_scen_state" )
L100.Pop_thous_state <- readdata( "GCAMUSA_LEVEL1_DATA" , "L100.Pop_thous_state" )
L100.pcGDP_thous90usd_state <- readdata( "GCAMUSA_LEVEL1_DATA", "L100.pcGDP_thous90usd_state" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# Need to delete the buildings sector in the USA region (gcam.consumers and supplysectors)
L244.DeleteConsumer_USAbld <- data.frame( region = "USA", A44.gcam_consumer_usa[ "gcam.consumer" ] )
L244.DeleteSupplysector_USAbld <- data.frame( region = "USA", A44.sector_usa[ supp ] )

#Subregional population and income shares: need to be read in because these default to 0
printlog( "L244.SubregionalShares: subregional population and income shares (not currently used)" )
L244.SubregionalShares <- write_to_all_states( A44.gcam_consumer, names_BldConsumers )
L244.SubregionalShares[ c( "pop.year.fillout", "inc.year.fillout" ) ] <- min( model_base_years )
L244.SubregionalShares[ c( "subregional.population.share", "subregional.income.share" ) ] <- 1

#internal gains
printlog( "L244.PriceExp_IntGains: price exponent on floorspace and naming of internal gains trial markets" )
L244.PriceExp_IntGains <- write_to_all_states( A44.gcam_consumer, names_PriceExp_IntGains )

printlog( "L244.Floorspace: base year floorspace" )
#Building residential floorspace in the base years
# Keep all historical years for now - these are needed in calculating satiation adders later on
L244.Floorspace_resid <- interpolate_and_melt( L144.flsp_bm2_state_res, historical_years, value.name="base.building.size", digits = digits_floorspace )
L244.Floorspace_resid$region <- L244.Floorspace_resid$state
bld_nodes_noregion <- c( "gcam.consumer", "nodeInput", "building.node.input" )
A44.gcam_consumer_resid <- subset( A44.gcam_consumer, grepl( "res", A44.gcam_consumer$gcam.consumer ) )
L244.Floorspace_resid[ bld_nodes_noregion ] <- A44.gcam_consumer_resid[ bld_nodes_noregion ]
L244.Floorspace_resid <- L244.Floorspace_resid[ names_Floorspace ]

#Commercial floorspace
L244.Floorspace_comm <- interpolate_and_melt( L144.flsp_bm2_state_comm, historical_years, value.name="base.building.size", digits = digits_floorspace ) 
L244.Floorspace_comm$region <- L244.Floorspace_comm$state
A44.gcam_consumer_comm <- subset( A44.gcam_consumer, grepl( "comm", A44.gcam_consumer$gcam.consumer ) )
L244.Floorspace_comm[ bld_nodes_noregion ] <- A44.gcam_consumer_comm[ bld_nodes_noregion ]
L244.Floorspace_comm <- L244.Floorspace_comm[ names_Floorspace ]

L244.Floorspace_full <- rbind( L244.Floorspace_resid, L244.Floorspace_comm )
L244.Floorspace <- subset( L244.Floorspace_full, year %in% model_base_years )

#demand function
printlog( "L244.DemandFunction_serv and L244.DemandFunction_flsp: demand function types" )
L244.DemandFunction_serv <- write_to_all_states( A44.demandFn_serv, names_DemandFunction_serv )
L244.DemandFunction_flsp <- write_to_all_states( A44.demandFn_flsp, names_DemandFunction_flsp )

#Floorspace demand satiation
printlog( "L244.Satiation_flsp: Satiation levels assumed for floorspace" )
L244.Satiation_flsp <- melt( A44.satiation_flsp, id.vars = "state", variable.name = "gcam.consumer" )
L244.Satiation_flsp$region <- L244.Satiation_flsp$state

# Need to make sure that the satiation level is greater than the floorspace in the final base year
L244.Satiation_flsp$flsp_bm2 <- L244.Floorspace$base.building.size[
      match( paste( L244.Satiation_flsp$region, L244.Satiation_flsp$gcam.consumer, max( model_base_years ) ),
             vecpaste( L244.Floorspace[ c( "region", "gcam.consumer", "year" ) ]))]
L244.Satiation_flsp$pop <- L100.Pop_thous_state[[ paste0( "X", max( model_base_years ) ) ]][
      match( L244.Satiation_flsp$region, L100.Pop_thous_state$state ) ]
L244.Satiation_flsp$pcflsp_mm2cap <- L244.Satiation_flsp$flsp_bm2 / L244.Satiation_flsp$pop      
      
#Satiation level = maximum of exogenous assumption and the observed value in the final calibration year
L244.Satiation_flsp$satiation.level <- round(
      pmax( L244.Satiation_flsp$value * conv_thous_bil,
            L244.Satiation_flsp$pcflsp_mm2cap * 1.001 ),
      digits_satiation_adder )
L244.Satiation_flsp[ bld_nodes_noregion ] <- A44.gcam_consumer[
      match( L244.Satiation_flsp$gcam.consumer, A44.gcam_consumer$gcam.consumer ),
      bld_nodes_noregion ]
L244.Satiation_flsp <- L244.Satiation_flsp[ c( names_BldNodes, "satiation.level" ) ]

# Satiation adder - this is total BS. Required for shaping the future floorspace growth trajectories in each region
printlog( "L244.SatiationAdder: Satiation adders in floorspace demand function" )
#First, prepare socioeconomics tables by adding region names
L100.pcGDP_thous90usd_state$region <- L100.pcGDP_thous90usd_state$state
L100.Pop_thous_state$region <- L100.Pop_thous_state$state

#Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)
L244.SatiationAdder <- L244.Satiation_flsp
L244.SatiationAdder$pcGDP_thous90USD <- L100.pcGDP_thous90usd_state[[ X_satiation_year ]][
      match( L244.SatiationAdder$region, L100.pcGDP_thous90usd_state$region ) ]
L244.SatiationAdder$Flsp_bm2 <- L244.Floorspace_full$base.building.size[
      match( paste( L244.SatiationAdder$region, L244.SatiationAdder$gcam.consumer, satiation_year ),
             paste( L244.Floorspace_full$region, L244.Floorspace_full$gcam.consumer, L244.Floorspace_full$year ) ) ]
L244.SatiationAdder$pop_thous <- L100.Pop_thous_state[[ X_satiation_year ]][
      match( L244.SatiationAdder$region, L100.Pop_thous_state$region ) ]
L244.SatiationAdder$pcFlsp_mm2 <- L244.SatiationAdder$Flsp_bm2 / L244.SatiationAdder$pop_thous

#At this point, we have all of the data required for calculating the satiation adder in each region
L244.SatiationAdder$satiation.adder <- round(
         L244.SatiationAdder$satiation.level - (
         exp( log( 2 ) * L244.SatiationAdder$pcGDP_thous90USD / gdp_mid_satiation ) *
         ( L244.SatiationAdder$satiation.level - L244.SatiationAdder$pcFlsp_mm2 ) ),
      digits_satiation_adder )

#The satiation adder (million square meters of floorspace per person) needs to be less than the per-capita demand in the final calibration year
# need to match in the demand in the final calibration year to check this. love the buildings model.
L244.SatiationAdder$Flsp_bm2_fby <- L244.Floorspace_full$base.building.size[
      match( paste( L244.SatiationAdder$region, L244.SatiationAdder$gcam.consumer, max( model_base_years ) ),
             paste( L244.Floorspace_full$region, L244.Floorspace_full$gcam.consumer, L244.Floorspace_full$year ) ) ]
L244.SatiationAdder$pcFlsp_mm2_fby <- L244.SatiationAdder$Flsp_bm2_fby / L244.SatiationAdder$pop_thous
L244.SatiationAdder$satiation.adder[ L244.SatiationAdder$satiation.adder > L244.SatiationAdder$pcFlsp_mm2_fby ] <-
      L244.SatiationAdder$pcFlsp_mm2_fby[ L244.SatiationAdder$satiation.adder > L244.SatiationAdder$pcFlsp_mm2_fby ] * 0.999
L244.SatiationAdder <- L244.SatiationAdder[ names_SatiationAdder ]
# First, separate the thermal from the generic services. Generic services will be assumed to produce
# internal gain energy, so anything in the internal gains assumptions table will be assumed generic
generic_services <- unique( A44.globaltech_intgains$supplysector )
thermal_services <- unique( A44.sector$supplysector )[ !unique( A44.sector$supplysector ) %in% generic_services ]

#Heating and cooling degree days (thermal services only)
printlog( "L244.HDDCDD: Heating and cooling degree days by scenario" )
# Processing of HDD and CDD data
L244.all_scen_gcm <- unique( L143.HDDCDD_scen_state [c ("Scen", "GCM") ] )
L244.all_scen_gcm$scenID <- 1:nrow( L244.all_scen_gcm )  
L244.HDDCDD_scen_state <- L143.HDDCDD_scen_state
L244.HDDCDD_scen_state$scenID <- L244.all_scen_gcm$scenID[
  match( vecpaste( L244.HDDCDD_scen_state[ c( "Scen", "GCM" ) ] ),
         vecpaste( L244.all_scen_gcm[ c( "Scen", "GCM" ) ] ) ) ]

L244.HDDCDD_scen_state.melt <- melt( L244.HDDCDD_scen_state, id.vars = c( "state","scenID", "Scen", "GCM", "variable" ), variable.name = "Xyear")
L244.HDDCDD_scen_state.melt$year <- sub( "X", "", L244.HDDCDD_scen_state.melt$Xyear )
L244.HDDCDD_scen_state.melt$region <- L244.HDDCDD_scen_state.melt$state

#Let's make a climate normal for each region, using a selected interval of years
# Don't want to just set one year, because we want average values for all regions
# Probably want this to be up to 2000, since in SRES scenarios 2001 is a future year
L244.HDDCDD_normal_state <- subset( L244.HDDCDD_scen_state.melt, scenID == 1 & year %in% climate_normal_years )
L244.HDDCDD_normal_state <- aggregate( L244.HDDCDD_normal_state[ "value" ],
  by = as.list( L244.HDDCDD_normal_state[ c( "region", "variable" ) ] ), mean )

#Subset the heating and cooling services, separately
heating_services <- thermal_services[ grepl( "heating", thermal_services ) ]
cooling_services <- thermal_services[ grepl( "cooling", thermal_services ) ]
L244.HDDCDD <- data.frame(
      region = rep( states_subregions$state, times = length( thermal_services ) ),
      thermal.building.service.input = sort( rep( thermal_services, times = nrow( states_subregions ) ) ) )
L244.HDDCDD$gcam.consumer <- calibrated_techs_bld_usa$sector[
      match( L244.HDDCDD$thermal.building.service.input, calibrated_techs_bld_usa$service ) ]
L244.HDDCDD[ bld_nodes_noregion ] <- A44.gcam_consumer[
      match( L244.HDDCDD$gcam.consumer, A44.gcam_consumer$gcam.consumer ),
      bld_nodes_noregion ]
L244.HDDCDD <- L244.HDDCDD[ c( names_BldNodes, "thermal.building.service.input" ) ]
L244.HDDCDD <- repeat_and_add_vector( L244.HDDCDD, Y, model_years )
L244.HDDCDD <- repeat_and_add_vector( L244.HDDCDD, "scenID", unique( L244.HDDCDD_scen_state$scenID ) )
L244.HDDCDD$variable[ L244.HDDCDD$thermal.building.service.input %in% heating_services ] <- "HDD"
L244.HDDCDD$variable[ L244.HDDCDD$thermal.building.service.input %in% cooling_services ] <- "CDD"
L244.HDDCDD$degree.days <- round( L244.HDDCDD_scen_state.melt$value[
  match( vecpaste( L244.HDDCDD[ c( "scenID", "region", "variable", "year" ) ] ),
         vecpaste( L244.HDDCDD_scen_state.melt[ c( "scenID", "region", "variable", "year" ) ] ) ) ],
  digits_hddcdd )

#shell efficiency
printlog( "L244.ShellConductance_bld: Shell conductance (inverse of shell efficiency)" )
L244.shell_eff_state <- interpolate_and_melt( A44.bld_shell_conductance, model_years, value.name="shell.conductance", digits = digits_efficiency )
L244.shell_eff_state <- repeat_and_add_vector( L244.shell_eff_state, "region", states_subregions$state )
L244.shell_eff_state[ bld_nodes_noregion ] <- A44.gcam_consumer[
  match( L244.shell_eff_state$gcam.consumer, A44.gcam_consumer$gcam.consumer ),
  bld_nodes_noregion ]

L244.shell_eff_state$shell.year <- L244.shell_eff_state$year
L244.shell_eff_state$floor.to.surface.ratio <- floor.to.surface.ratio
L244.ShellConductance_bld <- L244.shell_eff_state[ names_ShellConductance ]

#The remainder of the building-level parameters require information about the output of each service, which we do not have yet
# First, technology-level inputs and efficiencies need to be assigned.
# So moving to service supply sectors/subsectors/technologies
#supplysector
printlog( "L244.Supplysector_bld: Supplysector info for buildings" )
L244.Supplysector_bld <- write_to_all_states( A44.sector, names_Supplysector )
L244.SectorLogitTables_bld <- get_logit_fn_tables( write_to_all_states( A44.sector, names_SupplysectorLogitType ),
    names_SupplysectorLogitType, base.header="Supplysector_", include.equiv.table=T, write.all.regions=F )

printlog( "L244.FinalEnergyKeyword_bld: Supply sector keywords for detailed building sector" )
L244.FinalEnergyKeyword_bld <- na.omit( write_to_all_states( A44.sector, names_FinalEnergyKeyword ) )

# Subsector information
#logit
printlog( "L244.SubsectorLogit_bld: Subsector logit exponents of building sector" )
L244.SubsectorLogit_bld <- write_to_all_states( A44.subsector_logit, names_SubsectorLogit )
L244.SubsectorLogitTables_bld <- get_logit_fn_tables( write_to_all_states( A44.subsector_logit, names_SubsectorLogitType ),
    names_SubsectorLogitType, base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )

#shareweight
printlog( "L244.SubsectorShrwt_bld and L244.SubsectorShrwtFllt_bld: Subsector shareweights of building sector" )
if( any( !is.na( A44.subsector_shrwt$year ) ) ){
  L244.SubsectorShrwt_bld <- write_to_all_states( A44.subsector_shrwt[ !is.na( A44.subsector_shrwt$year ), ], names_SubsectorShrwt )
}
if( any( !is.na( A44.subsector_shrwt$year.fillout ) ) ){
  L244.SubsectorShrwtFllt_bld <- write_to_all_states( A44.subsector_shrwt[ !is.na( A44.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
}

#interpolate
printlog( "L244.SubsectorInterp_bld and L244.SubsectorInterpTo_bld: Subsector shareweight interpolation of building sector" )
if( any( is.na( A44.subsector_interp$to.value ) ) ){
  L244.SubsectorInterp_bld <- write_to_all_states( A44.subsector_interp[ is.na( A44.subsector_interp$to.value ), ], names_SubsectorInterp )
}
if( any( !is.na( A44.subsector_interp$to.value ) ) ){
  L244.SubsectorInterpTo_bld <- write_to_all_states( A44.subsector_interp[ !is.na( A44.subsector_interp$to.value ), ], names_SubsectorInterpTo )
}

#technology 
printlog( "L244.StubTech_bld: Identification of stub technologies for buildings" )
L244.StubTech_bld <- write_to_all_states( unique( A44.globaltech_eff[ s_s_t ] ), names_Tech )
names( L244.StubTech_bld ) <- sub( "technology", "stub.technology", names( L244.StubTech_bld ) )

#end use effciency 
printlog( "L244.GlobalTechEff_bld: Assumed efficiencies (all years) of buildings technologies" )
L244.end_use_eff <- interpolate_and_melt( A44.globaltech_eff, model_years, value.name="efficiency", digits = digits_calOutput )
L244.end_use_eff$region <- L244.end_use_eff$state
L244.end_use_eff[ c( "sector.name", "subsector.name" ) ] <- L244.end_use_eff[ c( supp, subs ) ]
L244.GlobalTechEff_bld <- L244.end_use_eff[ names_GlobalTechEff ]

printlog( "L244.StubTechMarket_bld: Specify market names for fuel inputs to all technologies in each state" )
L244.end_use_eff$market.name <- "USA"
L244.end_use_eff$stub.technology <- L244.end_use_eff$technology
L244.StubTechMarket_bld <- write_to_all_states( L244.end_use_eff, names_StubTechMarket )

if( use_regional_fuel_markets ){
	L244.StubTechMarket_bld$market.name[ L244.StubTechMarket_bld[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
	      match( L244.StubTechMarket_bld$region[ L244.StubTechMarket_bld[[input]] %in% regional_fuel_markets ],
	             states_subregions$state ) ]
}
printlog( "NOTE: electricity is consumed from state markets" )
L244.StubTechMarket_bld$market.name[ L244.StubTechMarket_bld[[input]] %in% elect_td_sectors ] <-
      L244.StubTechMarket_bld$region[ L244.StubTechMarket_bld[[input]] %in% elect_td_sectors ]

#calibration 
printlog( "L244.StubTechCalInput_bld: Calibrated energy consumption by buildings technologies" )
L244.in_EJ_state_bld_F_U_Y <- rbind( L144.in_EJ_state_res_F_U_Y, L144.in_EJ_state_comm_F_U_Y )
L244.in_EJ_R_bld_serv_F_Yh <- interpolate_and_melt( L244.in_EJ_state_bld_F_U_Y, model_base_years, value.name = "calibrated.value", digits = digits_calOutput )
#NOTE: techs don't have partitioned efficiencies at this point, so don't match in the technology names
L244.in_EJ_R_bld_serv_F_Yh[ c( supp, subs, input ) ] <- calibrated_techs_bld_usa[
  match( vecpaste( L244.in_EJ_R_bld_serv_F_Yh[ c( "sector", "service", "fuel" ) ] ),
         vecpaste( calibrated_techs_bld_usa[ c( "sector", "supplysector", "fuel" ) ] ) ),
  c( supp, subs, input ) ]

printlog( "NOTE: Shares allocated to partitioned technologies need to be computed first")
L244.globaltech_eff_prt <- interpolate_and_melt(
      subset( A44.globaltech_eff, paste( supplysector, subsector ) %in%
              paste( A44.globaltech_eff_avg$supplysector, A44.globaltech_eff_avg$subsector ) ),
      efficiency_partition_year,
      value.name = "efficiency" )

A44.globaltech_eff_avg$efficiency_tech1 <- L244.globaltech_eff_prt$efficiency[
      match( vecpaste( A44.globaltech_eff_avg[ c( supp, subs, "technology1" ) ] ),
             vecpaste( L244.globaltech_eff_prt[ c( s_s_t ) ] ) ) ]
A44.globaltech_eff_avg$efficiency_tech2 <- L244.globaltech_eff_prt$efficiency[
      match( vecpaste( A44.globaltech_eff_avg[ c( supp, subs, "technology2" ) ] ),
             vecpaste( L244.globaltech_eff_prt[ c( s_s_t ) ] ) ) ]
A44.globaltech_eff_avg$share_tech1 <- with( A44.globaltech_eff_avg, ( stockavg - efficiency_tech2 ) / ( efficiency_tech1 - efficiency_tech2 ) )
A44.globaltech_eff_avg$share_tech2 <- 1 - A44.globaltech_eff_avg$share_tech1

#These shares can now be binded into the table of technology shares, where shares are not computed from stock avg efficiencies
L244.globaltech_shares <- rbind( A44.globaltech_eff_avg[ names( A44.globaltech_shares ) ], A44.globaltech_shares )

#For calibration table, start with global tech efficiency table, repeat by states, and match in tech shares.
L244.StubTechCalInput_bld <- repeat_and_add_vector( L244.GlobalTechEff_bld[ L244.GlobalTechEff_bld$year %in% model_base_years, names_GlobalTechInput ], "region", states_subregions$state )
L244.StubTechCalInput_bld[ c( supp, subs, "stub.technology" ) ] <- L244.StubTechCalInput_bld[ c( "sector.name", "subsector.name", "technology" ) ]
L244.StubTechCalInput_bld$share <- 1
L244.StubTechCalInput_bld$share[
      vecpaste( L244.StubTechCalInput_bld[ s_s_t ] ) %in% vecpaste( L244.globaltech_shares[ c( supp, subs, "technology1" ) ] ) ] <-
      L244.globaltech_shares$share_tech1[
         match( vecpaste( L244.StubTechCalInput_bld[ vecpaste( L244.StubTechCalInput_bld[ s_s_t ] ) %in% vecpaste( L244.globaltech_shares[ c( supp, subs, "technology1" ) ] ), s_s_t ] ),
                vecpaste( L244.globaltech_shares[ c( supp, subs, "technology1" ) ] ) ) ]
L244.StubTechCalInput_bld$share[
      vecpaste( L244.StubTechCalInput_bld[ s_s_t ] ) %in% vecpaste( L244.globaltech_shares[ c( supp, subs, "technology2" ) ] ) ] <-
      L244.globaltech_shares$share_tech2[
         match( vecpaste( L244.StubTechCalInput_bld[ vecpaste( L244.StubTechCalInput_bld[ s_s_t ] ) %in% vecpaste( L244.globaltech_shares[ c( supp, subs, "technology2" ) ] ), s_s_t ] ),
                vecpaste( L244.globaltech_shares[ c( supp, subs, "technology2" ) ] ) ) ]

#At this point, the energy by state/service/fuel is ready to be matched in, multiplied by the shares allocated to each technology
L244.StubTechCalInput_bld$calibrated.value <- round(
      L244.StubTechCalInput_bld$share *
         L244.in_EJ_R_bld_serv_F_Yh$calibrated.value[
            match( vecpaste( L244.StubTechCalInput_bld[ c( "region", supp, subs, input, Y ) ] ),
                   vecpaste( L244.in_EJ_R_bld_serv_F_Yh[ c( "state", supp, subs, input, Y ) ] ) ) ],
      digits_calOutput )

L244.StubTechCalInput_bld$share.weight.year <- L244.StubTechCalInput_bld$year
L244.StubTechCalInput_bld <- set_subsector_shrwt( L244.StubTechCalInput_bld, value.name = "calibrated.value" )
L244.StubTechCalInput_bld$tech.share.weight <- ifelse( L244.StubTechCalInput_bld$calibrated.value > 0, 1, 0 )
L244.StubTechCalInput_bld <- L244.StubTechCalInput_bld[ names_StubTechCalInput ]

#shareweight
printlog( "L244.GlobalTechShrwt_bld: Default shareweights for global building technologies" )
L244.GlobalTechShrwt_bld <- interpolate_and_melt( A44.globaltech_shrwt, model_years, value.name="share.weight" )
L244.GlobalTechShrwt_bld[ c( "sector.name", "subsector.name" ) ] <- L244.GlobalTechShrwt_bld[ c( supp, subs ) ]
L244.GlobalTechShrwt_bld <- L244.GlobalTechShrwt_bld[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L244.GlobalTechInterpTo_bld: Technology shareweight interpolation (selected techs only)" )
L244.GlobalTechInterpTo_bld <- A44.globaltech_interp
L244.GlobalTechInterpTo_bld[ c( "sector.name", "subsector.name" ) ] <- L244.GlobalTechInterpTo_bld[ c( supp, subs ) ]
L244.GlobalTechInterpTo_bld <- L244.GlobalTechInterpTo_bld[ names_GlobalTechInterpTo ]

#Costs of global technologies
printlog( "L244.GlobalTechCost_bld: Non-fuel costs of global building technologies" )
L244.GlobalTechCost_bld <- interpolate_and_melt( A44.globaltech_cost, model_years, value.name = "input.cost" )
L244.GlobalTechCost_bld[ c( "sector.name", "subsector.name" ) ] <- L244.GlobalTechCost_bld[ c( supp, subs ) ]
L244.GlobalTechCost_bld$minicam.non.energy.input <- "non-energy"
L244.GlobalTechCost_bld <- L244.GlobalTechCost_bld[ names_GlobalTechCost ]

#retirement and shutdown rate
printlog( "L244.GlobalTechSCurve_bld: Retirement rates for building technologies" )
L244.GlobalTechSCurve_bld <- subset( L244.GlobalTechCost_bld[ names_GlobalTechYr ],
      year %in% c( max( model_base_years ), model_future_years ) & sector.name %in% A44.globaltech_retirement$supplysector )

L244.GlobalTechSCurve_bld$lifetime <- A44.globaltech_retirement$lifetime[
      match( L244.GlobalTechSCurve_bld$sector.name, A44.globaltech_retirement$supplysector ) ]

L244.GlobalTechSCurve_bld$steepness <- NA
L244.GlobalTechSCurve_bld$steepness[ L244.GlobalTechSCurve_bld$year == max( model_base_years ) ] <- A44.globaltech_retirement$steepness_stock[
      match( L244.GlobalTechSCurve_bld$sector.name[ L244.GlobalTechSCurve_bld$year == max( model_base_years ) ], A44.globaltech_retirement$supplysector ) ]
L244.GlobalTechSCurve_bld$steepness[ L244.GlobalTechSCurve_bld$year != max( model_base_years ) ] <- A44.globaltech_retirement$steepness_new[
      match( L244.GlobalTechSCurve_bld$sector.name[ L244.GlobalTechSCurve_bld$year != max( model_base_years ) ], A44.globaltech_retirement$supplysector ) ]

L244.GlobalTechSCurve_bld$half.life <- NA
L244.GlobalTechSCurve_bld$half.life[ L244.GlobalTechSCurve_bld$year == max( model_base_years ) ] <- A44.globaltech_retirement$half_life_stock[
      match( L244.GlobalTechSCurve_bld$sector.name[ L244.GlobalTechSCurve_bld$year == max( model_base_years ) ], A44.globaltech_retirement$supplysector ) ]
L244.GlobalTechSCurve_bld$half.life[ L244.GlobalTechSCurve_bld$year != max( model_base_years ) ] <- A44.globaltech_retirement$half_life_new[
      match( L244.GlobalTechSCurve_bld$sector.name[ L244.GlobalTechSCurve_bld$year != max( model_base_years ) ], A44.globaltech_retirement$supplysector ) ]

L244.GlobalTechSCurve_bld <- L244.GlobalTechSCurve_bld[ names_GlobalTechSCurve ]

#internal gains output ratio
printlog( "L244.GlobalTechIntGainOutputRatio: Output ratios of internal gain energy from non-thermal building services" )
L244.GlobalTechIntGainOutputRatio <- repeat_and_add_vector( A44.globaltech_intgains, Y, model_years )
L244.GlobalTechIntGainOutputRatio[ c( "sector.name", "subsector.name" ) ] <- L244.GlobalTechIntGainOutputRatio[ c( supp, subs ) ]
L244.GlobalTechIntGainOutputRatio$gcam.consumer <- calibrated_techs_bld_usa$sector[ 
  match( L244.GlobalTechIntGainOutputRatio$supplysector, calibrated_techs_bld_usa$supplysector ) ]

L244.GlobalTechIntGainOutputRatio$internal.gains.market.name <- A44.gcam_consumer[ 
  match( L244.GlobalTechIntGainOutputRatio$gcam.consumer,
        A44.gcam_consumer$gcam.consumer ), "internal.gains.market.name" ]

L244.GlobalTechIntGainOutputRatio$efficiency <- L244.GlobalTechEff_bld$efficiency[
      match( vecpaste( L244.GlobalTechIntGainOutputRatio[ c( "sector.name", "subsector.name", "technology", "year" ) ] ),
             vecpaste( L244.GlobalTechEff_bld[ c( "sector.name", "subsector.name", "technology", "year" ) ] ) ) ]
L244.GlobalTechIntGainOutputRatio$internal.gains.output.ratio <- round(
      L244.GlobalTechIntGainOutputRatio$input.ratio / L244.GlobalTechIntGainOutputRatio$efficiency,
      digits_efficiency )
L244.GlobalTechIntGainOutputRatio <- L244.GlobalTechIntGainOutputRatio[c(names_GlobalTechYr,
                                  "internal.gains.output.ratio", "internal.gains.market.name" ) ]

#Services: base-service (service output in the base year)
printlog( "L244.GenericBaseService and L244.ThermalBaseService: Base year output of buildings services (per unit floorspace)" )
#Base-service: Multiply energy consumption by efficiency for each technology, and aggregate by service
L244.base_service <- L244.StubTechCalInput_bld
L244.base_service$efficiency <- L244.GlobalTechEff_bld$efficiency[
      match( vecpaste( L244.base_service[ c( supp, subs, "stub.technology", Y ) ] ),
             vecpaste( L244.GlobalTechEff_bld[ c( "sector.name", "subsector.name", tech, Y ) ] ) ) ]
L244.base_service$base.service <- round( L244.base_service$calibrated.value * L244.base_service$efficiency, digits_calOutput )

#Aggregate base service by service (supplysector)
L244.base_service_agg <- aggregate( L244.base_service[ "base.service" ],
      by=as.list( L244.base_service[ c( "region", supp, Y ) ] ), sum )
L244.base_service_agg$gcam.consumer <- calibrated_techs_bld_usa$sector[ match(
      L244.base_service_agg[[supp]], calibrated_techs_bld_usa[[supp]] ) ]
L244.base_service_agg[ bld_nodes_noregion ] <- A44.gcam_consumer[
      match( L244.base_service_agg$gcam.consumer, A44.gcam_consumer$gcam.consumer ),
      bld_nodes_noregion ]

# Separate thermal and generic services into separate tables with different ID strings
L244.GenericBaseService <- subset( L244.base_service_agg, supplysector %in% generic_services )
names( L244.GenericBaseService )[ names( L244.GenericBaseService ) == supp ] <- "building.service.input"
L244.GenericBaseService <- L244.GenericBaseService[ names_GenericBaseService ]

L244.ThermalBaseService <- subset( L244.base_service_agg, supplysector %in% thermal_services )
names( L244.ThermalBaseService )[ names( L244.ThermalBaseService ) == supp ] <- "thermal.building.service.input"
L244.ThermalBaseService <- L244.ThermalBaseService[ names_ThermalBaseService ]

#Service satiation
printlog( "L244.GenericServiceSatiation: Satiation levels assumed for non-thermal building services")  
#Just multiply the base-service by an exogenous multiplier
L244.GenericServiceSatiation <- subset( L244.GenericBaseService, year == max( model_base_years ) )
L244.GenericServiceSatiation$flsp_bm2 <- L244.Floorspace$base.building.size[
      match( vecpaste( L244.GenericServiceSatiation[ names_BldNodes ] ),
             vecpaste( L244.Floorspace[ names_BldNodes ] ) ) ]
L244.GenericServiceSatiation$satiation.level <- round(
      L244.GenericServiceSatiation$base.service / L244.GenericServiceSatiation$flsp_bm2 *
      A44.demand_satiation_mult$multiplier[
         match( L244.GenericServiceSatiation$building.service.input, A44.demand_satiation_mult[[supp]] ) ],
      digits_coefficient )
L244.GenericServiceSatiation <- L244.GenericServiceSatiation[ names_GenericServiceSatiation ]

printlog( "L244.ThermalServiceSatiation: Satiation levels assumed for thermal building services")  
L244.ThermalServiceSatiation <- subset( L244.ThermalBaseService, year == max( model_base_years ) )
L244.ThermalServiceSatiation$flsp_bm2 <- L244.Floorspace$base.building.size[
      match( vecpaste( L244.ThermalServiceSatiation[ names_BldNodes ] ),
             vecpaste( L244.Floorspace[ names_BldNodes ] ) ) ]
L244.ThermalServiceSatiation$satiation.level <- round(
      L244.ThermalServiceSatiation$base.service / L244.ThermalServiceSatiation$flsp_bm2 *
      A44.demand_satiation_mult$multiplier[
         match( L244.ThermalServiceSatiation$thermal.building.service.input, A44.demand_satiation_mult[[supp]] ) ],
      digits_coefficient )
L244.ThermalServiceSatiation <- L244.ThermalServiceSatiation[ names_ThermalServiceSatiation ]

#internal gains scaling
printlog( "L244.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands" )
variable <- c("HDD", "CDD")
scalar <- c(InternalGainsScalar_USA_h, InternalGainsScalar_USA_c)
DDnorm <- c( base_HDD_USA, base_CDD_USA )
US.base.scalar <- data.frame(variable, scalar, DDnorm )

L244.Intgains_scalar <- L244.ThermalServiceSatiation
L244.Intgains_scalar$variable[ L244.Intgains_scalar$thermal.building.service.input %in% heating_services ] <- "HDD"
L244.Intgains_scalar$variable[ L244.Intgains_scalar$thermal.building.service.input %in% cooling_services ] <- "CDD"
L244.Intgains_scalar$InternalGainsScalar_USA <- US.base.scalar$scalar[
  match( L244.Intgains_scalar$variable, US.base.scalar$variable ) ]
L244.Intgains_scalar$DDnorm <- US.base.scalar$DDnorm[
  match( L244.Intgains_scalar$variable, US.base.scalar$variable ) ]
L244.Intgains_scalar$degree.days <- L244.HDDCDD_normal_state$value[
  match( vecpaste( L244.Intgains_scalar[ c( "region", "variable" ) ] ),
         vecpaste( L244.HDDCDD_normal_state[ c( "region", "variable" ) ] ) ) ]
L244.Intgains_scalar$internal.gains.scalar <- round(
  L244.Intgains_scalar$InternalGainsScalar_USA *
  L244.Intgains_scalar$degree.days / L244.Intgains_scalar$DDnorm,
  digits_hddcdd )

#FINAL STEP: Prevent very warm places from having negative heating demands, using exogenous threshold
threshold_HDD <- 500
L244.Intgains_scalar$internal.gains.scalar[ L244.Intgains_scalar$variable == "HDD" & L244.Intgains_scalar$degree.days < threshold_HDD ] <- 0
L244.Intgains_scalar <- L244.Intgains_scalar[ names_Intgains_scalar ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L244.DeleteConsumer_USAbld, "DeleteConsumer", "GCAMUSA_LEVEL2_DATA", "L244.DeleteConsumer_USAbld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.DeleteSupplysector_USAbld, "DeleteSupplysector", "GCAMUSA_LEVEL2_DATA", "L244.DeleteSupplysector_USAbld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.SubregionalShares, "SubregionalShares", "GCAMUSA_LEVEL2_DATA", "L244.SubregionalShares", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.PriceExp_IntGains, "PriceExp_IntGains", "GCAMUSA_LEVEL2_DATA", "L244.PriceExp_IntGains", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.Floorspace, "Floorspace", "GCAMUSA_LEVEL2_DATA", "L244.Floorspace", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.DemandFunction_serv, "DemandFunction_serv", "GCAMUSA_LEVEL2_DATA", "L244.DemandFunction_serv", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.DemandFunction_flsp, "DemandFunction_flsp", "GCAMUSA_LEVEL2_DATA", "L244.DemandFunction_flsp", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.Satiation_flsp, "Satiation_flsp", "GCAMUSA_LEVEL2_DATA", "L244.Satiation_flsp", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.SatiationAdder, "SatiationAdder", "GCAMUSA_LEVEL2_DATA", "L244.SatiationAdder", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.ThermalBaseService, "ThermalBaseService", "GCAMUSA_LEVEL2_DATA", "L244.ThermalBaseService", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.GenericBaseService, "GenericBaseService", "GCAMUSA_LEVEL2_DATA", "L244.GenericBaseService", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
#Need to subset and write out the HDD/CDD xmls in a for loop, as we don't know the number or names of scenarios
for( i in 1:max( L244.HDDCDD_scen_state$scenID ) ){
  scenarios <- unique( L244.HDDCDD_scen_state[ c( "scenID", "Scen", "GCM" ) ] )
  scen_strings <- paste( scenarios$Scen, scenarios$GCM, sep = "_" )
  objectname <- paste0( "L244.HDDCDD_", scen_strings[i] )
  object <- subset( L244.HDDCDD, scenID == i )[ names_HDDCDD ] 
  assign( objectname, object )
  batchXMLstring <- paste0( "batch_HDDCDD_",
                            substr( objectname, regexpr( "HDDCDD_", objectname ) + 7, nchar( objectname ) ),
                            "_USA.xml" )
  write_mi_data( object, "HDDCDD", "GCAMUSA_LEVEL2_DATA", objectname, "GCAMUSA_XML_BATCH", batchXMLstring )
  XMLstring <- sub( "batch_", "", batchXMLstring )
  insert_file_into_batchxml( "GCAMUSA_XML_BATCH", batchXMLstring, "GCAMUSA_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}
write_mi_data( L244.ThermalServiceSatiation, "ThermalServiceSatiation", "GCAMUSA_LEVEL2_DATA", "L244.ThermalServiceSatiation", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.GenericServiceSatiation, "GenericServiceSatiation", "GCAMUSA_LEVEL2_DATA", "L244.GenericServiceSatiation", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.Intgains_scalar, "Intgains_scalar", "GCAMUSA_LEVEL2_DATA", "L244.Intgains_scalar", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.ShellConductance_bld, "ShellConductance", "GCAMUSA_LEVEL2_DATA", "L244.ShellConductance_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 

for( curr_table in names ( L244.SectorLogitTables_bld ) ) {
write_mi_data( L244.SectorLogitTables_bld[[ curr_table ]]$data, L244.SectorLogitTables_bld[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L244.", L244.SectorLogitTables_bld[[ curr_table ]]$header ), "GCAMUSA_XML_BATCH",
    "batch_building_USA.xml" )
}
write_mi_data( L244.Supplysector_bld, IDstring="Supplysector", domain="GCAMUSA_LEVEL2_DATA", fn="L244.Supplysector_bld", batch_XML_domain="GCAMUSA_XML_BATCH", batch_XML_file="batch_building_USA.xml" ) 
write_mi_data( L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword", "GCAMUSA_LEVEL2_DATA", "L244.FinalEnergyKeyword_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
 if( exists( "L244.SubsectorShrwt_bld" ) ){
 	write_mi_data( L244.SubsectorShrwt_bld, "SubsectorShrwt", "GCAMUSA_LEVEL2_DATA", "L244.SubsectorShrwt_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" )
 	}
 if( exists( "L244.SubsectorShrwtFllt_bld" ) ){
 	write_mi_data( L244.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt", "GCAMUSA_LEVEL2_DATA", "L244.SubsectorShrwtFllt_bld",
 	               "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
 	}
 if( exists( "L244.SubsectorInterp_bld" ) ) {
 	write_mi_data( L244.SubsectorInterp_bld, "SubsectorInterp", "GCAMUSA_LEVEL2_DATA", "L244.SubsectorInterp_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" )
 	}
 if( exists( "L244.SubsectorInterpTo_bld" ) ) {
 	write_mi_data( L244.SubsectorInterpTo_bld, "SubsectorInterpTo", "GCAMUSA_LEVEL2_DATA", "L244.SubsectorInterpTo_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" )
 	}
for( curr_table in names ( L244.SubsectorLogitTables_bld ) ) {
write_mi_data( L244.SubsectorLogitTables_bld[[ curr_table ]]$data, L244.SubsectorLogitTables_bld[[ curr_table ]]$header,
    "GCAMUSA_LEVEL2_DATA", paste0("L244.", L244.SubsectorLogitTables_bld[[ curr_table ]]$header ), "GCAMUSA_XML_BATCH",
    "batch_building_USA.xml" )
}
write_mi_data( L244.SubsectorLogit_bld, "SubsectorLogit", "GCAMUSA_LEVEL2_DATA", "L244.SubsectorLogit_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.StubTech_bld, "StubTech", "GCAMUSA_LEVEL2_DATA", "L244.StubTech_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.StubTechCalInput_bld, "StubTechCalInput", "GCAMUSA_LEVEL2_DATA", "L244.StubTechCalInput_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.StubTechMarket_bld, "StubTechMarket", "GCAMUSA_LEVEL2_DATA", "L244.StubTechMarket_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 

write_mi_data( L244.GlobalTechIntGainOutputRatio, "GlobalTechIntGainOutputRatio", "GCAMUSA_LEVEL2_DATA", "L244.GlobalTechIntGainOutputRatio", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" )
write_mi_data( L244.GlobalTechInterpTo_bld, "GlobalTechInterpTo", "GCAMUSA_LEVEL2_DATA", "L244.GlobalTechInterpTo_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" )
write_mi_data( L244.GlobalTechEff_bld, "GlobalTechEff", "GCAMUSA_LEVEL2_DATA", "L244.GlobalTechEff_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.GlobalTechShrwt_bld, "GlobalTechShrwt", "GCAMUSA_LEVEL2_DATA", "L244.GlobalTechShrwt_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" ) 
write_mi_data( L244.GlobalTechCost_bld, "GlobalTechCost", "GCAMUSA_LEVEL2_DATA", "L244.GlobalTechCost_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" )
write_mi_data( L244.GlobalTechSCurve_bld, "GlobalTechSCurve", "GCAMUSA_LEVEL2_DATA", "L244.GlobalTechSCurve_bld", "GCAMUSA_XML_BATCH", "batch_building_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_building_USA.xml", "GCAMUSA_XML_FINAL", "building_USA.xml", "", xml_tag="outFile" )

logstop()
