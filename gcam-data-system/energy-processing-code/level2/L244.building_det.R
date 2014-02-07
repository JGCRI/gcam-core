if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "L244.building_det.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Model input for detailed global building sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
calibrated_techs_bld_det <- readdata( "ENERGY_MAPPINGS", "calibrated_techs_bld_det" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A44.sector <- readdata( "ENERGY_ASSUMPTIONS", "A44.sector" )
A44.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A44.subsector_interp" )
A44.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A44.subsector_logit" )
A44.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A44.subsector_shrwt" )
A44.fuelprefElasticity <- readdata( "ENERGY_ASSUMPTIONS", "A44.fuelprefElasticity" )
A44.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A44.globaltech_shrwt" )
A44.gcam_consumer <- readdata( "ENERGY_ASSUMPTIONS", "A44.gcam_consumer" )
A44.demandFn_serv <- readdata( "ENERGY_ASSUMPTIONS", "A44.demandFn_serv" )
A44.demandFn_flsp <- readdata( "ENERGY_ASSUMPTIONS", "A44.demandFn_flsp" )
A44.internal_gains <- readdata( "ENERGY_ASSUMPTIONS", "A44.internal_gains" )
A44.satiation_flsp <- readdata( "ENERGY_ASSUMPTIONS", "A44.satiation_flsp" )
A44.cost_efficiency <- readdata ("ENERGY_ASSUMPTIONS", "A44.cost_efficiency" )
A44.demand_satiation_mult <- readdata( "ENERGY_ASSUMPTIONS", "A44.demand_satiation_mult" )
L144.flsp_bm2_R_res_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L144.flsp_bm2_R_res_Yh" )
L144.flsp_bm2_R_comm_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L144.flsp_bm2_R_comm_Yh" )
L144.base_service_EJ_serv <- readdata( "ENERGY_LEVEL1_DATA", "L144.base_service_EJ_serv" )
L144.in_EJ_R_bld_serv_F_Yh <- readdata("ENERGY_LEVEL1_DATA","L144.in_EJ_R_bld_serv_F_Yh")
L144.end_use_eff <- readdata("ENERGY_LEVEL1_DATA","L144.end_use_eff")
L144.shell_eff_R_Y <- readdata("ENERGY_LEVEL1_DATA","L144.shell_eff_R_Y")
L144.NEcost_75USDGJ <- readdata( "ENERGY_LEVEL1_DATA", "L144.NEcost_75USDGJ" )
L144.internal_gains <- readdata ("ENERGY_LEVEL1_DATA", "L144.internal_gains" )
L143.HDDCDD_scen_R_Y <- readdata( "ENERGY_LEVEL1_DATA", "L143.HDDCDD_scen_R_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Subregional population and income shares: need to be read in because these default to 0
L244.SubregionalShares <- write_to_all_regions( A44.gcam_consumer, names_Consumers )
L244.SubregionalShares[ c( "pop.year.fillout", "inc.year.fillout" ) ] <- min( model_base_years )
L244.SubregionalShares[ c( "subregional.population.share", "subregional.income.share" ) ] <- 1

#internal gains
L244.PriceExp_IntGains <- write_to_all_regions( A44.gcam_consumer, names_PriceExp_IntGains )

#Building residential floorspace in the base years
L244.Floorspace_resid <- interpolate_and_melt( L144.flsp_bm2_R_res_Yh, model_base_years, value.name="base.building.size", digits = digits_floorspace )
L244.Floorspace_resid <- add_region_name( L244.Floorspace_resid )
bld_nodes_noregion <- c( "gcam.consumer", "nodeInput", "building.node.input" )
A44.gcam_consumer_resid <- subset( A44.gcam_consumer, grepl( "res", A44.gcam_consumer$gcam.consumer ) )
L244.Floorspace_resid[ bld_nodes_noregion ] <- A44.gcam_consumer_resid[ bld_nodes_noregion ]
L244.Floorspace_resid <- L244.Floorspace_resid[ names_Floorspace ]

#Commercial floorspace
L244.Floorspace_comm <- interpolate_and_melt( L144.flsp_bm2_R_comm_Yh, model_base_years, value.name="base.building.size", digits = digits_floorspace ) 
L244.Floorspace_comm <- add_region_name( L244.Floorspace_comm )
A44.gcam_consumer_comm <- subset( A44.gcam_consumer, grepl( "comm", A44.gcam_consumer$gcam.consumer ) )
L244.Floorspace_comm[ bld_nodes_noregion ] <- A44.gcam_consumer_comm[ bld_nodes_noregion ]
L244.Floorspace_comm <- L244.Floorspace_comm[ names_Floorspace ]

L244.Floorspace <- rbind( L244.Floorspace_resid, L244.Floorspace_comm )

#demand function
L244.DemandFunction_serv <- write_to_all_regions( A44.demandFn_serv, names_DemandFunction_serv )
L244.DemandFunction_flsp <- write_to_all_regions( A44.demandFn_flsp, names_DemandFunction_flsp )

#Floorspace demand satiation
L244.Satiation_flsp_class <- melt( A44.satiation_flsp, id.vars = "region.class", variable_name = "sector" )
L244.Satiation_flsp_class$satiation.level <- L244.Satiation_flsp_class$value * conv_thous_bil
L244.Satiation_flsp <- write_to_all_regions( A44.gcam_consumer, names_BldNodes )

#Match in the region class, and use this to then match in the satiation floorspace
L244.Satiation_flsp$region.class <- A_regions$region.class[
  match( L244.Satiation_flsp$region, A_regions$region ) ]
L244.Satiation_flsp$satiation.level <- L244.Satiation_flsp_class$satiation.level[
  match( vecpaste( L244.Satiation_flsp[ c( "region.class", "gcam.consumer" ) ] ),
         vecpaste( L244.Satiation_flsp_class[ c( "region.class", "sector" ) ] ) ) ]
L244.Satiation_flsp <- L244.Satiation_flsp[ names_Satiation_flsp ]

#Services: base-service (service output in the base year)
# First, separate the thermal from the generic services. Generic services will be assumed to produce
# internal gain energy, so anything in the internal gains assumptions table will be assumed generic
generic_services <- unique( A44.internal_gains$supplysector )
thermal_services <- unique( A44.sector$supplysector )[ !unique( A44.sector$supplysector ) %in% generic_services ]

#Base-service: interpolate and melt, subsetting only the model base years. change names as indicated in
# calibrated_techs_bld_det (from sector and service to gcam.consumer, nodeInput, building.node.input and
# building.service.input, where the last column is the same as the supplysector)

L244.base_service <- interpolate_and_melt(L144.base_service_EJ_serv, model_base_years, value.name = "base.service", digits = digits_calOutput )
L244.base_service[ c( "gcam.consumer", "nodeInput", "building.node.input", "building.service.input" ) ] <- 
  calibrated_techs_bld_det[ match( vecpaste( L244.base_service[c ( "sector", "service" ) ] ), 
                                   vecpaste( calibrated_techs_bld_det[ c( "sector","service" ) ] ) ),
                           c( "gcam.consumer", "nodeInput", "building.node.input", "supplysector" ) ]
L244.base_service <- add_region_name( L244.base_service )
L244.base_service <- L244.base_service[ c( names_BldNodes, "building.service.input", "year", "base.service" ) ]

# Separate thermal and generic services into separate tables with different ID strings

L244.GenericBaseService <- subset( L244.base_service, building.service.input %in% generic_services )
L244.ThermalBaseService <- subset( L244.base_service, building.service.input %in% thermal_services )
names( L244.ThermalBaseService )[ names( L244.ThermalBaseService ) == "building.service.input" ] <- "thermal.building.service.input"

#Heating and cooling degree days (thermal services only)
# Processing of HDD and CDD data
L244.all_sres_gcm <- unique( L143.HDDCDD_scen_R_Y [c ("SRES", "GCM") ] )
L244.all_sres_gcm$scenID <- 1:nrow( L244.all_sres_gcm )  
L244.HDDCDD_scen_R_Y <- L143.HDDCDD_scen_R_Y
L244.HDDCDD_scen_R_Y$scenID <- L244.all_sres_gcm$scenID[
  match( vecpaste( L244.HDDCDD_scen_R_Y[ c( "SRES", "GCM" ) ] ),
         vecpaste( L244.all_sres_gcm[ c( "SRES", "GCM" ) ] ) ) ]

L244.HDDCDD_scen_R_Y.melt <- melt( L244.HDDCDD_scen_R_Y, id.vars = c( "GCAM_region_ID","scenID", "SRES", "GCM", "variable" ), variable_name = "Xyear")
L244.HDDCDD_scen_R_Y.melt$year <- sub( "X", "", L244.HDDCDD_scen_R_Y.melt$Xyear )
L244.HDDCDD_scen_R_Y.melt <- add_region_name( L244.HDDCDD_scen_R_Y.melt )

#Let's make a climate normal for each region, using a selected interval of years
# Don't want to just set one year, because we want average values for all regions
# Probably want this to be up to 2000, since in SRES 2001 is a future year
L244.HDDCDD_normal_R_Y <- subset( L244.HDDCDD_scen_R_Y.melt, scenID == 1 & year %in% climate_normal_years )
L244.HDDCDD_normal_R_Y <- aggregate( L244.HDDCDD_normal_R_Y[ "value" ],
  by = as.list( L244.HDDCDD_normal_R_Y[ c( "region", "variable" ) ] ), mean )

#Subset the heating and cooling services, separately
heating_services <- thermal_services[ grepl( "heating", thermal_services ) ]
cooling_services <- thermal_services[ grepl( "cooling", thermal_services ) ]
L244.HDDCDD <- subset( L244.ThermalBaseService, year == min( year ) )[ c( names_BldNodes, "thermal.building.service.input" ) ]
L244.HDDCDD <- repeat_and_add_vector( L244.HDDCDD, Y, model_years )
L244.HDDCDD <- repeat_and_add_vector( L244.HDDCDD, "scenID", unique( L244.HDDCDD_scen_R_Y$scenID ) )
L244.HDDCDD$variable[ L244.HDDCDD$thermal.building.service.input %in% heating_services ] <- "HDD"
L244.HDDCDD$variable[ L244.HDDCDD$thermal.building.service.input %in% cooling_services ] <- "CDD"
L244.HDDCDD$degree.days <- round( L244.HDDCDD_scen_R_Y.melt$value[
  match( vecpaste( L244.HDDCDD[ c( "scenID", "region", "variable", "year" ) ] ),
         vecpaste( L244.HDDCDD_scen_R_Y.melt[ c( "scenID", "region", "variable", "year" ) ] ) ) ],
  digits_hddcdd )

#Service satiation
#First, calculate the service output per unit floorspace in the USA region
L244.ServiceSatiation_USA <- L144.base_service_EJ_serv[
  L144.base_service_EJ_serv[[R]] == 1, c( R_S, "service", X_historical_years ) ]
L244.ServiceSatiation_USA[ c( "gcam.consumer", "nodeInput", "building.node.input", "building.service.input" ) ] <- 
  calibrated_techs_bld_det[ match( vecpaste( L244.ServiceSatiation_USA[c ( "sector", "service" ) ] ), 
                                   vecpaste( calibrated_techs_bld_det[ c( "sector","service" ) ] ) ),
                            c( "gcam.consumer", "nodeInput", "building.node.input", "supplysector" ) ]
L244.ServiceSatiation_USA <- add_region_name( L244.ServiceSatiation_USA )
L244.ServiceSatiation_USA[[Y]] <- max( historical_years )
L244.ServiceSatiation_USA$floorspace_bm2 <- L244.Floorspace$base.building.size[
  match( vecpaste( L244.ServiceSatiation_USA[ c( "region", "gcam.consumer", Y ) ] ),
         vecpaste( L244.Floorspace[ c( "region", "gcam.consumer", Y ) ] ) ) ]

L244.ServiceSatiation_USA$satiation.level <- round(
  L244.ServiceSatiation_USA[[ X_final_historical_year ]] *
  A44.demand_satiation_mult$multiplier[
    match( L244.ServiceSatiation_USA$building.service.input, A44.demand_satiation_mult[[supp]] ) ] /
  L244.ServiceSatiation_USA$floorspace_bm2,
  digits_calOutput )
  
#Generic services: read these values to all regions because they're all the same
L244.GenericServiceSatiation <- subset( L244.ServiceSatiation_USA, building.service.input %in% generic_services )
L244.GenericServiceSatiation <- write_to_all_regions( L244.GenericServiceSatiation, names_GenericServiceSatiation )

## This is bad. Should be done here but we aren't. Instead need to match in the floorspace into the base service table, divide to calculate the service demand
## per unit floorspace in the final calibration year. This (increased slightly) is then the minimum satiation level that needs to be read in.
## TODO: fix the bad code in the model. need a flexible building service function
L244.BS <- L244.GenericBaseService
L244.BS$flsp <- L244.Floorspace$base.building.size[
      match( vecpaste( L244.BS[ c( names_BldNodes, "year" ) ] ), vecpaste( L244.Floorspace[ c( names_BldNodes, "year" ) ] ) ) ]
L244.BS$service.per.flsp <- L244.BS$base.service / L244.BS$flsp
L244.moreBS <- subset( L244.BS, year == max( model_base_years ) )
L244.GenericServiceSatiation$satiation.level <- pmax( L244.GenericServiceSatiation$satiation.level, L244.moreBS$service.per.flsp[
      match( vecpaste( L244.GenericServiceSatiation[ c( names_BldNodes, "building.service.input" ) ] ),
             vecpaste( L244.moreBS[ c( names_BldNodes, "building.service.input" ) ] ) ) ] * 1.0001 )

#Thermal services: need to multiply by HDD/CDD ratio from the USA
L244.ThermalServiceSatiation <- subset( L244.ServiceSatiation_USA, building.service.input %in% thermal_services )
L244.ThermalServiceSatiation <- write_to_all_regions( L244.ThermalServiceSatiation, names_GenericServiceSatiation )
names( L244.ThermalServiceSatiation ) <- sub( "building.service.input", "thermal.building.service.input", names( L244.ThermalServiceSatiation ) )

printlog( "Thermal service satiation is modified in each region according to the HDD/CDD ratio to the USA in a given year")
L244.ThermalServiceSatiation$variable[ L244.ThermalServiceSatiation$thermal.building.service.input %in% heating_services ] <- "HDD"
L244.ThermalServiceSatiation$variable[ L244.ThermalServiceSatiation$thermal.building.service.input %in% cooling_services ] <- "CDD"
L244.ThermalServiceSatiation$degree.days <- L244.HDDCDD_normal_R_Y$value[
  match( vecpaste( L244.ThermalServiceSatiation[ c( "region", "variable" ) ] ),
         vecpaste( L244.HDDCDD_normal_R_Y[ c( "region", "variable" ) ] ) ) ]
L244.ThermalServiceSatiation$satiation_mult <- L244.ThermalServiceSatiation$degree.days /
  L244.ThermalServiceSatiation$degree.days[ L244.ThermalServiceSatiation$region=="USA" ]
L244.ThermalServiceSatiation$satiation.level <- round( L244.ThermalServiceSatiation$satiation.level * L244.ThermalServiceSatiation$satiation_mult,
      digits = digits_calOutput )

#This part here is bad. The service satiation in the final cal year can not be lower than the observed demand, so need to use pmax to set a floor on the quantity
# TODO: fix model code.
# First need to calculate the maximum quantities of demand over the historical time period, expressed per unit floorspace
L244.tmp <- L244.ThermalBaseService
L244.tmp$flsp <- L244.Floorspace$base.building.size[
      match( vecpaste( L244.tmp[ c( names_BldNodes, "year" ) ] ), vecpaste( L244.Floorspace[ c( names_BldNodes, "year" ) ] ) ) ]
L244.tmp$service.per.flsp <- L244.tmp$base.service / L244.tmp$flsp
L244.tmp1 <- subset( L244.tmp, year == max( model_base_years ) )

# Then, match in this quantity into the thermal service satiation and take the max
L244.ThermalServiceSatiation$service.per.flsp <- L244.tmp1$service.per.flsp[
      match( vecpaste( L244.ThermalServiceSatiation[ c( names_BldNodes, "thermal.building.service.input" ) ] ),
             vecpaste( L244.tmp1[ c( names_BldNodes, "thermal.building.service.input" ) ] ) ) ]
L244.ThermalServiceSatiation$satiation.level <- round( pmax( L244.ThermalServiceSatiation$satiation.level, L244.ThermalServiceSatiation$service.per.flsp * 1.0001 ),
      digits = digits_calOutput )
L244.ThermalServiceSatiation <- L244.ThermalServiceSatiation[ names_ThermalServiceSatiation ]

#shell efficiency
L244.shell_eff_R_Y <- interpolate_and_melt(L144.shell_eff_R_Y, model_years, value.name="shell.conductance", digits = digits_efficiency )
L244.shell_eff_R_Y <- add_region_name(L244.shell_eff_R_Y)
L244.shell_eff_R_Y[ bld_nodes_noregion ] <- A44.gcam_consumer[
  match( L244.shell_eff_R_Y$supplysector, A44.gcam_consumer$gcam.consumer ),
  bld_nodes_noregion ]

L244.shell_eff_R_Y$shell.year <- L244.shell_eff_R_Y$year
L244.shell_eff_R_Y$floor.to.surface.ratio <- floor.to.surface.ratio
L244.ShellConductance_bld <- L244.shell_eff_R_Y[ names_ShellConductance ]

#supplysector
L244.Supplysector_bld <- write_to_all_regions( A44.sector, names_Supplysector )

printlog( "L244.FinalEnergyKeyword_bld: Supply sector keywords for detailed building sector" )
L244.FinalEnergyKeyword_bld <- na.omit( write_to_all_regions( A44.sector, names_FinalEnergyKeyword ) )

# Subsector information
## Not all subsectors exist in all regions; tradbio and heat are only modeled in selected regions
## The level1 end-use tech efficiency file has all of the combinations that exist
L244.Tech_bld <- add_region_name( L144.end_use_eff )[ names_Tech ]

#logit
printlog( "L244.SubsectorLogit_bld: Subsector logit exponents of building sector" )
L244.SubsectorLogit_bld <- write_to_all_regions( A44.subsector_logit, names_SubsectorLogit )
L244.SubsectorLogit_bld <- subset( L244.SubsectorLogit_bld,
      paste( region, supplysector, subsector ) %in% paste( L244.Tech_bld$region, L244.Tech_bld$supplysector, L244.Tech_bld$subsector ) )

#shareweight
printlog( "L244.SubsectorShrwt_bld and L244.SubsectorShrwtFllt_bld: Subsector shareweights of building sector" )
if( any( !is.na( A44.subsector_shrwt$year ) ) ){
  L244.SubsectorShrwt_bld <- write_to_all_regions( A44.subsector_shrwt[ !is.na( A44.subsector_shrwt$year ), ], names_SubsectorShrwt )
  L244.SubsectorShrwt_bld <- subset( L244.SubsectorShrwt_bld,
      paste( region, supplysector, subsector ) %in% paste( L244.Tech_bld$region, L244.Tech_bld$supplysector, L244.Tech_bld$subsector ) )
}
if( any( !is.na( A44.subsector_shrwt$year.fillout ) ) ){
  L244.SubsectorShrwtFllt_bld <- write_to_all_regions( A44.subsector_shrwt[ !is.na( A44.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
  L244.SubsectorShrwtFllt_bld <- subset( L244.SubsectorShrwtFllt_bld,
      paste( region, supplysector, subsector ) %in% paste( L244.Tech_bld$region, L244.Tech_bld$supplysector, L244.Tech_bld$subsector ) )
}

#interpolate
printlog( "L244.SubsectorInterp_bld and L244.SubsectorInterpTo_bld: Subsector shareweight interpolation of building sector" )
if( any( is.na( A44.subsector_interp$to.value ) ) ){
  L244.SubsectorInterp_bld <- write_to_all_regions( A44.subsector_interp[ is.na( A44.subsector_interp$to.value ), ], names_SubsectorInterp )
  L244.SubsectorInterp_bld <- subset( L244.SubsectorInterp_bld,
      paste( region, supplysector, subsector ) %in% paste( L244.Tech_bld$region, L244.Tech_bld$supplysector, L244.Tech_bld$subsector ) )
}
if( any( !is.na( A44.subsector_interp$to.value ) ) ){
  L244.SubsectorInterpTo_bld <- write_to_all_regions( A44.subsector_interp[ !is.na( A44.subsector_interp$to.value ), ], names_SubsectorInterpTo )
  L244.SubsectorInterpTo_bld <- subset( L244.SubsectorInterpTo_bld,
      paste( region, supplysector, subsector ) %in% paste( L244.Tech_bld$region, L244.Tech_bld$supplysector, L244.Tech_bld$subsector ) )
}

#fuel preference elasticity
A44.fuelprefElasticity$year.fillout <- min( model_future_years )
L244.FuelPrefElast_bld <- write_to_all_regions( A44.fuelprefElasticity, names_FuelPrefElasticity )
L244.FuelPrefElast_bld <- subset( L244.FuelPrefElast_bld,
      paste( region, supplysector, subsector ) %in% paste( L244.Tech_bld$region, L244.Tech_bld$supplysector, L244.Tech_bld$subsector ) )

#technology 
L244.StubTech_bld <- L244.Tech_bld
names( L244.StubTech_bld ) <- sub( "technology", "stub.technology", names( L244.StubTech_bld ) )

#calibration 

L244.in_EJ_R_bld_serv_F_Yh <- interpolate_and_melt(L144.in_EJ_R_bld_serv_F_Yh, model_base_years, value.name = "calibrated.value", digits = digits_calOutput)
L244.in_EJ_R_bld_serv_F_Yh <- add_region_name(L244.in_EJ_R_bld_serv_F_Yh)

L244.in_EJ_R_bld_serv_F_Yh[c(s_s_t_i,"stub.technology")] <- calibrated_techs_bld_det[
  match( vecpaste(L244.in_EJ_R_bld_serv_F_Yh[c("sector","service", "fuel")]),
         vecpaste(calibrated_techs_bld_det[c("sector","supplysector", "fuel")] ) ),
  c(s_s_t_i, "technology") ]
L244.in_EJ_R_bld_serv_F_Yh$share.weight.year <- L244.in_EJ_R_bld_serv_F_Yh$year
L244.in_EJ_R_bld_serv_F_Yh <- set_subsector_shrwt( L244.in_EJ_R_bld_serv_F_Yh, value.name = "calibrated.value" )
L244.in_EJ_R_bld_serv_F_Yh$tech.share.weight <- ifelse( L244.in_EJ_R_bld_serv_F_Yh$calibrated.value > 0, 1, 0 )
L244.StubTechCalInput_bld <- L244.in_EJ_R_bld_serv_F_Yh[ names_StubTechCalInput ]

#end use effciency 
L244.end_use_eff <- interpolate_and_melt(L144.end_use_eff, model_years, value.name="efficiency", digits = digits_calOutput )
L244.end_use_eff <- add_region_name(L244.end_use_eff)

L244.end_use_eff[c(input,"stub.technology")] <- calibrated_techs_bld_det[
  match( vecpaste(L244.end_use_eff[c(s_s_t)]),
         vecpaste(calibrated_techs_bld_det[c(s_s_t)] ) ),
  c(input, "technology") ]
L244.end_use_eff$market.name <- L244.end_use_eff$region
L244.StubTechEff_bld <- L244.end_use_eff[ names_StubTechEff ]

#shareweight
L244.GlobalTechShrwt_bld <- interpolate_and_melt( A44.globaltech_shrwt, model_years, value.name="share.weight" )
L244.GlobalTechShrwt_bld[ c( "sector.name", "subsector.name" ) ] <- L244.GlobalTechShrwt_bld[ c( supp, subs ) ]
L244.GlobalTechShrwt_bld <- L244.GlobalTechShrwt_bld[ c( names_GlobalTechYr, "share.weight" ) ]

#Costs of global technologies
printlog( "L244.GlobalTechCost_bld: Non-fuel costs of global building technologies" )
L144.NEcost_75USDGJ$input.cost <- round( L144.NEcost_75USDGJ$NEcostPerService, digits_cost)
L244.GlobalTechCost_bld <- repeat_and_add_vector( L144.NEcost_75USDGJ, Y, model_years )
L244.GlobalTechCost_bld[ c( "sector.name", "subsector.name" ) ] <- L244.GlobalTechCost_bld[ c( supp, subs ) ]
L244.GlobalTechCost_bld$minicam.non.energy.input <- "non-energy"
L244.GlobalTechCost_bld <- L244.GlobalTechCost_bld[ names_GlobalTechCost ]

#retirement and shutdown rate
printlog( "NOTE: Retirement and shutdown rates only applied for existing (final cal year) stock")
L244.GlobalTechShutdown_bld <- A44.cost_efficiency
L244.GlobalTechShutdown_bld[[Y]] <- max( model_base_years )
L244.GlobalTechShutdown_bld[ c( "sector.name", "subsector.name" ) ] <- L244.GlobalTechShutdown_bld[ c( supp, subs ) ]
L244.GlobalTechShutdown_bld <- L244.GlobalTechShutdown_bld[ names_GlobalTechShutdown ]

#internal gains output ratio

L244.StubTechIntGainOutputRatio <- interpolate_and_melt( L144.internal_gains, model_years, value="internal.gains.output.ratio", digits = digits_efficiency )
L244.StubTechIntGainOutputRatio <- add_region_name( L244.StubTechIntGainOutputRatio ) 
L244.StubTechIntGainOutputRatio$building.node.input <- calibrated_techs_bld_det[ 
  match( L244.StubTechIntGainOutputRatio$supplysector, calibrated_techs_bld_det$supplysector ), "building.node.input" ]

L244.StubTechIntGainOutputRatio$internal.gains.market.name <- A44.gcam_consumer[ 
  match( L244.StubTechIntGainOutputRatio$building.node.input,
        A44.gcam_consumer$building.node.input ), "internal.gains.market.name" ]

L244.StubTechIntGainOutputRatio <- L244.StubTechIntGainOutputRatio[c(names_TechYr,
                                  "internal.gains.output.ratio", "internal.gains.market.name" ) ]

#internal gains scaling

variable <- c("HDD", "CDD")
value <- c(InternalGainsScalar_USA_h, InternalGainsScalar_USA_c)
US.base.scalar <- data.frame(variable, value)

L244.Intgains_scalar <- L244.ThermalServiceSatiation
L244.Intgains_scalar$variable[ L244.Intgains_scalar$thermal.building.service.input %in% heating_services ] <- "HDD"
L244.Intgains_scalar$variable[ L244.Intgains_scalar$thermal.building.service.input %in% cooling_services ] <- "CDD"
L244.Intgains_scalar$InternalGainsScalar_USA <- US.base.scalar$value[
  match( L244.Intgains_scalar$variable, US.base.scalar$variable ) ]
L244.Intgains_scalar$degree.days <- L244.HDDCDD_normal_R_Y$value[
  match( vecpaste( L244.Intgains_scalar[ c( "region", "variable" ) ] ),
         vecpaste( L244.HDDCDD_normal_R_Y[ c( "region", "variable" ) ] ) ) ]
L244.Intgains_scalar$scalar_mult <- L244.Intgains_scalar$degree.days /
  L244.Intgains_scalar$degree.days[ L244.Intgains_scalar$region=="USA" ]
L244.Intgains_scalar$internal.gains.scalar <- round(
  L244.Intgains_scalar$InternalGainsScalar_USA *
  L244.Intgains_scalar$scalar_mult,
  digits_hddcdd )
L244.Intgains_scalar <- L244.Intgains_scalar[ names_Intgains_scalar ]

#Need to remove any services (supplysectors and building-service-inputs) and intgains trial markets for services that don't exist in any years
L244.DeleteThermalService <- subset(
      aggregate( L244.ThermalBaseService[ "base.service"],
            by=as.list( L244.ThermalBaseService[ c( names_BldNodes, "thermal.building.service.input" ) ] ), max ),
      base.service == 0 )[ c( names_BldNodes, "thermal.building.service.input" ) ]
L244.DeleteThermalService[[supp]] <- L244.DeleteThermalService$thermal.building.service.input
L244.DeleteGenericService <- subset(
      aggregate( L244.GenericBaseService[ "base.service"],
            by=as.list( L244.GenericBaseService[ c( names_BldNodes, "building.service.input" ) ] ), max ),
      base.service == 0 )[ c( names_BldNodes, "building.service.input" ) ]
L244.DeleteGenericService[[supp]] <- L244.DeleteGenericService$building.service.input

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L244.SubregionalShares, "SubregionalShares", "ENERGY_LEVEL2_DATA", "L244.SubregionalShares", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.PriceExp_IntGains, "PriceExp_IntGains", "ENERGY_LEVEL2_DATA", "L244.PriceExp_IntGains", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.Floorspace, "Floorspace", "ENERGY_LEVEL2_DATA", "L244.Floorspace", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.DemandFunction_serv, "DemandFunction_serv", "ENERGY_LEVEL2_DATA", "L244.DemandFunction_serv", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.DemandFunction_flsp, "DemandFunction_flsp", "ENERGY_LEVEL2_DATA", "L244.DemandFunction_flsp", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.Satiation_flsp, "Satiation_flsp", "ENERGY_LEVEL2_DATA", "L244.Satiation_flsp", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.ThermalBaseService, "ThermalBaseService", "ENERGY_LEVEL2_DATA", "L244.ThermalBaseService", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.GenericBaseService, "GenericBaseService", "ENERGY_LEVEL2_DATA", "L244.GenericBaseService", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
#Need to subset and write out the HDD/CDD xmls in a for loop, as we don't know the number or names of scenarios
for( i in 1:max( L244.HDDCDD_scen_R_Y$scenID ) ){
  scenarios <- unique( L244.HDDCDD_scen_R_Y[ c( "scenID", "SRES", "GCM" ) ] )
  scen_strings <- paste( scenarios$SRES, scenarios$GCM, sep = "_" )
  objectname <- paste0( "L244.HDDCDD_", scen_strings[i] )
  object <- subset( L244.HDDCDD, scenID == i )[ names_HDDCDD ] 
  assign( objectname, object )
  batchXMLstring <- paste0( "batch_HDDCDD_",
                            substr( objectname, regexpr( "HDDCDD_", objectname ) + 7, nchar( objectname ) ),
                            ".xml" )
  write_mi_data( object, "HDDCDD", "ENERGY_LEVEL2_DATA", objectname, "ENERGY_XML_BATCH", batchXMLstring )
  XMLstring <- sub( "batch_", "", batchXMLstring )
  insert_file_into_batchxml( "ENERGY_XML_BATCH", batchXMLstring, "ENERGY_XML_FINAL", XMLstring, "", xml_tag="outFile" )
}
write_mi_data( L244.ThermalServiceSatiation, "ThermalServiceSatiation", "ENERGY_LEVEL2_DATA", "L244.ThermalServiceSatiation", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.GenericServiceSatiation, "GenericServiceSatiation", "ENERGY_LEVEL2_DATA", "L244.GenericServiceSatiation", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.Intgains_scalar, "Intgains_scalar", "ENERGY_LEVEL2_DATA", "L244.Intgains_scalar", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.ShellConductance_bld, "ShellConductance", "ENERGY_LEVEL2_DATA", "L244.ShellConductance_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 

write_mi_data( L244.Supplysector_bld, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L244.Supplysector_bld", batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_building_det.xml" ) 
write_mi_data( L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword", "ENERGY_LEVEL2_DATA", "L244.FinalEnergyKeyword_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
 if( exists( "L244.SubsectorShrwt_bld" ) ){
 	write_mi_data( L244.SubsectorShrwt_bld, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L244.SubsectorShrwt_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" )
 	}
 if( exists( "L244.SubsectorShrwtFllt_bld" ) ){
 	write_mi_data( L244.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L244.SubsectorShrwtFllt_bld",
 	               "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
 	}
 if( exists( "L244.SubsectorInterp_bld" ) ) {
 	write_mi_data( L244.SubsectorInterp_bld, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L244.SubsectorInterp_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" )
 	}
 if( exists( "L244.SubsectorInterpTo_bld" ) ) {
 	write_mi_data( L244.SubsectorInterpTo_bld, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L244.SubsectorInterpTo_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" )
 	}
write_mi_data( L244.SubsectorLogit_bld, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L244.SubsectorLogit_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.FuelPrefElast_bld, "FuelPrefElast", "ENERGY_LEVEL2_DATA", "L244.FuelPrefElast_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" )
write_mi_data( L244.StubTech_bld, "StubTech", "ENERGY_LEVEL2_DATA", "L244.StubTech_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.StubTechEff_bld, "StubTechEff", "ENERGY_LEVEL2_DATA", "L244.StubTechEff_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.StubTechCalInput_bld, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L244.StubTechCalInput_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.StubTechIntGainOutputRatio, "StubTechIntGainOutputRatio", "ENERGY_LEVEL2_DATA", "L244.StubTechIntGainOutputRatio", "ENERGY_XML_BATCH", "batch_building_det.xml" )

write_mi_data( L244.GlobalTechShrwt_bld, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L244.GlobalTechShrwt_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" ) 
write_mi_data( L244.GlobalTechCost_bld, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L244.GlobalTechCost_bld", "ENERGY_XML_BATCH", "batch_building_det.xml" )

if( nrow( L244.DeleteThermalService ) > 0 ){
	write_mi_data( L244.DeleteThermalService, "DeleteThermalService", "ENERGY_LEVEL2_DATA", "L244.DeleteThermalService", "ENERGY_XML_BATCH", "batch_building_det.xml" )
}
if( nrow( L244.DeleteGenericService ) > 0 ){
	write_mi_data( L244.DeleteGenericService, "DeleteGenericService", "ENERGY_LEVEL2_DATA", "L244.DeleteGenericService", "ENERGY_XML_BATCH", "batch_building_det.xml" )
}

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_building_det.xml", "ENERGY_XML_FINAL", "building_det.xml", "", xml_tag="outFile" )

logstop()
