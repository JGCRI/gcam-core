
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
logstart( "L224.heat.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "District heat" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A24.sector <- readdata( "ENERGY_ASSUMPTIONS", "A24.sector" )
A24.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A24.subsector_logit" )
A24.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A24.subsector_shrwt" )
A24.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A24.subsector_interp" )
A24.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A24.globaltech_coef" )
A24.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A24.globaltech_cost" )
A24.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A24.globaltech_shrwt" )
L1231.eff_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.eff_R_elec_F_tech_Yh" )
L124.in_EJ_R_heat_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L124.in_EJ_R_heat_F_Yh" )
L124.heatoutratio_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L124.heatoutratio_R_elec_F_tech_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
heat_regions <- A_regions$region[ A_regions$heat == 1]
# 2a. Supplysector information
printlog( "L224.Supplysector_heat: Supply sector information for district heat sectors" )
L224.SectorLogitTables <- get_logit_fn_tables( A24.sector, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
for( curr_table in names( L224.SectorLogitTables ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L224.SectorLogitTables[[ curr_table ]]$data <- subset( L224.SectorLogitTables[[ curr_table ]]$data, region %in% heat_regions )
    }
}
L224.Supplysector_heat <- write_to_all_regions( A24.sector, names_Supplysector )
L224.Supplysector_heat <- subset( L224.Supplysector_heat, region %in% heat_regions)

# 2b. Subsector information
printlog( "L224.SubsectorLogit_heat: Subsector logit exponents of district heat sectors" )
L224.SubsectorLogitTables <- get_logit_fn_tables( A24.subsector_logit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
for( curr_table in names( L224.SubsectorLogitTables ) ) {
    if( curr_table != "EQUIV_TABLE" ) {
        L224.SubsectorLogitTables[[ curr_table ]]$data <- subset( L224.SubsectorLogitTables[[ curr_table ]]$data, region %in% heat_regions )
    }
}
L224.SubsectorLogit_heat <- write_to_all_regions( A24.subsector_logit, names_SubsectorLogit )
L224.SubsectorLogit_heat <- subset( L224.SubsectorLogit_heat, region %in% heat_regions)

printlog( "L224.SubsectorShrwt_heat and L224.SubsectorShrwtFllt_heat: Subsector shareweights of district heat sectors" )
if( any( !is.na( A24.subsector_shrwt$year ) ) ){
	L224.SubsectorShrwt_heat <- write_to_all_regions( A24.subsector_shrwt[ !is.na( A24.subsector_shrwt$year ), ], names_SubsectorShrwt )
	L224.SubsectorShrwt_heat <- subset( L224.SubsectorShrwt_heat, region %in% heat_regions)
	}
if( any( !is.na( A24.subsector_shrwt$year.fillout ) ) ){
	L224.SubsectorShrwtFllt_heat <- write_to_all_regions( A24.subsector_shrwt[ !is.na( A24.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	L224.SubsectorShrwtFllt_heat <- subset( L224.SubsectorShrwtFllt_heat, region %in% heat_regions)
	}

printlog( "L224.SubsectorInterp_heat and L224.SubsectorInterpTo_heat: Subsector shareweight interpolation of district heat sectors" )
if( any( is.na( A24.subsector_interp$to.value ) ) ){
	L224.SubsectorInterp_heat <- write_to_all_regions( A24.subsector_interp[ is.na( A24.subsector_interp$to.value ), ], names_SubsectorInterp )
	L224.SubsectorInterp_heat <- subset( L224.SubsectorInterp_heat, region %in% heat_regions)
	}
if( any( !is.na( A24.subsector_interp$to.value ) ) ){
	L224.SubsectorInterpTo_heat <- write_to_all_regions( A24.subsector_interp[ !is.na( A24.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	L224.SubsectorInterpTo_heat <- subset( L224.SubsectorInterpTo_heat, region %in% heat_regions)
	}

# 2c. Technology information
printlog( "L224.StubTech_heat: Identification of stub technologies of district heat" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L224.StubTech_heat <- write_to_all_regions( A24.globaltech_shrwt, names_Tech )
names( L224.StubTech_heat ) <- names_StubTech
L224.StubTech_heat <- subset( L224.StubTech_heat, region %in% heat_regions)

#Coefficients of global technologies
printlog( "L224.GlobalTechCoef_heat: Energy inputs and coefficients of global technologies for district heat" )
L224.globaltech_coef.melt <- interpolate_and_melt( A24.globaltech_coef, c( model_base_years, model_future_years ), value.name="coefficient", digits = digits_coefficient )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L224.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L224.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L224.GlobalTechCoef_heat <- L224.globaltech_coef.melt[ names_GlobalTechCoef ]

#Costs of global technologies
printlog( "L224.GlobalTechCost_heat: Costs of global technologies for district heat" )
L224.globaltech_cost.melt <- interpolate_and_melt( A24.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost", digits = digits_cost )
L224.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L224.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L224.GlobalTechCost_heat <- L224.globaltech_cost.melt[ names_GlobalTechCost ]

#Shareweights of global technologies
printlog( "L224.GlobalTechShrwt_heat: Shareweights of global technologies for district heat" )
L224.globaltech_shrwt.melt <- interpolate_and_melt( A24.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L224.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L224.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L224.GlobalTechShrwt_heat <- L224.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

#2d. Calibration and region-specific data
L224.in_EJ_R_heat_F_Yh <- interpolate_and_melt( L124.in_EJ_R_heat_F_Yh, model_base_years )
L224.in_EJ_R_heat_F_Yh <- add_region_name( L224.in_EJ_R_heat_F_Yh )
L224.in_EJ_R_heat_F_Yh[ c( "supplysector", "subsector", "stub.technology", "minicam.energy.input" ) ] <- calibrated_techs[
      match( paste( L224.in_EJ_R_heat_F_Yh$sector, L224.in_EJ_R_heat_F_Yh$fuel ),
             paste( calibrated_techs$sector, calibrated_techs$fuel ) ),
      c( "supplysector", "subsector", "technology", "minicam.energy.input" ) ]
L224.in_EJ_R_heat_F_Yh <- subset( L224.in_EJ_R_heat_F_Yh, region %in% heat_regions )

printlog( "L224.StubTechCalInput_heat: calibrated input to district heat")
L224.StubTechCalInput_heat <- L224.in_EJ_R_heat_F_Yh[ c( names_StubTechYr, "minicam.energy.input" ) ]
L224.StubTechCalInput_heat$calibrated.value <- round( L224.in_EJ_R_heat_F_Yh$value, digits_calOutput )
L224.StubTechCalInput_heat$year.share.weight <- L224.StubTechCalInput_heat$year
L224.StubTechCalInput_heat <- set_subsector_shrwt( L224.StubTechCalInput_heat, value.name = "calibrated.value" )
L224.StubTechCalInput_heat$share.weight <- ifelse( L224.StubTechCalInput_heat$calibrated.value > 0, 1, 0 )

#Secondary output of heat, applied to electricity generation technologies
#NOTE: This is complicated. Initially tried using historical information for all model periods that fall within historical time
# (i.e. not just the model base years). However for regions like the FSU where historical periods often have very low output of heat
# from the district heat sector, and most heat as a secondary output from the electricity sector, the secondary output heat can easily
# exceed the demands from the end-use sectors, causing model solution failure. For this reason, the convention applied here is to
# use the secondary output of heat from the power sector only in the model base years.
L224.heatoutratio_R_elec_F_tech_Yh <- interpolate_and_melt( L124.heatoutratio_R_elec_F_tech_Yh, model_base_years )
L224.heatoutratio_R_elec_F_tech_Yh <- add_region_name( L224.heatoutratio_R_elec_F_tech_Yh )
L224.heatoutratio_R_elec_F_tech_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L224.heatoutratio_R_elec_F_tech_Yh$sector, L224.heatoutratio_R_elec_F_tech_Yh$fuel, L224.heatoutratio_R_elec_F_tech_Yh$technology ),
             paste( calibrated_techs$sector, calibrated_techs$fuel, calibrated_techs$technology ) ),
      c( "supplysector", "subsector", "technology" ) ]
L224.heatoutratio_R_elec_F_tech_Yh$secondary.output.name <- A24.sector$supplysector[1]

printlog( "L224.StubTechSecOut_elec: secondary output of district heat from electricity technologies")
L224.StubTechSecOut_elec <- L224.heatoutratio_R_elec_F_tech_Yh[ c( names_StubTechYr, "secondary.output.name" ) ]
L224.StubTechSecOut_elec$secondary.output <- round( L224.heatoutratio_R_elec_F_tech_Yh$value, digits_calOutput )

#Calculate cost adjustment, equal to the output of heat multiplied by the heat price (to minimize the distortion of including the secondary output)
L224.StubTechCost_elec <- L224.StubTechSecOut_elec[ names_StubTechYr ]
L224.StubTechCost_elec$minicam.non.energy.input <- "heat plant"
L224.StubTechCost_elec$input.cost <- round( L224.StubTechSecOut_elec$secondary.output * heat_price, digits_cost )

## NOTE: For gas-electric technologies whose efficiencies are below the default assumptions, this cost needs to be reduced
L224.eff_R_elec_F_tech_Y <- melt( L1231.eff_R_elec_F_tech_Yh,
      id.vars = R_S_F_tech,
      measure.vars = X_model_base_years,
      variable.name = Y,
      value.name = "efficiency" )
L224.eff_R_elec_F_tech_Y$year <- as.numeric( sub( "X", "", L224.eff_R_elec_F_tech_Y$year ) )
L224.eff_R_elec_F_tech_Y <- add_region_name( L224.eff_R_elec_F_tech_Y )
L224.eff_Rh_elec_gas_sc_Y <- subset( L224.eff_R_elec_F_tech_Y, region %in% heat_regions & fuel == "gas" & efficiency < default_electric_efficiency )
L224.eff_Rh_elec_gas_sc_Y$cost_modifier <- gas_price * ( 1 / default_electric_efficiency - 1 / L224.eff_Rh_elec_gas_sc_Y$efficiency )

#Modify the costs (don't read this iteratively)
L224.StubTechCost_elec$cost_modifier <- NA
L224.StubTechCost_elec$cost_modifier <- L224.eff_Rh_elec_gas_sc_Y$cost_modifier[
      match( vecpaste( L224.StubTechCost_elec[ c( "region", "subsector", "stub.technology", "year" ) ] ),
             vecpaste( L224.eff_Rh_elec_gas_sc_Y[ c( "region", "fuel", "technology", "year" ) ] ) ) ]
L224.StubTechCost_elec$input.cost[ !is.na( L224.StubTechCost_elec$cost_modifier ) ] <-
   round( pmax( 0,
      L224.StubTechCost_elec$input.cost[ !is.na( L224.StubTechCost_elec$cost_modifier ) ] +
      L224.StubTechCost_elec$cost_modifier[ !is.na( L224.StubTechCost_elec$cost_modifier ) ] ),
   digits_cost )
L224.StubTechCost_elec$cost_modifier <- NULL

#Need to fill out object names for all model time periods
L224.StubTechCost_elec_fut <- repeat_and_add_vector( subset( L224.StubTechCost_elec, year == max( year ) ), Y, model_future_years )
L224.StubTechCost_elec_fut$input.cost <- 0
L224.StubTechCost_elec <- rbind( L224.StubTechCost_elec, L224.StubTechCost_elec_fut )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L224.SectorLogitTables) ) {
write_mi_data( L224.SectorLogitTables[[ curr_table ]]$data, L224.SectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L224.", L224.SectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_heat.xml" )
}
write_mi_data( L224.Supplysector_heat, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L224.Supplysector_heat",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_heat.xml" ) 
for( curr_table in names ( L224.SubsectorLogitTables ) ) {
write_mi_data( L224.SubsectorLogitTables[[ curr_table ]]$data, L224.SubsectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L224.", L224.SubsectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_heat.xml" )
}
write_mi_data( L224.SubsectorLogit_heat, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L224.SubsectorLogit_heat", "ENERGY_XML_BATCH", "batch_heat.xml" ) 
if( exists( "L224.SubsectorShrwt_heat" ) ){
	write_mi_data( L224.SubsectorShrwt_heat, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L224.SubsectorShrwt_heat", "ENERGY_XML_BATCH", "batch_heat.xml" )
	}
if( exists( "L224.SubsectorShrwtFllt_heat" ) ){
	write_mi_data( L224.SubsectorShrwtFllt_heat, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L224.SubsectorShrwtFllt_heat",
	               "ENERGY_XML_BATCH", "batch_heat.xml" ) 
	}
if( exists( "L224.SubsectorInterp_heat" ) ) {
	write_mi_data( L224.SubsectorInterp_heat, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L224.SubsectorInterp_heat", "ENERGY_XML_BATCH", "batch_heat.xml" )
	}
if( exists( "L224.SubsectorInterpTo_heat" ) ) {
	write_mi_data( L224.SubsectorInterpTo_heat, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L224.SubsectorInterpTo_heat", "ENERGY_XML_BATCH", "batch_heat.xml" )
	}
write_mi_data( L224.StubTech_heat, "StubTech", "ENERGY_LEVEL2_DATA", "L224.StubTech_heat", "ENERGY_XML_BATCH", "batch_heat.xml" )
write_mi_data( L224.GlobalTechCoef_heat, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L224.GlobalTechCoef_heat", "ENERGY_XML_BATCH", "batch_heat.xml" )
write_mi_data( L224.GlobalTechCost_heat, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L224.GlobalTechCost_heat", "ENERGY_XML_BATCH", "batch_heat.xml" )
write_mi_data( L224.GlobalTechShrwt_heat, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L224.GlobalTechShrwt_heat", "ENERGY_XML_BATCH", "batch_heat.xml" )
write_mi_data( L224.StubTechCalInput_heat, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L224.StubTechCalInput_heat", "ENERGY_XML_BATCH", "batch_heat.xml" )
write_mi_data( L224.StubTechSecOut_elec, "StubTechSecOut", "ENERGY_LEVEL2_DATA", "L224.StubTechSecOut_elec", "ENERGY_XML_BATCH", "batch_heat.xml" )
write_mi_data( L224.StubTechCost_elec, "StubTechCost", "ENERGY_LEVEL2_DATA", "L224.StubTechCost_elec", "ENERGY_XML_BATCH", "batch_heat.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_heat.xml", "ENERGY_XML_FINAL", "heat.xml", "", xml_tag="outFile" )

logstop()


