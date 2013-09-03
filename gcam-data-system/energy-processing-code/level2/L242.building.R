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
logstart( "L242.building.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Model input for aggregate building sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs_bld_agg <- readdata( "ENERGY_MAPPINGS", "calibrated_techs_bld_agg" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A42.sector <- readdata( "ENERGY_ASSUMPTIONS", "A42.sector" )
A42.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A42.subsector_interp" )
A42.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A42.subsector_logit" )
A42.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A42.subsector_shrwt" )
A42.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A42.globaltech_cost" )
A42.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A42.globaltech_eff" )
A42.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A42.globaltech_shrwt" )
A42.globaltech_interp <- readdata( "ENERGY_ASSUMPTIONS", "A42.globaltech_interp" )
A42.fuelprefElasticity <- readdata( "ENERGY_ASSUMPTIONS", "A42.fuelprefElasticity" )
A42.demand <- readdata( "ENERGY_ASSUMPTIONS", "A42.demand" )
L124.in_EJ_R_heat_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L124.in_EJ_R_heat_F_Yh" )
L142.in_EJ_R_bld_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L142.in_EJ_R_bld_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Build table to drop heat subsectors and technologies in regions where heat is not modeled as a separate fuel
L242.heat_techs <- unique( calibrated_techs_bld_agg[ grepl( "bld", calibrated_techs_bld_agg$sector ) & calibrated_techs_bld_agg$fuel == "heat", s_s_t ] )
L242.rm_heat_techs_R <- repeat_and_add_vector( L242.heat_techs, R, A_regions[[R]][ A_regions$heat == 0 ] )
L242.rm_heat_techs_R <- add_region_name( L242.rm_heat_techs_R )

# 2a. Supplysector information
printlog( "L242.Supplysector_bld: Supply sector information for building sector" )
L242.Supplysector_bld <- write_to_all_regions( A42.sector, names_Supplysector )

# 2b. Subsector information
printlog( "L242.SubsectorLogit_bld: Subsector logit exponents of building sector" )
L242.SubsectorLogit_bld <- write_to_all_regions( A42.subsector_logit, names_SubsectorLogit )
L242.SubsectorLogit_bld <- L242.SubsectorLogit_bld[
      vecpaste( L242.SubsectorLogit_bld[ c( "region", "subsector" ) ] ) %!in% vecpaste( L242.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]

printlog( "L242.SubsectorShrwt_bld and L242.SubsectorShrwtFllt_bld: Subsector shareweights of building sector" )
if( any( !is.na( A42.subsector_shrwt$year ) ) ){
	L242.SubsectorShrwt_bld <- write_to_all_regions( A42.subsector_shrwt[ !is.na( A42.subsector_shrwt$year ), ], names_SubsectorShrwt )
	#Remove non-existent heat subsectors from each region
	L242.SubsectorShrwt_bld <- L242.SubsectorShrwt_bld[
      vecpaste( L242.SubsectorShrwt_bld[ c( "region", "subsector" ) ] ) %!in% vecpaste( L242.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
	}
if( any( !is.na( A42.subsector_shrwt$year.fillout ) ) ){
	L242.SubsectorShrwtFllt_bld <- write_to_all_regions( A42.subsector_shrwt[ !is.na( A42.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	#Remove non-existent heat subsectors from each region
	L242.SubsectorShrwtFllt_bld <- L242.SubsectorShrwtFllt_bld[
      vecpaste( L242.SubsectorShrwtFllt_bld[ c( "region", "subsector" ) ] ) %!in% vecpaste( L242.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
	}

printlog( "L242.SubsectorInterp_bld and L242.SubsectorInterpTo_bld: Subsector shareweight interpolation of building sector" )
if( any( is.na( A42.subsector_interp$to.value ) ) ){
	L242.SubsectorInterp_bld <- write_to_all_regions( A42.subsector_interp[ is.na( A42.subsector_interp$to.value ), ], names_SubsectorInterp )
	#Remove non-existent heat subsectors from each region
	L242.SubsectorInterp_bld <- L242.SubsectorInterp_bld[
      vecpaste( L242.SubsectorInterp_bld[ c( "region", "subsector" ) ] ) %!in% vecpaste( L242.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
	}
if( any( !is.na( A42.subsector_interp$to.value ) ) ){
	L242.SubsectorInterpTo_bld <- write_to_all_regions( A42.subsector_interp[ !is.na( A42.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	#Remove non-existent heat subsectors from each region
	L242.SubsectorInterpTo_bld <- L242.SubsectorInterpTo_bld[
      vecpaste( L242.SubsectorInterpTo_bld[ c( "region", "subsector" ) ] ) %!in% vecpaste( L242.rm_heat_techs_R[ c( "region", "subsector" ) ] ), ]
	}

# 2c. Technology information
printlog( "L242.StubTech_bld: Identification of stub technologies of building sector" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L242.StubTech_bld <- write_to_all_regions( A42.globaltech_shrwt, names_Tech )
names( L242.StubTech_bld ) <- names_StubTech
#Drop heat as a stub-technology in regions where it is not modeled as a fuel
L242.StubTech_bld <- L242.StubTech_bld[
      vecpaste( L242.StubTech_bld[ c( "region", "stub.technology" ) ] ) %!in% vecpaste( L242.rm_heat_techs_R[ c( "region", "technology" ) ] ), ]

printlog( "L242.GlobalTechShrwt_bld: Shareweights of global building sector technologies" )
L242.globaltech_shrwt.melt <- interpolate_and_melt( A42.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L242.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L242.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L242.GlobalTechShrwt_bld <- L242.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L242.GlobalTechInterp_bld: Global technology shareweight interpolation of building sector" )
L242.GlobalTechInterp_bld <- set_years( A42.globaltech_interp )
L242.GlobalTechInterp_bld[ c( "sector.name", "subsector.name" ) ] <- L242.GlobalTechInterp_bld[ c( "supplysector", "subsector" ) ]
L242.GlobalTechInterp_bld <- L242.GlobalTechInterp_bld[ names_GlobalTechInterp ]

printlog( "L242.GlobalTechEff_bld: Energy inputs and coefficients of global building energy use and feedstocks technologies" )
L242.globaltech_eff.melt <- interpolate_and_melt( A42.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L242.globaltech_eff.melt[ c( "sector.name", "subsector.name" ) ] <- L242.globaltech_eff.melt[ c( "supplysector", "subsector" ) ]
L242.GlobalTechEff_bld <- L242.globaltech_eff.melt[ names_GlobalTechEff ]
L242.GlobalTechEff_bld$efficiency <- round( L242.GlobalTechEff_bld$efficiency, digits_efficiency )

#Costs of global technologies
printlog( "L242.GlobalTechCost_bld: Capital costs of global building technologies" )
L242.globaltech_cost.melt <- interpolate_and_melt( A42.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost" )
L242.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L242.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L242.GlobalTechCost_bld <- L242.globaltech_cost.melt[ names_GlobalTechCost ]

#Calibration and region-specific data
printlog( "L242.StubTechCalInput_bld: calibrated input of building energy use technologies (including cogen)")
L242.in_EJ_R_bld_F_Yh <- interpolate_and_melt( L142.in_EJ_R_bld_F_Yh, model_base_years )
L242.in_EJ_R_bld_F_Yh <- add_region_name( L242.in_EJ_R_bld_F_Yh )
L242.in_EJ_R_bld_F_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs_bld_agg[
      match( vecpaste( L242.in_EJ_R_bld_F_Yh[ S_F ] ),
             vecpaste( calibrated_techs_bld_agg[ S_F ] ) ),
      c( "supplysector", "subsector", "technology" ) ]

#Aggregate as indicated in the supplysector/subsector/technology mapping
L242.in_EJ_R_bld_F_Yh <- aggregate( L242.in_EJ_R_bld_F_Yh[ "value" ],
      by=as.list( L242.in_EJ_R_bld_F_Yh[ c( "region", "supplysector", "subsector", "stub.technology", Y ) ] ), sum )

L242.StubTechCalInput_bld <- L242.in_EJ_R_bld_F_Yh[ names_StubTechYr ]
L242.StubTechCalInput_bld$minicam.energy.input <- A42.globaltech_eff$minicam.energy.input[ 
      match( vecpaste( L242.StubTechCalInput_bld[ c( "subsector", "stub.technology" ) ] ),
             vecpaste( A42.globaltech_eff[ c( "subsector", "technology" ) ] ) ) ]
L242.StubTechCalInput_bld$calibrated.value <- round( L242.in_EJ_R_bld_F_Yh$value, digits_calOutput )
L242.StubTechCalInput_bld$share.weight.year <- L242.StubTechCalInput_bld$year
L242.StubTechCalInput_bld <- set_subsector_shrwt( L242.StubTechCalInput_bld, value.name = "calibrated.value" )
L242.StubTechCalInput_bld$tech.share.weight <- ifelse( L242.StubTechCalInput_bld$calibrated.value > 0, 1, 0 )
L242.StubTechCalInput_bld <- L242.StubTechCalInput_bld[ names_StubTechCalInput ]

printlog( "L242.FuelPrefElast_bld: fuel preference elasticities of building energy use" )
A42.fuelprefElasticity$year.fillout <- min( model_future_years )
L242.FuelPrefElast_bld <- write_to_all_regions( A42.fuelprefElasticity, names_FuelPrefElasticity )

printlog( "L242.PerCapitaBased_bld: per-capita based flag for building final demand" )
L242.PerCapitaBased_bld <- write_to_all_regions( A42.demand, names_PerCapitaBased )

printlog( "L242.PriceElasticity_bld: price elasticity of building final demand" )
#Price elasticities are only applied to future periods. Application in base years will cause solution failure
L242.PriceElasticity_bld <- repeat_and_add_vector( A42.demand, Y, model_future_years )
L242.PriceElasticity_bld <- write_to_all_regions( L242.PriceElasticity_bld, names_PriceElasticity )

printlog( "L242.BaseService_bld: base-year service output of building final demand" )
#Base service is equal to the output of the building supplysector
L242.caloutput_bld_tech <- L242.StubTechCalInput_bld
L242.caloutput_bld_tech$efficiency <- L242.GlobalTechEff_bld$efficiency[
      match( vecpaste( L242.caloutput_bld_tech[ c( "supplysector", "subsector" , "stub.technology", Y ) ] ),
             vecpaste( L242.GlobalTechEff_bld[ c( "sector.name", "subsector.name", "technology", Y ) ] ) ) ]
L242.caloutput_bld_tech$output <- L242.caloutput_bld_tech$calibrated.value * L242.caloutput_bld_tech$efficiency
L242.caloutput_bld <- aggregate( L242.caloutput_bld_tech[ "output" ], by=as.list( L242.caloutput_bld_tech[ c( "region", "supplysector", Y ) ] ), sum )

L242.BaseService_bld <- data.frame(
      region = L242.caloutput_bld$region,
      energy.final.demand = L242.caloutput_bld$supplysector,
      year = L242.caloutput_bld$year,
      base.service = round( L242.caloutput_bld$output, digits_calOutput ) )

##NOTE: income elasticities are GDP-dependent and are set in the socioeconomics module

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L242.Supplysector_bld, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L242.Supplysector_bld",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_building_agg.xml" ) 
write_mi_data( L242.SubsectorLogit_bld, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L242.SubsectorLogit_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" ) 
if( exists( "L242.SubsectorShrwt_bld" ) ){
	write_mi_data( L242.SubsectorShrwt_bld, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L242.SubsectorShrwt_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
	}
if( exists( "L242.SubsectorShrwtFllt_bld" ) ){
	write_mi_data( L242.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L242.SubsectorShrwtFllt_bld",
	               "ENERGY_XML_BATCH", "batch_building_agg.xml" ) 
	}
if( exists( "L242.SubsectorInterp_bld" ) ) {
	write_mi_data( L242.SubsectorInterp_bld, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L242.SubsectorInterp_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
	}
if( exists( "L242.SubsectorInterpTo_bld" ) ) {
	write_mi_data( L242.SubsectorInterpTo_bld, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L242.SubsectorInterpTo_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
	}
write_mi_data( L242.StubTech_bld, "StubTech", "ENERGY_LEVEL2_DATA", "L242.StubTech_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.GlobalTechInterp_bld, "GlobalTechInterp", "ENERGY_LEVEL2_DATA", "L242.GlobalTechInterp_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.GlobalTechShrwt_bld, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L242.GlobalTechShrwt_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.GlobalTechEff_bld, "GlobalTechEff", "ENERGY_LEVEL2_DATA", "L242.GlobalTechEff_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.GlobalTechCost_bld, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L242.GlobalTechCost_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.StubTechCalInput_bld, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L242.StubTechCalInput_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.FuelPrefElast_bld, "FuelPrefElast", "ENERGY_LEVEL2_DATA", "L242.FuelPrefElast_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.PerCapitaBased_bld, "PerCapitaBased", "ENERGY_LEVEL2_DATA", "L242.PerCapitaBased_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.PriceElasticity_bld, "PriceElasticity", "ENERGY_LEVEL2_DATA", "L242.PriceElasticity_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )
write_mi_data( L242.BaseService_bld, "BaseService", "ENERGY_LEVEL2_DATA", "L242.BaseService_bld", "ENERGY_XML_BATCH", "batch_building_agg.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_building_agg.xml", "ENERGY_XML_FINAL", "building_agg.xml", "", xml_tag="outFile" )

logstop()
