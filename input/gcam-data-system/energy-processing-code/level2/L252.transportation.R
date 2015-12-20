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
logstart( "L252.transportation.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Model input for aggregate transportation sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs_trn_agg <- readdata( "ENERGY_MAPPINGS", "calibrated_techs_trn_agg" )
A52.sector <- readdata( "ENERGY_ASSUMPTIONS", "A52.sector" )
A52.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A52.subsector_interp" )
A52.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A52.subsector_logit" )
A52.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A52.subsector_shrwt" )
A52.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A52.globaltech_cost" )
A52.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A52.globaltech_eff" )
A52.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A52.globaltech_shrwt" )
A52.demand <- readdata( "ENERGY_ASSUMPTIONS", "A52.demand" )
L152.in_EJ_R_trn_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L152.in_EJ_R_trn_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Supplysector information
printlog( "L252.Supplysector_trn: Supply sector information for transportation sector" )
L252.SectorLogitTables <- get_logit_fn_tables( A52.sector, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
L252.Supplysector_trn <- write_to_all_regions( A52.sector, names_Supplysector )

printlog( "L252.FinalEnergyKeyword_trn: Supply sector keywords for transportation sector" )
L252.FinalEnergyKeyword_trn <- na.omit( write_to_all_regions( A52.sector, names_FinalEnergyKeyword ) )

# 2b. Subsector information
printlog( "L252.SubsectorLogit_trn: Subsector logit exponents of transportation sector" )
L252.SubsectorLogitTables <- get_logit_fn_tables( A52.subsector_logit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
L252.SubsectorLogit_trn <- write_to_all_regions( A52.subsector_logit, names_SubsectorLogit )

printlog( "L252.SubsectorShrwt_trn and L252.SubsectorShrwtFllt_trn: Subsector shareweights of transportation sector" )
if( any( !is.na( A52.subsector_shrwt$year ) ) ){
	L252.SubsectorShrwt_trn <- write_to_all_regions( A52.subsector_shrwt[ !is.na( A52.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A52.subsector_shrwt$year.fillout ) ) ){
	L252.SubsectorShrwtFllt_trn <- write_to_all_regions( A52.subsector_shrwt[ !is.na( A52.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

printlog( "L252.SubsectorInterp_trn and L252.SubsectorInterpTo_trn: Subsector shareweight interpolation of transportation sector" )
if( any( is.na( A52.subsector_interp$to.value ) ) ){
	L252.SubsectorInterp_trn <- write_to_all_regions( A52.subsector_interp[ is.na( A52.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
if( any( !is.na( A52.subsector_interp$to.value ) ) ){
	L252.SubsectorInterpTo_trn <- write_to_all_regions( A52.subsector_interp[ !is.na( A52.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}

# 2c. Technology information
printlog( "L252.StubTech_trn: Identification of stub technologies of transportation sector" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L252.StubTech_trn <- write_to_all_regions( A52.globaltech_shrwt, names_Tech )
names( L252.StubTech_trn ) <- names_StubTech

printlog( "L252.GlobalTechShrwt_trn: Shareweights of global transportation sector technologies" )
L252.globaltech_shrwt.melt <- interpolate_and_melt( A52.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L252.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L252.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L252.GlobalTechShrwt_trn <- L252.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L252.GlobalTechEff_trn: Energy inputs and coefficients of global transportation energy use and feedstocks technologies" )
L252.globaltech_eff.melt <- interpolate_and_melt( A52.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency", digits = digits_efficiency )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L252.globaltech_eff.melt[ c( "sector.name", "subsector.name" ) ] <- L252.globaltech_eff.melt[ c( "supplysector", "subsector" ) ]
L252.GlobalTechEff_trn <- L252.globaltech_eff.melt[ names_GlobalTechEff ]

#Costs of global technologies
printlog( "L252.GlobalTechCost_trn: Capital costs of global transportation technologies" )
L252.globaltech_cost.melt <- interpolate_and_melt( A52.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost" )
L252.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L252.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L252.GlobalTechCost_trn <- L252.globaltech_cost.melt[ names_GlobalTechCost ]

#Calibration and region-specific data
printlog( "L252.StubTechCalInput_trn: calibrated input of transportation energy use technologies (including cogen)")
L252.in_EJ_R_trn_F_Yh <- interpolate_and_melt( L152.in_EJ_R_trn_F_Yh, model_base_years )
L252.in_EJ_R_trn_F_Yh <- add_region_name( L252.in_EJ_R_trn_F_Yh )
L252.in_EJ_R_trn_F_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs_trn_agg[
      match( vecpaste( L252.in_EJ_R_trn_F_Yh[ S_F ] ),
             vecpaste( calibrated_techs_trn_agg[ S_F ] ) ),
      c( "supplysector", "subsector", "technology" ) ]

#Aggregate as indicated in the supplysector/subsector/technology mapping
L252.in_EJ_R_trn_F_Yh <- aggregate( L252.in_EJ_R_trn_F_Yh[ "value" ],
      by=as.list( L252.in_EJ_R_trn_F_Yh[ c( "region", "supplysector", "subsector", "stub.technology", Y ) ] ), sum )

L252.StubTechCalInput_trn <- L252.in_EJ_R_trn_F_Yh[ names_StubTechYr ]
L252.StubTechCalInput_trn$minicam.energy.input <- A52.globaltech_eff$minicam.energy.input[ 
      match( vecpaste( L252.StubTechCalInput_trn[ c( "subsector", "stub.technology" ) ] ),
             vecpaste( A52.globaltech_eff[ c( "subsector", "technology" ) ] ) ) ]
L252.StubTechCalInput_trn$calibrated.value <- round( L252.in_EJ_R_trn_F_Yh$value, digits_calOutput )
L252.StubTechCalInput_trn$share.weight.year <- L252.StubTechCalInput_trn$year
L252.StubTechCalInput_trn <- set_subsector_shrwt( L252.StubTechCalInput_trn, value.name = "calibrated.value" )
L252.StubTechCalInput_trn$tech.share.weight <- ifelse( L252.StubTechCalInput_trn$calibrated.value > 0, 1, 0 )
L252.StubTechCalInput_trn <- L252.StubTechCalInput_trn[ names_StubTechCalInput ]

printlog( "L252.PerCapitaBased_trn: per-capita based flag for transportation final demand" )
L252.PerCapitaBased_trn <- write_to_all_regions( A52.demand, names_PerCapitaBased )

printlog( "L252.PriceElasticity_trn: price elasticity of transportation final demand" )
#Price elasticities are only applied to future periods. Application in base years will cause solution failure
L252.PriceElasticity_trn <- repeat_and_add_vector( A52.demand, Y, model_future_years )
L252.PriceElasticity_trn <- write_to_all_regions( L252.PriceElasticity_trn, names_PriceElasticity )

printlog( "L252.BaseService_trn: base-year service output of transportation final demand" )
#Base service is equal to the output of the transportation supplysector
L252.caloutput_trn_tech <- L252.StubTechCalInput_trn
L252.caloutput_trn_tech$efficiency <- L252.GlobalTechEff_trn$efficiency[
      match( vecpaste( L252.caloutput_trn_tech[ c( "supplysector", "subsector" , "stub.technology", Y ) ] ),
             vecpaste( L252.GlobalTechEff_trn[ c( "sector.name", "subsector.name", "technology", Y ) ] ) ) ]
L252.caloutput_trn_tech$output <- L252.caloutput_trn_tech$calibrated.value * L252.caloutput_trn_tech$efficiency
L252.caloutput_trn <- aggregate( L252.caloutput_trn_tech[ "output" ], by=as.list( L252.caloutput_trn_tech[ c( "region", "supplysector", Y ) ] ), sum )

L252.BaseService_trn <- data.frame(
      region = L252.caloutput_trn$region,
      energy.final.demand = L252.caloutput_trn$supplysector,
      year = L252.caloutput_trn$year,
      base.service = round( L252.caloutput_trn$output, digits_calOutput ) )

##NOTE: income elasticities are GDP-dependent and are set in the socioeconomics module

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L252.SectorLogitTables) ) {
write_mi_data( L252.SectorLogitTables[[ curr_table ]]$data, L252.SectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L252.", L252.SectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_transportation_agg.xml" )
}
write_mi_data( L252.Supplysector_trn, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L252.Supplysector_trn",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_transportation_agg.xml" ) 
write_mi_data( L252.FinalEnergyKeyword_trn, "FinalEnergyKeyword", "ENERGY_LEVEL2_DATA", "L252.FinalEnergyKeyword_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" ) 
for( curr_table in names ( L252.SubsectorLogitTables ) ) {
write_mi_data( L252.SubsectorLogitTables[[ curr_table ]]$data, L252.SubsectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L252.", L252.SubsectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_transportation_agg.xml" )
}
write_mi_data( L252.SubsectorLogit_trn, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L252.SubsectorLogit_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" ) 
if( exists( "L252.SubsectorShrwt_trn" ) ){
	write_mi_data( L252.SubsectorShrwt_trn, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L252.SubsectorShrwt_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
	}
if( exists( "L252.SubsectorShrwtFllt_trn" ) ){
	write_mi_data( L252.SubsectorShrwtFllt_trn, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L252.SubsectorShrwtFllt_trn",
	               "ENERGY_XML_BATCH", "batch_transportation_agg.xml" ) 
	}
if( exists( "L252.SubsectorInterp_trn" ) ) {
	write_mi_data( L252.SubsectorInterp_trn, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L252.SubsectorInterp_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
	}
if( exists( "L252.SubsectorInterpTo_trn" ) ) {
	write_mi_data( L252.SubsectorInterpTo_trn, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L252.SubsectorInterpTo_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
	}
write_mi_data( L252.StubTech_trn, "StubTech", "ENERGY_LEVEL2_DATA", "L252.StubTech_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
write_mi_data( L252.GlobalTechShrwt_trn, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L252.GlobalTechShrwt_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
write_mi_data( L252.GlobalTechEff_trn, "GlobalTechEff", "ENERGY_LEVEL2_DATA", "L252.GlobalTechEff_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
write_mi_data( L252.GlobalTechCost_trn, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L252.GlobalTechCost_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
write_mi_data( L252.StubTechCalInput_trn, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L252.StubTechCalInput_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
write_mi_data( L252.PerCapitaBased_trn, "PerCapitaBased", "ENERGY_LEVEL2_DATA", "L252.PerCapitaBased_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
write_mi_data( L252.PriceElasticity_trn, "PriceElasticity", "ENERGY_LEVEL2_DATA", "L252.PriceElasticity_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )
write_mi_data( L252.BaseService_trn, "BaseService", "ENERGY_LEVEL2_DATA", "L252.BaseService_trn", "ENERGY_XML_BATCH", "batch_transportation_agg.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_transportation_agg.xml", "ENERGY_XML_FINAL", "transportation_agg.xml", "", xml_tag="outFile" )

logstop()
