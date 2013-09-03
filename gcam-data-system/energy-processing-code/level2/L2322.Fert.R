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
logstart( "L2322.Fert.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Model input for fertilizer sector" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A322.sector <- readdata( "ENERGY_ASSUMPTIONS", "A322.sector" )
A322.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A322.subsector_interp" )
A322.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A322.subsector_logit" )
A322.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A322.subsector_shrwt" )
A322.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A322.globaltech_coef" )
A322.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A322.globaltech_shrwt" )
A322.globaltech_co2capture <- readdata( "ENERGY_ASSUMPTIONS", "A322.globaltech_co2capture" )
A322.globaltech_renew <- readdata( "ENERGY_ASSUMPTIONS", "A322.globaltech_renew" )
L1322.Fert_Prod_MtN_R_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L1322.Fert_Prod_MtN_R_F_Y")
L1322.IO_R_Fert_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.IO_R_Fert_F_Yh" )
L1322.Fert_NEcost_75USDkgN_F <- readdata( "ENERGY_LEVEL1_DATA", "L1322.Fert_NEcost_75USDkgN_F" )
L142.ag_Fert_NetExp_MtN_R_Y <- readdata( "AGLU_LEVEL1_DATA", "L142.ag_Fert_NetExp_MtN_R_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
# 2a. Supplysector information
printlog( "L2322.Supplysector_Fert: Supply sector information for fertilizer sector" )
L2322.Supplysector_Fert <- write_to_all_regions( A322.sector, names_Supplysector )

# 2b. Subsector information
printlog( "L2322.SubsectorLogit_Fert: Subsector logit exponents of fertilizer sector" )
L2322.SubsectorLogit_Fert <- write_to_all_regions( A322.subsector_logit, names_SubsectorLogit )

printlog( "L2322.SubsectorShrwt_Fert and L2322.SubsectorShrwtFllt_Fert: Subsector shareweights of fertilizer sector" )
if( any( !is.na( A322.subsector_shrwt$year ) ) ){
	L2322.SubsectorShrwt_Fert <- write_to_all_regions( A322.subsector_shrwt[ !is.na( A322.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A322.subsector_shrwt$year.fillout ) ) ){
	L2322.SubsectorShrwtFllt_Fert <- write_to_all_regions( A322.subsector_shrwt[ !is.na( A322.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

printlog( "L2322.SubsectorInterp_Fert and L2322.SubsectorInterpTo_Fert: Subsector shareweight interpolation of fertilizer sector" )
if( any( is.na( A322.subsector_interp$to.value ) ) ){
	L2322.SubsectorInterp_Fert <- write_to_all_regions( A322.subsector_interp[ is.na( A322.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
if( any( !is.na( A322.subsector_interp$to.value ) ) ){
	L2322.SubsectorInterpTo_Fert <- write_to_all_regions( A322.subsector_interp[ !is.na( A322.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}

# 2c. Technology information
printlog( "L2322.StubTech_Fert: Identification of stub technologies of fertilizer sector" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L2322.StubTech_Fert <- write_to_all_regions( A322.globaltech_shrwt, names_Tech )
names( L2322.StubTech_Fert ) <- names_StubTech

printlog( "L2322.GlobalTechShrwt_Fert: Shareweights of global fertilizer sector technologies" )
L2322.globaltech_shrwt.melt <- interpolate_and_melt( A322.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L2322.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L2322.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L2322.GlobalTechShrwt_Fert <- L2322.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L2322.GlobalTechCoef_Fert: Energy inputs and coefficients of global fertilizer energy use and feedstocks technologies" )
L2322.globaltech_coef.melt <- interpolate_and_melt( A322.globaltech_coef, c( model_base_years, model_future_years ), value.name="coefficient" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L2322.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L2322.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L2322.GlobalTechCoef_Fert <- L2322.globaltech_coef.melt[ names_GlobalTechCoef ]
L2322.GlobalTechCoef_Fert$coefficient <- round( L2322.GlobalTechCoef_Fert$coefficient, digits_coefficient )

#Costs of global technologies
printlog( "L2322.GlobalTechCost_Fert: Non-energy costs of global fertilizer manufacturing technologies" )
L2322.GlobalTechCost_Fert <- L2322.GlobalTechCoef_Fert[ names_GlobalTechYr ]
L2322.GlobalTechCost_Fert$minicam.non.energy.input <- "non-energy"
L2322.GlobalTechCost_Fert$input.cost <- round( L1322.Fert_NEcost_75USDkgN_F$NEcost_75USDkgN[
      match( L2322.GlobalTechCost_Fert$technology, L1322.Fert_NEcost_75USDkgN_F$fuel ) ],
      digits_cost )
#Export technologies have no cost assigned. Just drop the object
L2322.GlobalTechCost_Fert <- na.omit( L2322.GlobalTechCost_Fert )

#Carbon capture rates from technologies with CCS
printlog( "L2322.GlobalTechCapture_Fert: CO2 capture fractions from global fertilizer production technologies with CCS" )
## No need to consider historical periods or intermittent technologies here
L2322.globaltech_co2capture.melt <- interpolate_and_melt( A322.globaltech_co2capture, model_future_years, value.name="remove.fraction" )
L2322.globaltech_co2capture.melt[ c( "sector.name", "subsector.name" ) ] <- L2322.globaltech_co2capture.melt[ c( "supplysector", "subsector" ) ]
L2322.GlobalTechCapture_Fert <- data.frame(
      L2322.globaltech_co2capture.melt[ names_GlobalTechYr ],
      remove.fraction = round( L2322.globaltech_co2capture.melt$remove.fraction, digits = digits_remove.fraction ) )
L2322.GlobalTechCapture_Fert$storage.market <- CO2.storage.market

#Calibration and region-specific data
printlog( "L2322.StubTechProd_Fert: calibrated output of fertilizer technologies" )
L2322.StubTechProd_Fert <- interpolate_and_melt( L1322.Fert_Prod_MtN_R_F_Y, model_base_years, value.name = "calOutputValue" )
L2322.StubTechProd_Fert <- add_region_name( L2322.StubTechProd_Fert )
L2322.StubTechProd_Fert[ s_s_t ] <- calibrated_techs[ match( vecpaste( L2322.StubTechProd_Fert[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ), s_s_t ]
L2322.StubTechProd_Fert$stub.technology <- L2322.StubTechProd_Fert$technology
L2322.StubTechProd_Fert$calOutputValue <- round( L2322.StubTechProd_Fert$calOutputValue, digits_calOutput )
L2322.StubTechProd_Fert$share.weight.year <- L2322.StubTechProd_Fert[[Y]]
L2322.StubTechProd_Fert$subs.share.weight <- ifelse( L2322.StubTechProd_Fert$calOutputValue > 0, 1, 0 )
L2322.StubTechProd_Fert$tech.share.weight <- L2322.StubTechProd_Fert$subs.share.weight
L2322.StubTechProd_Fert <- L2322.StubTechProd_Fert[ names_StubTechProd ]

printlog( "L2322.StubTechCoef_Fert: calibrated base-year coefficients of fertilizer production technologies" )
L2322.StubTechCoef_Fert <- interpolate_and_melt( L1322.IO_R_Fert_F_Yh, model_base_years, value.name = "coefficient" )
#Where 0, drop from this table (to revert to assumed defaults)
L2322.StubTechCoef_Fert <- subset( L2322.StubTechCoef_Fert, coefficient != 0 )
L2322.StubTechCoef_Fert <- add_region_name( L2322.StubTechCoef_Fert )
L2322.StubTechCoef_Fert[ s_s_t_i ] <- calibrated_techs[ match( vecpaste( L2322.StubTechCoef_Fert[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ), s_s_t_i ]
L2322.StubTechCoef_Fert$stub.technology <- L2322.StubTechCoef_Fert$technology
L2322.StubTechCoef_Fert$coefficient <- round( L2322.StubTechCoef_Fert$coefficient, digits_coefficient )
L2322.StubTechCoef_Fert$market.name <- L2322.StubTechCoef_Fert$region
L2322.StubTechCoef_Fert <- L2322.StubTechCoef_Fert[ names_StubTechCoef ]

printlog( "L2322.StubTechFixOut_Fert_imp: fixed output of import technology (fixed imports)" )
#Imports are negative net exports
fixout_years_base <- historical_years[ historical_years %in% c( model_base_years, model_future_years ) ]
fixout_years_fut <- model_future_years[ model_future_years %!in% fixout_years_base ]
L2322.StubTechFixOut_Fert_imp_base <- interpolate_and_melt( L142.ag_Fert_NetExp_MtN_R_Y, fixout_years_base, value.name = "fixedOutput" )
L2322.StubTechFixOut_Fert_imp_base <- add_region_name( L2322.StubTechFixOut_Fert_imp_base )
L2322.StubTechFixOut_Fert_imp_base[ c( "supplysector", "subsector", "stub.technology" ) ] <- A322.globaltech_renew[
      rep( 1, times = nrow( L2322.StubTechFixOut_Fert_imp_base ) ), s_s_t ]
L2322.StubTechFixOut_Fert_imp_base$fixedOutput <- round( pmax( 0, -1 * L2322.StubTechFixOut_Fert_imp_base$fixedOutput ), digits_calOutput )
L2322.StubTechFixOut_Fert_imp_base$share.weight.year <- L2322.StubTechFixOut_Fert_imp_base$year
L2322.StubTechFixOut_Fert_imp_base[ c( "subs.share.weight", "tech.share.weight" ) ] <- 0

#Repeat final year to all future years and rbind
L2322.StubTechFixOut_Fert_imp_fut <- repeat_and_add_vector(
      L2322.StubTechFixOut_Fert_imp_base[ L2322.StubTechFixOut_Fert_imp_base[[Y]] == max( fixout_years_base ), ], Y, fixout_years_fut )
L2322.StubTechFixOut_Fert_imp <- rbind( L2322.StubTechFixOut_Fert_imp_base, L2322.StubTechFixOut_Fert_imp_fut )[ names_StubTechFixOut ]

printlog( "L2322.StubTechFixOut_Fert_exp: fixed output of import technology (fixed imports)" )
#Exports are positive net exports
L2322.StubTechFixOut_Fert_exp_base <- interpolate_and_melt( L142.ag_Fert_NetExp_MtN_R_Y, fixout_years_base, value.name = "fixedOutput" )
L2322.StubTechFixOut_Fert_exp_base <- add_region_name( L2322.StubTechFixOut_Fert_exp_base )
L2322.StubTechFixOut_Fert_exp_base[ c( "supplysector", "subsector", "stub.technology" ) ] <- A322.globaltech_shrwt[
      grepl( "Exports", A322.globaltech_shrwt$supplysector ), s_s_t ][
          rep( 1, times = nrow( L2322.StubTechFixOut_Fert_exp_base ) ), ]
L2322.StubTechFixOut_Fert_exp_base$fixedOutput <- round( pmax( 0, L2322.StubTechFixOut_Fert_exp_base$fixedOutput ), digits_calOutput )
L2322.StubTechFixOut_Fert_exp_base$share.weight.year <- L2322.StubTechFixOut_Fert_exp_base$year
L2322.StubTechFixOut_Fert_exp_base[ c( "subs.share.weight", "tech.share.weight" ) ] <- 0

#Repeat final year to all future years and rbind
L2322.StubTechFixOut_Fert_exp_fut <- repeat_and_add_vector(
      L2322.StubTechFixOut_Fert_exp_base[ L2322.StubTechFixOut_Fert_exp_base[[Y]] == max( fixout_years_base ), ], Y, fixout_years_fut )
L2322.StubTechFixOut_Fert_exp <- rbind( L2322.StubTechFixOut_Fert_exp_base, L2322.StubTechFixOut_Fert_exp_fut )[ names_StubTechFixOut ]

printlog( "L2322.PerCapitaBased_Fert: per-capita based flag for fertilizer exports final demand" )
L2322.PerCapitaBased_Fert <- data.frame(
      region = GCAM_region_names$region,
      energy.final.demand = A322.sector$supplysector[ grepl( "Exports", A322.sector$supplysector ) ],
      perCapitaBased = 0 )

printlog( "L2322.BaseService_Fert: base-year service output of fertilizer exports final demand" )
#Base service is equal to the output of the exports supplysector
L2322.BaseService_Fert <- data.frame(
      region = L2322.StubTechFixOut_Fert_exp_base$region,
      energy.final.demand = L2322.StubTechFixOut_Fert_exp_base$supplysector,
      year = L2322.StubTechFixOut_Fert_exp_base$year,
      base.service = L2322.StubTechFixOut_Fert_exp_base$fixedOutput )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2322.Supplysector_Fert, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L2322.Supplysector_Fert",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_en_Fert.xml" ) 
write_mi_data( L2322.SubsectorLogit_Fert, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L2322.SubsectorLogit_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" ) 
if( exists( "L2322.SubsectorShrwt_Fert" ) ){
	write_mi_data( L2322.SubsectorShrwt_Fert, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L2322.SubsectorShrwt_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
	}
if( exists( "L2322.SubsectorShrwtFllt_Fert" ) ){
	write_mi_data( L2322.SubsectorShrwtFllt_Fert, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L2322.SubsectorShrwtFllt_Fert",
	               "ENERGY_XML_BATCH", "batch_en_Fert.xml" ) 
	}
if( exists( "L2322.SubsectorInterp_Fert" ) ) {
	write_mi_data( L2322.SubsectorInterp_Fert, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L2322.SubsectorInterp_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
	}
if( exists( "L2322.SubsectorInterpTo_Fert" ) ) {
	write_mi_data( L2322.SubsectorInterpTo_Fert, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L2322.SubsectorInterpTo_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
	}
write_mi_data( L2322.StubTech_Fert, "StubTech", "ENERGY_LEVEL2_DATA", "L2322.StubTech_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.GlobalTechShrwt_Fert, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L2322.GlobalTechShrwt_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.GlobalTechCoef_Fert, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L2322.GlobalTechCoef_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.GlobalTechCost_Fert, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L2322.GlobalTechCost_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.GlobalTechCapture_Fert, "GlobalTechCapture", "ENERGY_LEVEL2_DATA", "L2322.GlobalTechCapture_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.StubTechProd_Fert, "StubTechProd", "ENERGY_LEVEL2_DATA", "L2322.StubTechProd_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.StubTechCoef_Fert, "StubTechCoef", "ENERGY_LEVEL2_DATA", "L2322.StubTechCoef_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.StubTechFixOut_Fert_imp, "StubTechFixOut", "ENERGY_LEVEL2_DATA", "L2322.StubTechFixOut_Fert_imp", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.StubTechFixOut_Fert_exp, "StubTechFixOut", "ENERGY_LEVEL2_DATA", "L2322.StubTechFixOut_Fert_exp", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.PerCapitaBased_Fert, "PerCapitaBased", "ENERGY_LEVEL2_DATA", "L2322.PerCapitaBased_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )
write_mi_data( L2322.BaseService_Fert, "BaseService", "ENERGY_LEVEL2_DATA", "L2322.BaseService_Fert", "ENERGY_XML_BATCH", "batch_en_Fert.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_en_Fert.xml", "ENERGY_XML_FINAL", "en_Fert.xml", "", xml_tag="outFile" )

logstop()
