
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
logstart( "L222.en_transformation.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Primary energy transformation" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_ccs_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions")
A22.sector <- readdata( "ENERGY_ASSUMPTIONS", "A22.sector" )
A22.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A22.subsector_logit" )
A22.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A22.subsector_shrwt" )
A22.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A22.subsector_interp" )
A22.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_coef" )
A22.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_cost" )
A22.globaltech_cost_low <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_cost_low" ) # Note: "Low" indicates low tech. Costs are actually higher than core.
A22.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_shrwt" )
A22.globaltech_interp <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_interp" )
A22.globaltech_co2capture <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_co2capture" )
A22.globaltech_retirement <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_retirement" )
L122.out_EJ_R_gasproc_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.out_EJ_R_gasproc_F_Yh" )
L122.out_EJ_R_refining_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.out_EJ_R_refining_F_Yh" )
L122.IO_R_oilrefining_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.IO_R_oilrefining_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Supplysector information
printlog( "L222.Supplysector_en: Supply sector information for energy transformation sectors" )
L222.SectorLogitTables <- get_logit_fn_tables( A22.sector, names_SupplysectorLogitType,
    base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
L222.Supplysector_en <- write_to_all_regions( A22.sector, names_Supplysector )

# 2b. Subsector information
printlog( "L222.SubsectorLogit_en: Subsector logit exponents of energy transformation sectors" )
L222.SubsectorLogitTables <- get_logit_fn_tables( A22.subsector_logit, names_SubsectorLogitType,
    base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
L222.SubsectorLogit_en <- write_to_all_regions( A22.subsector_logit, names_SubsectorLogit )

printlog( "L222.SubsectorShrwt_en and L222.SubsectorShrwtFllt_en: Subsector shareweights of energy transformation sectors" )
if( any( !is.na( A22.subsector_shrwt$year ) ) ){
	L222.SubsectorShrwt_en <- write_to_all_regions( A22.subsector_shrwt[ !is.na( A22.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A22.subsector_shrwt$year.fillout ) ) ){
	L222.SubsectorShrwtFllt_en <- write_to_all_regions( A22.subsector_shrwt[ !is.na( A22.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

printlog( "L222.SubsectorInterp_en and L222.SubsectorInterpTo_en: Subsector shareweight interpolation of energy transformation sectors" )
if( any( is.na( A22.subsector_interp$to.value ) ) ){
	L222.SubsectorInterp_en <- write_to_all_regions( A22.subsector_interp[ is.na( A22.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
if( any( !is.na( A22.subsector_interp$to.value ) ) ){
	L222.SubsectorInterpTo_en <- write_to_all_regions( A22.subsector_interp[ !is.na( A22.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}

# 2c. Technology information
printlog( "L222.StubTech_en: Identification of stub technologies of energy transformation" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L222.StubTech_en <- write_to_all_regions( A22.globaltech_shrwt, names_Tech )
names( L222.StubTech_en ) <- names_StubTech

#Drop region x technology combinations that are not applicable
firstgenbio_techs <- c( "corn ethanol", "sugarbeet ethanol", "sugar cane ethanol", "biodiesel" )
L222.StubTech_en <- subset( L222.StubTech_en, stub.technology %!in% firstgenbio_techs | 
      paste( region, stub.technology ) %in%
      c( paste( A_regions$region, A_regions$ethanol ),
         paste( A_regions$region, A_regions$biodiesel ) ) )

printlog( "L222.GlobalTechInterp_en: Technology shareweight interpolation of energy transformation sectors" )
L222.GlobalTechInterp_en <- set_years( A22.globaltech_interp )
names( L222.GlobalTechInterp_en )[ names( L222.GlobalTechInterp_en ) %in% c( "supplysector", "subsector" ) ] <- c( "sector.name", "subsector.name" )

#Coefficients of global technologies
printlog( "L222.GlobalTechCoef_en: Energy inputs and coefficients of global technologies for energy transformation" )
L222.globaltech_coef.melt <- interpolate_and_melt( A22.globaltech_coef, c( model_base_years, model_future_years ), value.name="coefficient", digits = digits_coefficient )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L222.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L222.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L222.GlobalTechCoef_en <- L222.globaltech_coef.melt[ names_GlobalTechCoef ]

#Costs of global technologies
printlog( "L222.GlobalTechCost_en: Costs of global technologies for energy transformation" )
L222.globaltech_cost.melt <- interpolate_and_melt( A22.globaltech_cost, model_years, value.name="input.cost", digits = digits_cost )
L222.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L222.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L222.GlobalTechCost_en <- L222.globaltech_cost.melt[ names_GlobalTechCost ]

#Costs of global technologies -- low tech option
printlog( "L222.GlobalTechCost_low_en: Costs of global technologies for energy transformation" )
L222.globaltech_cost_low.melt <- interpolate_and_melt( A22.globaltech_cost_low, model_years, value.name="input.cost", digits = digits_cost )
L222.globaltech_cost_low.melt[ c( "sector.name", "subsector.name" ) ] <- L222.globaltech_cost_low.melt[ c( "supplysector", "subsector" ) ]
L222.GlobalTechCost_low_en <- L222.globaltech_cost_low.melt[ names_GlobalTechCost ]

#Shareweights of global technologies
printlog( "L222.GlobalTechShrwt_en: Shareweights of global technologies for energy transformation" )
L222.globaltech_shrwt.melt <- interpolate_and_melt( A22.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L222.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L222.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L222.GlobalTechShrwt_en <- L222.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

#CO2 capture rates of global technologies
printlog( "L222.GlobalTechCapture_en: CO2 capture fractions from global technologies for energy transformation" )
## No need to consider historical periods here
L222.globaltech_co2capture.melt <- interpolate_and_melt( A22.globaltech_co2capture, model_future_years, value.name="remove.fraction", digits = digits_remove.fraction )
L222.globaltech_co2capture.melt[ c( "sector.name", "subsector.name" ) ] <- L222.globaltech_co2capture.melt[ c( "supplysector", "subsector" ) ]
L222.GlobalTechCapture_en <- L222.globaltech_co2capture.melt[ c( names_GlobalTechYr, "remove.fraction" ) ]
L222.GlobalTechCapture_en$storage.market <- CO2.storage.market

#Retirement information
L222.globaltech_retirement <- set_years ( A22.globaltech_retirement )
L222.globaltech_retirement[ c( "sector.name", "subsector.name" ) ] <- L222.globaltech_retirement[ c( "supplysector", "subsector" ) ]
#Copy the data in the first future period through to the end year
L222.globaltech_retirement <- rbind(
      subset( L222.globaltech_retirement, year == max( model_base_years ) ),
      repeat_and_add_vector( subset( L222.globaltech_retirement, year == min(model_future_years ) ), "year", model_future_years ) )

#Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
# All of these options have different headers, and all are allowed
if( any( !is.na( L222.globaltech_retirement$shutdown.rate ) ) ){
	printlog( "L222.GlobalTechShutdown_en: Global tech lifetime and shutdown rate" )
	L222.GlobalTechShutdown_en <- L222.globaltech_retirement[
	     !is.na( L222.globaltech_retirement$shutdown.rate ),
	      c( names_GlobalTechYr, "lifetime", "shutdown.rate") ]
	}
if( any( !is.na( L222.globaltech_retirement$half.life ) ) ){
	printlog( "L222.GlobalTechSCurve_en: Global tech lifetime and s-curve retirement function" )
	L222.GlobalTechSCurve_en <- L222.globaltech_retirement[
	     !is.na( L222.globaltech_retirement$half.life ),
	      c( names_GlobalTechYr, "lifetime", "steepness", "half.life" ) ]
	}
if( any( is.na( L222.globaltech_retirement$shutdown.rate ) & is.na( L222.globaltech_retirement$half.life ) ) ){
	printlog( "L222.GlobalTechLifetime_en: Global tech lifetime" )
	L222.GlobalTechLifetime_en <- L222.globaltech_retirement[
	     is.na( L222.globaltech_retirement$shutdown.rate ) & is.na( L222.globaltech_retirement$half.life ),
	      c( names_GlobalTechYr, "lifetime" ) ]
	}
if( any( !is.na( L222.globaltech_retirement$median.shutdown.point ) ) ){
	printlog( "L222.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters" )
	L222.GlobalTechProfitShutdown_en <- L222.globaltech_retirement[
	     !is.na( L222.globaltech_retirement$median.shutdown.point ),
	      c( names_GlobalTechYr, "median.shutdown.point", "profit.shutdown.steepness") ]
	}

#2d. Calibration and region-specific data
#  Gas processing calibrated output by technology
L222.out_EJ_R_gasproc_F_Yh <- interpolate_and_melt( L122.out_EJ_R_gasproc_F_Yh, model_base_years )
L222.out_EJ_R_gasproc_F_Yh <- add_region_name( L222.out_EJ_R_gasproc_F_Yh )
L222.out_EJ_R_gasproc_F_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L222.out_EJ_R_gasproc_F_Yh$sector, L222.out_EJ_R_gasproc_F_Yh$fuel ),
             paste( calibrated_techs$sector, calibrated_techs$fuel ) ),
      c( "supplysector", "subsector", "technology" ) ]

printlog( "L222.StubTechProd_gasproc: calibrated output of gas processing technologies")
L222.StubTechProd_gasproc <- write_to_all_regions( subset( A22.globaltech_coef, supplysector == "gas processing" ), c( names_Tech, "minicam.energy.input" ) )
L222.StubTechProd_gasproc$stub.technology <- L222.StubTechProd_gasproc$technology
L222.StubTechProd_gasproc <- repeat_and_add_vector( L222.StubTechProd_gasproc, "year", model_base_years )
L222.StubTechProd_gasproc$calOutputValue <- round(
      L222.out_EJ_R_gasproc_F_Yh$value[
         match( paste( L222.StubTechProd_gasproc$region, L222.StubTechProd_gasproc$supplysector, L222.StubTechProd_gasproc$subsector,
                       L222.StubTechProd_gasproc$stub.technology, L222.StubTechProd_gasproc$year ),
                paste( L222.out_EJ_R_gasproc_F_Yh$region, L222.out_EJ_R_gasproc_F_Yh$supplysector, L222.out_EJ_R_gasproc_F_Yh$subsector,
                       L222.out_EJ_R_gasproc_F_Yh$stub.technology, L222.out_EJ_R_gasproc_F_Yh$year ) ) ],
      digits_calOutput )
#Coal to gas isn't included in all regions so replace missing values
L222.StubTechProd_gasproc$calOutputValue[ is.na( L222.StubTechProd_gasproc$calOutputValue ) ] <- 0
L222.StubTechProd_gasproc <- L222.StubTechProd_gasproc[ c( names_StubTechYr, "calOutputValue" ) ]
L222.StubTechProd_gasproc$year.share.weight <- L222.StubTechProd_gasproc$year
L222.StubTechProd_gasproc <- set_subsector_shrwt( L222.StubTechProd_gasproc )
L222.StubTechProd_gasproc$share.weight <- ifelse( L222.StubTechProd_gasproc$calOutputValue > 0, 1, 0 )

# Oil refining calibrated output by technology
L222.out_EJ_R_refining_F_Yh <- interpolate_and_melt( L122.out_EJ_R_refining_F_Yh, model_base_years )
L222.out_EJ_R_refining_F_Yh <- add_region_name( L222.out_EJ_R_refining_F_Yh )
L222.out_EJ_R_refining_F_Yh[ c( "supplysector", "subsector", "stub.technology" ) ] <- calibrated_techs[
      match( paste( L222.out_EJ_R_refining_F_Yh$sector, L222.out_EJ_R_refining_F_Yh$fuel ),
             paste( calibrated_techs$sector, calibrated_techs$fuel ) ),
      c( "supplysector", "subsector", "technology" ) ]

printlog( "L222.StubTechProd_refining: calibrated output of refining technologies")
L222.StubTechProd_refining <- L222.out_EJ_R_refining_F_Yh[ names_StubTechYr ]
L222.StubTechProd_refining$calOutputValue <- round( L222.out_EJ_R_refining_F_Yh$value, digits_calOutput )
L222.StubTechProd_refining$year.share.weight <- L222.StubTechProd_refining$year
L222.StubTechProd_refining <- set_subsector_shrwt( L222.StubTechProd_refining )
L222.StubTechProd_refining$share.weight <- ifelse( L222.StubTechProd_refining$calOutputValue > 0, 1, 0 )

# Oil refining input-output coefficients by region and input
L222.IO_R_oilrefining_F_Yh <- interpolate_and_melt( L122.IO_R_oilrefining_F_Yh, model_base_years )
L222.IO_R_oilrefining_F_Yh <- add_region_name( L222.IO_R_oilrefining_F_Yh )
L222.IO_R_oilrefining_F_Yh[ c( "supplysector", "subsector", "stub.technology", "minicam.energy.input" ) ] <- calibrated_techs[
      match( paste( L222.IO_R_oilrefining_F_Yh$sector, L222.IO_R_oilrefining_F_Yh$fuel ),
             paste( calibrated_techs$sector, calibrated_techs$fuel ) ),
      c( "supplysector", "subsector", "technology", "minicam.energy.input" ) ]

printlog( "L222.StubTechCoef_refining: calibrated input-output coefficients of oil refining")
L222.StubTechCoef_refining <- L222.IO_R_oilrefining_F_Yh[ c( names_StubTechYr, "minicam.energy.input" ) ]
L222.StubTechCoef_refining$coefficient <- round( L222.IO_R_oilrefining_F_Yh$value, digits_coefficient )
L222.StubTechCoef_refining$market.name <- L222.StubTechCoef_refining$region      

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L222.SectorLogitTables) ) {
write_mi_data( L222.SectorLogitTables[[ curr_table ]]$data, L222.SectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L222.", L222.SectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_en_transformation.xml" )
}
write_mi_data( L222.Supplysector_en, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L222.Supplysector_en",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_en_transformation.xml" ) 
for( curr_table in names ( L222.SubsectorLogitTables ) ) {
write_mi_data( L222.SubsectorLogitTables[[ curr_table ]]$data, L222.SubsectorLogitTables[[ curr_table ]]$header,
    "ENERGY_LEVEL2_DATA", paste0("L222.", L222.SubsectorLogitTables[[ curr_table ]]$header ), "ENERGY_XML_BATCH",
    "batch_en_transformation.xml" )
}
write_mi_data( L222.SubsectorLogit_en, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L222.SubsectorLogit_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" ) 
if( exists( "L222.SubsectorShrwt_en" ) ){
	write_mi_data( L222.SubsectorShrwt_en, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L222.SubsectorShrwt_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
	}
if( exists( "L222.SubsectorShrwtFllt_en" ) ){
	write_mi_data( L222.SubsectorShrwtFllt_en, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L222.SubsectorShrwtFllt_en",
	               "ENERGY_XML_BATCH", "batch_en_transformation.xml" ) 
	}
if( exists( "L222.SubsectorInterp_en" ) ) {
	write_mi_data( L222.SubsectorInterp_en, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L222.SubsectorInterp_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
	}
if( exists( "L222.SubsectorInterpTo_en" ) ) {
	write_mi_data( L222.SubsectorInterpTo_en, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L222.SubsectorInterpTo_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
	}
write_mi_data( L222.StubTech_en, "StubTech", "ENERGY_LEVEL2_DATA", "L222.StubTech_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
write_mi_data( L222.GlobalTechInterp_en, "GlobalTechInterp", "ENERGY_LEVEL2_DATA", "L222.GlobalTechInterp_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
write_mi_data( L222.GlobalTechCoef_en, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L222.GlobalTechCoef_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
write_mi_data( L222.GlobalTechCost_en, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L222.GlobalTechCost_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
write_mi_data( L222.GlobalTechShrwt_en, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L222.GlobalTechShrwt_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
write_mi_data( L222.GlobalTechCapture_en, "GlobalTechCapture", "ENERGY_LEVEL2_DATA", "L222.GlobalTechCapture_en", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
if( exists( "L222.GlobalTechShutdown_en" ) ) {
	write_mi_data( L222.GlobalTechShutdown_en, "GlobalTechShutdown", "ENERGY_LEVEL2_DATA", "L222.GlobalTechShutdown_en",
	               "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
	}
if( exists( "L222.GlobalTechSCurve_en" ) ) {
	write_mi_data( L222.GlobalTechSCurve_en, "GlobalTechSCurve", "ENERGY_LEVEL2_DATA", "L222.GlobalTechSCurve_en",
	               "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
	}
if( exists( "L222.GlobalTechLifetime_en" ) ) {
	write_mi_data( L222.GlobalTechLifetime_en, "GlobalTechLifetime", "ENERGY_LEVEL2_DATA", "L222.GlobalTechLifetime_en",
	               "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
	}
if( exists( "L222.GlobalTechProfitShutdown_en" ) ) {
	write_mi_data( L222.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown", "ENERGY_LEVEL2_DATA", "L222.GlobalTechProfitShutdown_en",
	               "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
	}
write_mi_data( L222.StubTechProd_gasproc, "StubTechProd", "ENERGY_LEVEL2_DATA", "L222.StubTechProd_gasproc", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
write_mi_data( L222.StubTechProd_refining, "StubTechProd", "ENERGY_LEVEL2_DATA", "L222.StubTechProd_refining", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )
write_mi_data( L222.StubTechCoef_refining, "StubTechCoef", "ENERGY_LEVEL2_DATA", "L222.StubTechCoef_refining", "ENERGY_XML_BATCH", "batch_en_transformation.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_en_transformation.xml", "ENERGY_XML_FINAL", "en_transformation.xml", "", xml_tag="outFile" )

write_mi_data( L222.GlobalTechCost_low_en, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L222.GlobalTechCost_low_en", "ENERGY_XML_BATCH", "batch_en_transformation_low.xml" )
insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_en_transformation_low.xml", "ENERGY_XML_FINAL", "en_transformation_low.xml", "", xml_tag="outFile" )

logstop()


