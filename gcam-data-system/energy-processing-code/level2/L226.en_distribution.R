
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
logstart( "L226.en_distribution.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Final energy distribution" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
fuel_energy_input <- readdata( "ENERGY_MAPPINGS", "fuel_energy_input" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
A26.sector <- readdata( "ENERGY_ASSUMPTIONS", "A26.sector" )
A26.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A26.subsector_logit" )
A26.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A26.subsector_shrwt" )
A26.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A26.subsector_interp" )
A26.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A26.globaltech_eff" )
A26.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A26.globaltech_cost" )
A26.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A26.globaltech_shrwt" )
L126.IO_R_elecownuse_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L126.IO_R_elecownuse_F_Yh" )
L126.IO_R_electd_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L126.IO_R_electd_F_Yh" )
L126.IO_R_gaspipe_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L126.IO_R_gaspipe_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
# 2a. Supplysector information
printlog( "L226.Supplysector_en: Supply sector information for energy distribution sectors" )
L226.Supplysector_en <- write_to_all_regions( A26.sector, names_Supplysector )

# 2b. Subsector information
printlog( "L226.SubsectorLogit_en: Subsector logit exponents of energy distribution sectors" )
L226.SubsectorLogit_en <- write_to_all_regions( A26.subsector_logit, names_SubsectorLogit )

printlog( "L226.SubsectorShrwt_en and L226.SubsectorShrwtFllt_en: Subsector shareweights of energy distribution sectors" )
if( any( !is.na( A26.subsector_shrwt$year ) ) ){
	L226.SubsectorShrwt_en <- write_to_all_regions( A26.subsector_shrwt[ !is.na( A26.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A26.subsector_shrwt$year.fillout ) ) ){
	L226.SubsectorShrwtFllt_en <- write_to_all_regions( A26.subsector_shrwt[ !is.na( A26.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

printlog( "L226.SubsectorInterp_en and L226.SubsectorInterpTo_en: Subsector shareweight interpolation of energy distribution sectors" )
if( any( is.na( A26.subsector_interp$to.value ) ) ){
	L226.SubsectorInterp_en <- write_to_all_regions( A26.subsector_interp[ is.na( A26.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
if( any( !is.na( A26.subsector_interp$to.value ) ) ){
	L226.SubsectorInterpTo_en <- write_to_all_regions( A26.subsector_interp[ !is.na( A26.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}

# 2c. Technology information
printlog( "L226.StubTech_en: Identification of stub technologies of energy distribution" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L226.StubTech_en <- write_to_all_regions( A26.globaltech_shrwt, names_Tech )
names( L226.StubTech_en ) <- names_StubTech

#Efficiencies of global technologies
printlog( "L226.GlobalTechEff_en: Energy inputs and efficiencies of global technologies for energy distribution" )
L226.globaltech_coef.melt <- interpolate_and_melt( A26.globaltech_eff, c( model_base_years, model_future_years ), value.name="efficiency" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L226.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L226.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L226.GlobalTechEff_en <- L226.globaltech_coef.melt[ names_GlobalTechEff ]
L226.GlobalTechEff_en$efficiency <- round( L226.GlobalTechEff_en$efficiency, digits_efficiency )

#Costs of global technologies
printlog( "L226.GlobalTechCost_en: Costs of global technologies for energy distribution" )
L226.globaltech_cost.melt <- interpolate_and_melt( A26.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost" )
L226.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L226.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L226.GlobalTechCost_en <- L226.globaltech_cost.melt[ names_GlobalTechCost ]
L226.GlobalTechCost_en$input.cost <- round( L226.GlobalTechCost_en$input.cost, digits_cost )

#Shareweights of global technologies
printlog( "L226.GlobalTechShrwt_en: Shareweights of global technologies for energy distribution" )
L226.globaltech_shrwt.melt <- interpolate_and_melt( A26.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L226.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L226.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L226.GlobalTechShrwt_en <- L226.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

#2d. Calibration and region-specific data
# Electricity ownuse IO coefs
L226.IO_R_elecownuse_F_Yh <- interpolate_and_melt( L126.IO_R_elecownuse_F_Yh, model_base_years )
L226.IO_R_elecownuse_F_Yh <- add_region_name( L226.IO_R_elecownuse_F_Yh )
L226.IO_R_elecownuse_F_Yh[ c( "supplysector", "subsector", "stub.technology", "minicam.energy.input" ) ] <- calibrated_techs[
      match( paste( L226.IO_R_elecownuse_F_Yh$sector, L226.IO_R_elecownuse_F_Yh$fuel ),
             paste( calibrated_techs$sector, calibrated_techs$fuel ) ),
      c( "supplysector", "subsector", "technology", "minicam.energy.input" ) ]
      
#Ownuse ratios are held constant in the future (could perhaps be tied to industrial CHP...but also AUTOELEC)
L226.IO_R_elecownuse_F_Yfut <- repeat_and_add_vector( subset( L226.IO_R_elecownuse_F_Yh, year == max( model_base_years ) ), "year", model_future_years )
L226.IO_R_elecownuse_F_Y <- rbind( L226.IO_R_elecownuse_F_Yh, L226.IO_R_elecownuse_F_Yfut )

printlog( "L226.StubTechCoef_elecownuse: calibrated coefficients on electricity net ownuse")
L226.StubTechCoef_elecownuse <- L226.IO_R_elecownuse_F_Y[ c( names_StubTechYr, "minicam.energy.input" ) ]
L226.StubTechCoef_elecownuse$coefficient <- round( L226.IO_R_elecownuse_F_Y$value, digits_coefficient )
L226.StubTechCoef_elecownuse$market.name <- L226.IO_R_elecownuse_F_Y$region

#Electricity transmission and distribution
L226.IO_R_electd_F_Yh <- interpolate_and_melt( L126.IO_R_electd_F_Yh, model_base_years )

#Copy to future periods and apply assumed techchange
L226.IO_R_electd_F_Yfut <- repeat_and_add_vector( subset( L226.IO_R_electd_F_Yh, year == max( model_base_years ) ), "year", model_future_years )
L226.IO_R_electd_F_Yfut$value <- L226.IO_R_electd_F_Yfut$value * ( 1 - A_regions$elect_td_techchange[
      match( L226.IO_R_electd_F_Yfut$GCAM_region_ID, A_regions$GCAM_region_ID ) ] ) ^ ( L226.IO_R_electd_F_Yfut$year - max( model_base_years ) )
L226.IO_R_electd_F_Y <- rbind( L226.IO_R_electd_F_Yh, L226.IO_R_electd_F_Yfut )

#Electricity T&D: Because this is written out to multiple sectors, need to start with the list in calibrated_techs
L226.calibrated_techs_electd <- subset( calibrated_techs, paste( sector, fuel ) %in% paste( L226.IO_R_electd_F_Y$sector, L226.IO_R_electd_F_Y$fuel ) )
L226.calibrated_techs_electd_repR <- repeat_and_add_vector( L226.calibrated_techs_electd, "GCAM_region_ID", unique( L226.IO_R_electd_F_Y$GCAM_region_ID ) )
L226.calibrated_techs_electd_repR <- add_region_name( L226.calibrated_techs_electd_repR )
L226.calibrated_techs_electd_repR$market.name <- L226.calibrated_techs_electd_repR$region
L226.calibrated_techs_electd_repR_repY <- repeat_and_add_vector( L226.calibrated_techs_electd_repR, "year", unique( L226.IO_R_electd_F_Y$year ) )
L226.calibrated_techs_electd_repR_repY$stub.technology <- L226.calibrated_techs_electd_repR_repY$technology
L226.calibrated_techs_electd_repR_repY$coefficient <- round(
    L226.IO_R_electd_F_Y$value[
      match( paste( L226.calibrated_techs_electd_repR_repY$sector, L226.calibrated_techs_electd_repR_repY$fuel,
                    L226.calibrated_techs_electd_repR_repY$GCAM_region_ID, L226.calibrated_techs_electd_repR_repY$year ),
             paste( L226.IO_R_electd_F_Y$sector, L226.IO_R_electd_F_Y$fuel,
                    L226.IO_R_electd_F_Y$GCAM_region_ID, L226.IO_R_electd_F_Y$year ) ) ],
    digits_coefficient )

printlog( "L226.StubTechCoef_electd: calibrated coefficients on electricity transmission and distribution")
L226.StubTechCoef_electd <- L226.calibrated_techs_electd_repR_repY[ names_StubTechCoef ]

# Gas pipeline IO coefs
L226.IO_R_gaspipe_F_Yh <- interpolate_and_melt( L126.IO_R_gaspipe_F_Yh, model_base_years )
L226.IO_R_gaspipe_F_Yh <- add_region_name( L226.IO_R_gaspipe_F_Yh )
L226.IO_R_gaspipe_F_Yh[ c( "supplysector", "subsector", "stub.technology", "minicam.energy.input" ) ] <- calibrated_techs[
      match( paste( L226.IO_R_gaspipe_F_Yh$sector, L226.IO_R_gaspipe_F_Yh$fuel ),
             paste( calibrated_techs$sector, calibrated_techs$fuel ) ),
      c( "supplysector", "subsector", "technology", "minicam.energy.input" ) ]

#Gas pipeline energy use ratios are held constant in the future
L226.IO_R_gaspipe_F_Yfut <- repeat_and_add_vector( subset( L226.IO_R_gaspipe_F_Yh, year == max( model_base_years ) ), "year", model_future_years )
L226.IO_R_gaspipe_F_Y <- rbind( L226.IO_R_gaspipe_F_Yh, L226.IO_R_gaspipe_F_Yfut )

printlog( "L226.StubTechCoef_gaspipe: calibrated coefficients on gas pipeline energy use")
L226.StubTechCoef_gaspipe <- L226.IO_R_gaspipe_F_Y[ c( names_StubTechYr, "minicam.energy.input" ) ]
L226.StubTechCoef_gaspipe$coefficient <- round( L226.IO_R_gaspipe_F_Y$value, digits_coefficient )
L226.StubTechCoef_gaspipe$market.name <- L226.IO_R_gaspipe_F_Y$region

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L226.Supplysector_en, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L226.Supplysector_en",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_en_distribution.xml" ) 
write_mi_data( L226.SubsectorLogit_en, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L226.SubsectorLogit_en", "ENERGY_XML_BATCH", "batch_en_distribution.xml" ) 
if( exists( "L226.SubsectorShrwt_en" ) ){
	write_mi_data( L226.SubsectorShrwt_en, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L226.SubsectorShrwt_en", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
	}
if( exists( "L226.SubsectorShrwtFllt_en" ) ){
	write_mi_data( L226.SubsectorShrwtFllt_en, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L226.SubsectorShrwtFllt_en",
	               "ENERGY_XML_BATCH", "batch_en_distribution.xml" ) 
	}
if( exists( "L226.SubsectorInterp_en" ) ) {
	write_mi_data( L226.SubsectorInterp_en, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L226.SubsectorInterp_en", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
	}
if( exists( "L226.SubsectorInterpTo_en" ) ) {
	write_mi_data( L226.SubsectorInterpTo_en, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L226.SubsectorInterpTo_en", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
	}
write_mi_data( L226.StubTech_en, "StubTech", "ENERGY_LEVEL2_DATA", "L226.StubTech_en", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
write_mi_data( L226.GlobalTechEff_en, "GlobalTechEff", "ENERGY_LEVEL2_DATA", "L226.GlobalTechEff_en", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
write_mi_data( L226.GlobalTechCost_en, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L226.GlobalTechCost_en", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
write_mi_data( L226.GlobalTechShrwt_en, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L226.GlobalTechShrwt_en", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
write_mi_data( L226.StubTechCoef_elecownuse, "StubTechCoef", "ENERGY_LEVEL2_DATA", "L226.StubTechCoef_elecownuse", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
write_mi_data( L226.StubTechCoef_electd, "StubTechCoef", "ENERGY_LEVEL2_DATA", "L226.StubTechCoef_electd", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )
write_mi_data( L226.StubTechCoef_gaspipe, "StubTechCoef", "ENERGY_LEVEL2_DATA", "L226.StubTechCoef_gaspipe", "ENERGY_XML_BATCH", "batch_en_distribution.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_en_distribution.xml", "ENERGY_XML_FINAL", "en_distribution.xml", "", xml_tag="outFile" )

logstop()


