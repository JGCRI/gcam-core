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
logstart( "L2321.cement.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Model input for cement manufacturing and use" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
A321.sector <- readdata( "ENERGY_ASSUMPTIONS", "A321.sector" )
A_PrimaryFuelCCoef <- readdata( "EMISSIONS_ASSUMPTIONS", "A_PrimaryFuelCCoef" )
A321.sector <- readdata( "ENERGY_ASSUMPTIONS", "A321.sector" )
A321.subsector_interp <- readdata( "ENERGY_ASSUMPTIONS", "A321.subsector_interp" )
A321.subsector_logit <- readdata( "ENERGY_ASSUMPTIONS", "A321.subsector_logit" )
A321.subsector_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A321.subsector_shrwt" )
A321.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A321.globaltech_coef" )
A321.globaltech_cost <- readdata( "ENERGY_ASSUMPTIONS", "A321.globaltech_cost" )
A321.globaltech_shrwt <- readdata( "ENERGY_ASSUMPTIONS", "A321.globaltech_shrwt" )
A321.globaltech_co2capture <- readdata( "ENERGY_ASSUMPTIONS", "A321.globaltech_co2capture" )
A321.demand <- readdata( "ENERGY_ASSUMPTIONS", "A321.demand" )
L1321.out_Mt_R_cement_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1321.out_Mt_R_cement_Yh")
L1321.IO_GJkg_R_cement_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1321.IO_GJkg_R_cement_F_Yh" )
L1321.in_EJ_R_cement_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L1321.in_EJ_R_cement_F_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
# 2a. Supplysector information
printlog( "L2321.Supplysector_cement: Supply sector information for fertilizer sector" )
L2321.Supplysector_cement <- write_to_all_regions( A321.sector, names_Supplysector )

# 2b. Subsector information
printlog( "L2321.SubsectorLogit_cement: Subsector logit exponents of fertilizer sector" )
L2321.SubsectorLogit_cement <- write_to_all_regions( A321.subsector_logit, names_SubsectorLogit )

printlog( "L2321.SubsectorShrwt_cement and L2321.SubsectorShrwtFllt_cement: Subsector shareweights of fertilizer sector" )
if( any( !is.na( A321.subsector_shrwt$year ) ) ){
	L2321.SubsectorShrwt_cement <- write_to_all_regions( A321.subsector_shrwt[ !is.na( A321.subsector_shrwt$year ), ], names_SubsectorShrwt )
	}
if( any( !is.na( A321.subsector_shrwt$year.fillout ) ) ){
	L2321.SubsectorShrwtFllt_cement <- write_to_all_regions( A321.subsector_shrwt[ !is.na( A321.subsector_shrwt$year.fillout ), ], names_SubsectorShrwtFllt )
	}

printlog( "L2321.SubsectorInterp_cement and L2321.SubsectorInterpTo_cement: Subsector shareweight interpolation of fertilizer sector" )
if( any( is.na( A321.subsector_interp$to.value ) ) ){
	L2321.SubsectorInterp_cement <- write_to_all_regions( A321.subsector_interp[ is.na( A321.subsector_interp$to.value ), ], names_SubsectorInterp )
	}
if( any( !is.na( A321.subsector_interp$to.value ) ) ){
	L2321.SubsectorInterpTo_cement <- write_to_all_regions( A321.subsector_interp[ !is.na( A321.subsector_interp$to.value ), ], names_SubsectorInterpTo )
	}

# 2c. Technology information
printlog( "L2321.StubTech_cement: Identification of stub technologies of cement" )
#Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
L2321.StubTech_cement <- write_to_all_regions( A321.globaltech_shrwt, names_Tech )
names( L2321.StubTech_cement ) <- names_StubTech

printlog( "L2321.GlobalTechShrwt_cement: Shareweights of global cement technologies" )
L2321.globaltech_shrwt.melt <- interpolate_and_melt( A321.globaltech_shrwt, c( model_base_years, model_future_years ), value.name="share.weight" )
L2321.globaltech_shrwt.melt[ c( "sector.name", "subsector.name" ) ] <- L2321.globaltech_shrwt.melt[ c( "supplysector", "subsector" ) ]
L2321.GlobalTechShrwt_cement <- L2321.globaltech_shrwt.melt[ c( names_GlobalTechYr, "share.weight" ) ]

printlog( "L2321.GlobalTechCoef_cement: Energy inputs and coefficients of cement technologies" )
L2321.globaltech_coef.melt <- interpolate_and_melt( A321.globaltech_coef, c( model_base_years, model_future_years ), value.name="coefficient" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L2321.globaltech_coef.melt[ c( "sector.name", "subsector.name" ) ] <- L2321.globaltech_coef.melt[ c( "supplysector", "subsector" ) ]
L2321.GlobalTechCoef_cement <- L2321.globaltech_coef.melt[ names_GlobalTechCoef ]
L2321.GlobalTechCoef_cement$coefficient <- round( L2321.GlobalTechCoef_cement$coefficient, digits_coefficient )

#Carbon capture rates from technologies with CCS
printlog( "L2321.GlobalTechCapture_cement: CO2 capture fractions from global fertilizer production technologies with CCS" )
## No need to consider historical periods or intermittent technologies here
L2321.globaltech_co2capture.melt <- interpolate_and_melt( A321.globaltech_co2capture, model_future_years, value.name="remove.fraction" )
L2321.globaltech_co2capture.melt[ c( "sector.name", "subsector.name" ) ] <- L2321.globaltech_co2capture.melt[ c( "supplysector", "subsector" ) ]
L2321.GlobalTechCapture_cement <- L2321.globaltech_co2capture.melt[ names_GlobalTechYr ]
L2321.GlobalTechCapture_cement$remove.fraction <- round( L2321.globaltech_co2capture.melt$remove.fraction, digits = digits_remove.fraction )
L2321.GlobalTechCapture_cement$storage.market <- CO2.storage.market

printlog( "L2321.GlobalTechCost_cement: Non-energy costs of global cement manufacturing technologies" )
L2321.globaltech_cost.melt <- interpolate_and_melt( A321.globaltech_cost, c( model_base_years, model_future_years ), value.name="input.cost" )
#Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
L2321.globaltech_cost.melt[ c( "sector.name", "subsector.name" ) ] <- L2321.globaltech_cost.melt[ c( "supplysector", "subsector" ) ]
L2321.GlobalTechCost_cement <- L2321.globaltech_cost.melt[ names_GlobalTechCost ]

printlog( "Note: adjusting non-energy costs of technologies with CCS to include CO2 capture costs" )
##NOTE: The additional CCS-related non-energy costs are not included in the global technology assessment. Calculate here.
#First calculate the additional CCS costs per unit of carbon produced in 1975$
cement_CCS_cost_total_1975USDtC <- cement_CCS_cost_2000USDtCO2 * conv_2000_1975_USD * conv_C_CO2
CO2_storage_cost_1975USDtC <- CO2_storage_cost_1990USDtC * conv_1990_1975_USD
cement_CCS_cost_1975USDtC <- cement_CCS_cost_total_1975USDtC - CO2_storage_cost_1975USDtC

#Next calculate the quantity of CO2 produced per unit of cement produced (in kgC per kg cement)
cement_CO2_capture_frac <- mean( L2321.GlobalTechCapture_cement$remove.fraction )
CO2_IO_kgCkgcement <- mean( L2321.GlobalTechCoef_cement$coefficient[ L2321.GlobalTechCoef_cement$minicam.energy.input == "limestone" ] ) * 
      A_PrimaryFuelCCoef$PrimaryFuelCO2Coef[ A_PrimaryFuelCCoef$PrimaryFuelCO2Coef.name == "limestone" ]
CO2stored_IO_kgCkgcement <- CO2_IO_kgCkgcement * cement_CO2_capture_frac
cement_CCS_cost_75USD_tcement <- cement_CCS_cost_1975USDtC * CO2stored_IO_kgCkgcement / conv_t_kg

#Adjust the non-energy costs in the table for model input
L2321.GlobalTechCost_cement$input.cost[ L2321.GlobalTechCost_cement$technology %in% L2321.GlobalTechCapture_cement$technology ] <-
      L2321.GlobalTechCost_cement$input.cost[ L2321.GlobalTechCost_cement$technology %in% L2321.GlobalTechCapture_cement$technology ] +
      cement_CCS_cost_75USD_tcement
L2321.GlobalTechCost_cement$input.cost <- round( L2321.GlobalTechCost_cement$input.cost, digits_cost )

#Calibration and region-specific data
printlog( "L2321.StubTechProd_cement: calibrated cement production" )
L2321.StubTechProd_cement <- interpolate_and_melt( L1321.out_Mt_R_cement_Yh, model_base_years, value.name = "calOutputValue" )
L2321.StubTechProd_cement <- add_region_name( L2321.StubTechProd_cement )
L2321.StubTechProd_cement[ s_s_t ] <- calibrated_techs[
      match( paste( L2321.StubTechProd_cement$sector, "output" ),  #Only take the tech IDs where the calibration is identified as output
             paste( calibrated_techs$sector, calibrated_techs$calibration ) ), s_s_t ]
L2321.StubTechProd_cement$stub.technology <- L2321.StubTechProd_cement$technology
L2321.StubTechProd_cement$calOutputValue <- round( L2321.StubTechProd_cement$calOutputValue, digits_calOutput )
L2321.StubTechProd_cement$share.weight.year <- L2321.StubTechProd_cement[[Y]]
L2321.StubTechProd_cement$subs.share.weight <- ifelse( L2321.StubTechProd_cement$calOutputValue > 0, 1, 0 )
L2321.StubTechProd_cement$tech.share.weight <- L2321.StubTechProd_cement$subs.share.weight
L2321.StubTechProd_cement <- L2321.StubTechProd_cement[ names_StubTechProd ]

printlog( "L2321.StubTechCoef_cement: region-specific coefficients of cement production technologies" )
#Take this as a given in all years for which data is available
L2321.StubTechCoef_cement <- interpolate_and_melt( L1321.IO_GJkg_R_cement_F_Yh,
      historical_years[ historical_years %in% c( model_base_years, model_future_years ) ], value.name = "coefficient" )
L2321.StubTechCoef_cement <- add_region_name( L2321.StubTechCoef_cement )
L2321.StubTechCoef_cement[ s_s_t_i ] <- calibrated_techs[ match( vecpaste( L2321.StubTechCoef_cement[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ), s_s_t_i ]
L2321.StubTechCoef_cement$stub.technology <- L2321.StubTechCoef_cement$technology
L2321.StubTechCoef_cement$coefficient <- round( L2321.StubTechCoef_cement$coefficient, digits_coefficient )
L2321.StubTechCoef_cement$market.name <- L2321.StubTechCoef_cement$region
L2321.StubTechCoef_cement <- L2321.StubTechCoef_cement[ names_StubTechCoef ]

printlog( "L2321.StubTechCalInput_cement_heat: calibrated cement production" )
L2321.StubTechCalInput_cement_heat <- interpolate_and_melt( L1321.in_EJ_R_cement_F_Y, model_base_years, value.name = "calibrated.value" )
L2321.StubTechCalInput_cement_heat <- add_region_name( L2321.StubTechCalInput_cement_heat )
L2321.StubTechCalInput_cement_heat[ s_s_t_i ] <- calibrated_techs[
      match( vecpaste( L2321.StubTechCalInput_cement_heat[ S_F] ),
             vecpaste( calibrated_techs[ S_F ] ) ), s_s_t_i ]

#This table should only be the technologies for producing heat - drop the electricity inputs to the cement production technology
L2321.StubTechCalInput_cement_heat <- subset( L2321.StubTechCalInput_cement_heat, supplysector %!in% L2321.StubTechCoef_cement$supplysector )
L2321.StubTechCalInput_cement_heat$stub.technology <- L2321.StubTechCalInput_cement_heat$technology
L2321.StubTechCalInput_cement_heat$calibrated.value <- round( L2321.StubTechCalInput_cement_heat$calibrated.value, digits_calOutput )
L2321.StubTechCalInput_cement_heat$share.weight.year <- L2321.StubTechCalInput_cement_heat[[Y]]
L2321.StubTechCalInput_cement_heat$subs.share.weight <- ifelse( L2321.StubTechCalInput_cement_heat$calibrated.value > 0, 1, 0 )
L2321.StubTechCalInput_cement_heat$tech.share.weight <- L2321.StubTechCalInput_cement_heat$subs.share.weight
L2321.StubTechCalInput_cement_heat <- L2321.StubTechCalInput_cement_heat[ names_StubTechCalInput ]

printlog( "L2321.PerCapitaBased_cement: per-capita based flag for fertilizer exports final demand" )
L2321.PerCapitaBased_cement <- write_to_all_regions( A321.demand )[ names_PerCapitaBased ]

printlog( "L2321.BaseService_cement: base-year service output of cement" )
L2321.BaseService_cement <- data.frame(
      region = L2321.StubTechProd_cement$region,
      energy.final.demand = A321.demand$energy.final.demand,
      year = L2321.StubTechProd_cement$year,
      base.service = L2321.StubTechProd_cement$calOutputValue )

printlog( "L2321.PriceElasticity_cement: price elasticity" )
L2321.PriceElasticity_cement <- write_to_all_regions( A321.demand )
L2321.PriceElasticity_cement <- repeat_and_add_vector( L2321.PriceElasticity_cement, Y, model_future_years )[ names_PriceElasticity ]

#Income elasticities are read in the socioeconomics module

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2321.Supplysector_cement, IDstring="Supplysector", domain="ENERGY_LEVEL2_DATA", fn="L2321.Supplysector_cement",
               batch_XML_domain="ENERGY_XML_BATCH", batch_XML_file="batch_cement.xml" ) 
write_mi_data( L2321.SubsectorLogit_cement, "SubsectorLogit", "ENERGY_LEVEL2_DATA", "L2321.SubsectorLogit_cement", "ENERGY_XML_BATCH", "batch_cement.xml" ) 
if( exists( "L2321.SubsectorShrwt_cement" ) ){
	write_mi_data( L2321.SubsectorShrwt_cement, "SubsectorShrwt", "ENERGY_LEVEL2_DATA", "L2321.SubsectorShrwt_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
	}
if( exists( "L2321.SubsectorShrwtFllt_cement" ) ){
	write_mi_data( L2321.SubsectorShrwtFllt_cement, "SubsectorShrwtFllt", "ENERGY_LEVEL2_DATA", "L2321.SubsectorShrwtFllt_cement",
	               "ENERGY_XML_BATCH", "batch_cement.xml" ) 
	}
if( exists( "L2321.SubsectorInterp_cement" ) ) {
	write_mi_data( L2321.SubsectorInterp_cement, "SubsectorInterp", "ENERGY_LEVEL2_DATA", "L2321.SubsectorInterp_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
	}
if( exists( "L2321.SubsectorInterpTo_cement" ) ) {
	write_mi_data( L2321.SubsectorInterpTo_cement, "SubsectorInterpTo", "ENERGY_LEVEL2_DATA", "L2321.SubsectorInterpTo_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
	}
write_mi_data( L2321.StubTech_cement, "StubTech", "ENERGY_LEVEL2_DATA", "L2321.StubTech_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.GlobalTechShrwt_cement, "GlobalTechShrwt", "ENERGY_LEVEL2_DATA", "L2321.GlobalTechShrwt_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.GlobalTechCoef_cement, "GlobalTechCoef", "ENERGY_LEVEL2_DATA", "L2321.GlobalTechCoef_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.GlobalTechCost_cement, "GlobalTechCost", "ENERGY_LEVEL2_DATA", "L2321.GlobalTechCost_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.GlobalTechCapture_cement, "GlobalTechCapture", "ENERGY_LEVEL2_DATA", "L2321.GlobalTechCapture_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.StubTechProd_cement, "StubTechProd", "ENERGY_LEVEL2_DATA", "L2321.StubTechProd_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.StubTechCalInput_cement_heat, "StubTechCalInput", "ENERGY_LEVEL2_DATA", "L2321.StubTechCalInput_cement_heat", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.StubTechCoef_cement, "StubTechCoef", "ENERGY_LEVEL2_DATA", "L2321.StubTechCoef_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.PerCapitaBased_cement, "PerCapitaBased", "ENERGY_LEVEL2_DATA", "L2321.PerCapitaBased_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.BaseService_cement, "BaseService", "ENERGY_LEVEL2_DATA", "L2321.BaseService_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )
write_mi_data( L2321.PriceElasticity_cement, "PriceElasticity", "ENERGY_LEVEL2_DATA", "L2321.PriceElasticity_cement", "ENERGY_XML_BATCH", "batch_cement.xml" )

insert_file_into_batchxml( "ENERGY_XML_BATCH", "batch_cement.xml", "ENERGY_XML_FINAL", "cement.xml", "", xml_tag="outFile" )

logstop()
